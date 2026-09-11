suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
  library(readr)
  library(fixest)
  library(ggplot2)
})

education_outcomes <- function(x) {
  x <- tolower(trimws(x))
  known <- c(
    "illiterate", "literate", "primary", "middle", "higher", "inter",
    "graduate", "post graduate"
  )
  unexpected <- setdiff(unique(na.omit(x)), c(known, "--", "others", ""))
  if (length(unexpected)) stop("Unreviewed education labels: ", paste(unexpected, collapse = ", "))
  data.frame(
    education_known = x %in% known,
    illiterate = ifelse(x %in% known, as.integer(x == "illiterate"), NA_integer_),
    graduate_plus = ifelse(x %in% known, as.integer(x %in% c("graduate", "post graduate")), NA_integer_)
  )
}

select_winners <- function(candidates) {
  candidates %>%
    group_by(row_id) %>%
    mutate(
      contest_rows = n(),
      recorded_winners = sum(elected == "1"),
      selected = elected == "1" |
        (recorded_winners == 0 & contest_rows == 1 & result == "Uncontested"),
      selection_basis = if_else(elected == "1", "highest_recorded_votes", "sole_uncontested")
    ) %>%
    filter(selected, !is.na(candidate_name), nzchar(trimws(candidate_name))) %>%
    ungroup()
}

run_bihar <- function() {
  source_dir <- Sys.getenv("BIHAR_MASTER", "../local_elections/data/master")
  out_dir <- Sys.getenv("BIHAR_OUTPUT", "output/bihar_2016")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  paths <- file.path(source_dir, c("candidates_bihar.parquet", "master_bihar.parquet"))
  hashes <- vapply(paths, digest::digest, character(1), algo = "sha256", file = TRUE)
  candidates <- read_parquet(paths[1]) %>% as_tibble()
  seats <- read_parquet(paths[2]) %>% as_tibble()
  stopifnot(
    nrow(candidates) == 644507L, nrow(seats) == 219117L,
    !anyDuplicated(candidates$candidate_id), !anyDuplicated(seats$row_id),
    all(candidates$row_id %in% seats$row_id), all(candidates$year == 2016)
  )

  winners <- select_winners(candidates)
  stopifnot(!anyDuplicated(winners$row_id))
  winners <- winners %>%
    left_join(seats %>% select(row_id, quality_flags), by = "row_id", relationship = "many-to-one") %>%
    mutate(
      serial_ambiguous = grepl("serial_not_unique", coalesce(quality_flags, "")),
      quota = as.integer(woman_reserved),
      age = suppressWarnings(as.numeric(candidate_age)),
      age = if_else(age >= 21 & age <= 100, age, NA_real_),
      block_id = interaction(district, block, drop = TRUE, lex.order = TRUE),
      caste_reservation = factor(caste_reservation)
    )
  winners <- bind_cols(winners, education_outcomes(winners$candidate_education))
  flow <- seats %>%
    select(row_id, tier, woman_reserved) %>%
    left_join(winners %>% select(row_id, selection_basis, serial_ambiguous, education_known, age),
      by = "row_id", relationship = "one-to-one"
    ) %>%
    group_by(tier, woman_reserved) %>%
    summarise(
      source_seats = n(), identified_winners = sum(!is.na(selection_basis)),
      uncontested_winners = sum(selection_basis == "sole_uncontested", na.rm = TRUE),
      ambiguous_serial = sum(serial_ambiguous, na.rm = TRUE),
      education_unknown = sum(!education_known, na.rm = TRUE),
      age_missing = sum(!is.na(selection_basis) & is.na(age)), .groups = "drop"
    )
  write_csv(flow, file.path(out_dir, "sample_flow.csv"))
  write_csv(
    count(winners, candidate_education, education_known, sort = TRUE),
    file.path(out_dir, "education_labels.csv")
  )
  analysis <- winners %>%
    filter(!serial_ambiguous, !is.na(quota), !is.na(caste_reservation)) %>%
    select(
      row_id, tier, district, block, block_id, caste_reservation, quota,
      selection_basis, education_known, illiterate, graduate_plus, age
    )
  write_parquet(analysis, file.path(out_dir, "winners.parquet"), compression = "zstd")
  descriptive <- analysis %>%
    group_by(tier, quota) %>%
    summarise(
      n = n(), education_n = sum(education_known), age_n = sum(!is.na(age)),
      illiterate = mean(illiterate, na.rm = TRUE),
      graduate_plus = mean(graduate_plus, na.rm = TRUE),
      age = mean(age, na.rm = TRUE), .groups = "drop"
    )
  write_csv(descriptive, file.path(out_dir, "descriptive.csv"))

  results <- list()
  for (office in unique(as.character(analysis$tier))) {
    for (geography in c("district", if (office != "zp_member") "block_id")) {
      for (outcome in c("graduate_plus", "illiterate", "age")) {
        d <- analysis %>% filter(tier == office, !is.na(.data[[outcome]]), !is.na(.data[[geography]]))
        model <- feols(
          as.formula(paste(outcome, "~ quota |", geography, "+ caste_reservation")),
          data = d, vcov = as.formula(paste("~", geography)),
          fixef.rm = "none", notes = FALSE
        )
        interval <- as.numeric(unlist(confint(model, "quota")))
        results[[length(results) + 1L]] <- tibble(
          tier = office, geography = geography, outcome = outcome,
          estimate = unname(coef(model)["quota"]), se = unname(se(model)["quota"]),
          conf_low = interval[1], conf_high = interval[2], p = unname(pvalue(model)["quota"]),
          n = nobs(model), clusters = n_distinct(d[[geography]]),
          primary = geography == ifelse(office == "zp_member", "district", "block_id")
        )
      }
    }
  }
  results <- bind_rows(results)
  write_csv(results, file.path(out_dir, "regressions.csv"))
  labels <- c(
    gp_head = "Mukhiya", gp_ward = "Ward member", kachahari_head = "Sarpanch",
    kachahari_member = "Panch", block_member = "Panchayat samiti member",
    zp_member = "Zila parishad member"
  )
  plot_data <- results %>%
    filter(primary, outcome != "age") %>%
    mutate(
      office = factor(labels[tier], levels = rev(unname(labels))),
      outcome = factor(outcome,
        levels = c("graduate_plus", "illiterate"),
        labels = c("Graduate or above", "Illiterate")
      )
    )
  plot <- ggplot(plot_data, aes(x = estimate * 100, y = office)) +
    geom_vline(xintercept = 0, colour = "grey60", linewidth = 0.4) +
    geom_errorbar(aes(xmin = conf_low * 100, xmax = conf_high * 100),
      orientation = "y", width = 0.15, colour = "#165B85"
    ) +
    geom_point(colour = "#165B85", size = 2.3) +
    facet_wrap(~outcome, scales = "free_x") +
    labs(
      x = "Reserved minus open seats (percentage points)", y = NULL,
      title = "Bihar 2016: education of elected local officials",
      subtitle = "Block and caste-reservation controls; district controls for zila parishad",
      caption = "95% confidence intervals; SEs clustered by block (district for zila parishad).\nSource: Bihar SEC candidate records, via local_elections_bihar and local_elections."
    ) +
    theme_minimal(base_size = 12) +
    theme(
      panel.grid.minor = element_blank(), panel.grid.major.y = element_blank(),
      plot.caption = element_text(hjust = 0)
    )
  ggsave(file.path(out_dir, "education.png"), plot, width = 11, height = 5.1, dpi = 160)
  jsonlite::write_json(list(
    source_files = as.list(setNames(hashes, basename(paths))),
    candidate_records = nrow(candidates), seat_records = nrow(seats),
    identified_winners = nrow(winners), analysis_winners = nrow(analysis),
    treatment = "seat reserved for women", model = "OLS; geography and caste-reservation fixed effects",
    uncertainty = "geography-clustered, fixest default small-sample correction",
    packages = as.list(vapply(c("arrow", "dplyr", "fixest"), function(p) as.character(packageVersion(p)), character(1)))
  ), file.path(out_dir, "provenance.json"), pretty = TRUE, auto_unbox = TRUE)
  writeLines(trimws(capture.output(sessionInfo()), which = "right"),
             file.path(out_dir, "session.txt"))
  print(results %>% filter(primary), n = Inf)
  invisible(results)
}

if (sys.nframe() == 0L) run_bihar()
