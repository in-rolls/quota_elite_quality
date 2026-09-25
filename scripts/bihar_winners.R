suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
  library(readr)
  library(fixest)
  library(ggplot2)
})

source("R/bihar.R")
source("R/style.R")

run_bihar <- function() {
  source_dir <- Sys.getenv("BIHAR_MASTER", "../local_elections/data/master")
  out_dir <- Sys.getenv("BIHAR_OUTPUT", "output/bihar_2016")
  dir.create(out_dir, recursive = TRUE, showWarnings = FALSE)
  paths <- file.path(source_dir, c("candidates_bihar.parquet", "master_bihar.parquet"))
  hashes <- vapply(paths, digest::digest, character(1), algo = "sha256", file = TRUE)
  candidates <- read_parquet(paths[1]) |>
    as_tibble() |>
    filter(year == 2016)
  seats <- read_parquet(paths[2]) |>
    as_tibble() |>
    filter(year == 2016)
  stopifnot(
    nrow(candidates) == 644537L, nrow(seats) == 227317L,
    !anyDuplicated(candidates$candidate_id), !anyDuplicated(seats$row_id),
    all(candidates$row_id %in% seats$row_id), all(candidates$year == 2016)
  )

  winners <- select_winners(candidates)
  stopifnot(!anyDuplicated(winners$row_id))
  winners <- winners |>
    left_join(
      select(seats, row_id, selection_basis = winner_basis),
      by = "row_id", relationship = "one-to-one"
    ) |>
    mutate(
      quota = as.integer(woman_reserved),
      age = suppressWarnings(as.numeric(candidate_age)),
      age = if_else(age >= 21 & age <= 100, age, NA_real_),
      block_id = interaction(district, block, drop = TRUE, lex.order = TRUE),
      caste_reservation = factor(caste_reservation)
    )
  stopifnot(!anyNA(winners$selection_basis))
  winners <- bind_cols(winners, education_outcomes(winners$candidate_education))
  flow <- seats |>
    select(row_id, tier, woman_reserved) |>
    left_join(winners |> select(row_id, selection_basis, education_known, age),
      by = "row_id", relationship = "one-to-one"
    ) |>
    group_by(tier, woman_reserved) |>
    summarise(
      source_seats = n(), identified_winners = sum(!is.na(selection_basis)),
      uncontested_winners = sum(selection_basis == "uncontested", na.rm = TRUE),
      lot_winners = sum(selection_basis == "lot", na.rm = TRUE),
      education_unknown = sum(!education_known, na.rm = TRUE),
      age_missing = sum(!is.na(selection_basis) & is.na(age)), .groups = "drop"
    )
  write_csv(flow, file.path(out_dir, "sample_flow.csv"))
  write_csv(
    count(winners, candidate_education, education_known, sort = TRUE),
    file.path(out_dir, "education_labels.csv")
  )
  analysis <- winners |>
    filter(!is.na(quota), !is.na(caste_reservation)) |>
    select(
      row_id, tier, district, block, block_id, caste_reservation, quota,
      selection_basis, education_known, illiterate, graduate_plus, age
    )
  write_parquet(analysis, file.path(out_dir, "winners.parquet"), compression = "zstd")
  descriptive <- analysis |>
    group_by(tier, quota) |>
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
        d <- analysis |> filter(tier == office, !is.na(.data[[outcome]]), !is.na(.data[[geography]]))
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
  jsonlite::write_json(list(
    source_files = as.list(setNames(hashes, basename(paths))),
    candidate_records = nrow(candidates), seat_records = nrow(seats),
    identified_winners = nrow(winners), analysis_winners = nrow(analysis),
    treatment = "seat reserved for women", model = "OLS; geography and caste-reservation fixed effects",
    uncertainty = "geography-clustered, fixest default small-sample correction",
    packages = as.list(vapply(c("arrow", "dplyr", "fixest"), function(p) as.character(packageVersion(p)), character(1)))
  ), file.path(out_dir, "provenance.json"), pretty = TRUE, auto_unbox = TRUE)
  writeLines(
    trimws(capture.output(sessionInfo()), which = "right"),
    file.path(out_dir, "session.txt")
  )
  print(results |> filter(primary), n = Inf)
  invisible(results)
}

if (sys.nframe() == 0L) run_bihar()
