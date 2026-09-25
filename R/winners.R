suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
  library(readr)
  library(tidyr)
  library(stringr)
  library(fixest)
})

master_dir <- function() Sys.getenv("LOCAL_ELECTIONS_MASTER", "../local_elections/data/master")

read_seats <- function(state) {
  path <- file.path(master_dir(), paste0("master_", state, ".parquet"))
  d <- read_parquet(path) |> as_tibble()
  stopifnot(!anyDuplicated(d$row_id))
  d
}

winner_extras <- function(seats) {
  d <- read_parquet(file.path(master_dir(), "master_extras.parquet")) |>
    filter(row_id %in% seats$row_id, column %in% c("winner_education", "winner_age", "winner_occupation"))
  stopifnot(!anyDuplicated(paste(d$row_id, d$column)))
  d |> pivot_wider(names_from = column, values_from = value)
}

schooling <- function(x, state) {
  label <- str_squish(str_to_lower(x))
  graduate <- rep(NA_integer_, length(label))
  rule <- rep("unclassified", length(label))
  if (state == "uttar_pradesh") {
    high <- c("graduate", "postgraduate", "स्नातक", "परास्नातक", "पी० एच० डी०")
    low <- c(
      "illiterate", "literate", "primary", "junior high school", "high school", "intermediate",
      "निरक्षर", "प्राईमरी", "जूनियर हाईस्कूल", "हाईस्कूल", "इंटर"
    )
    graduate[label %in% high] <- 1L
    graduate[label %in% low] <- 0L
    illiterate <- ifelse(!is.na(graduate), as.integer(label %in% c("illiterate", "निरक्षर")), NA_integer_)
  } else if (state == "rajasthan") {
    high <- c("graduate", "postgraduate", "professional graduate", "professional post graduate")
    low <- c("5th", "8th", "secondary", "higher secondary", "literate", "illiterate")
    graduate[label %in% high] <- 1L
    graduate[label %in% low] <- 0L
    illiterate <- ifelse(!is.na(graduate), as.integer(label == "illiterate"), NA_integer_)
  } else {
    clean <- str_squish(str_replace_all(str_replace_all(label, "[,-]", " "), "[.]", ""))
    degrees <- paste0(
      "(^|[^a-z])(ba|b a|bcom|b com|bsc|b sc|btech|b tech|be|b e|bba|bca|bpharm|b pharm|bpt|",
      "bed|b ed|llb|l l b|llm|l l m|mbbs|bams|bhms|bums|ma|m a|msc|m sc|mcom|m com|mba|mtech|m tech|msw|",
      "mca|phd|graduate|graduation|degree|postgraduate)([^a-z]|$)"
    )
    school <- paste0(
      "(^|[^a-z])(sslc|s s l c|s s lc|s sl c|ss lc|ssc|hsc|vhse|v h s e|vhsc|puc|iti|itc|ttc|",
      "t t c|diploma|pdc|p d c|pree?.?degree|pree?.?digree|plus ?(one|two|2)|plustwo|",
      "high school|higher secondary|primary|literate|illiterate|seventh|eighth|ninth|nineth|",
      "tenth|fourth|fifth|sixth|vii|viii|ix|iv|vi|v|x)([^a-z]|$)|^[1-9] ?(st|nd|rd|th)?( std|",
      " standard| class| pass)?$|^1[012] ?(th)?( std| standard| class| pass)?$|^\\+2$"
    )
    low <- !is.na(clean) & str_detect(clean, school)
    high <- !is.na(clean) & str_detect(clean, degrees)
    # Pre-degree is school; incomplete degrees remain unclassified.
    high <- high & !str_detect(clean, "pree?.?degree|pree?.?digree")
    incomplete <- !is.na(clean) & str_detect(clean, paste0(
      "fail|incomplete|pursuing|studying|ongoing|under.?graduate|not complet|",
      "course complet|discont|[123] ?year|first year|second year|final year"
    ))
    postgrad_diploma <- !is.na(clean) & str_detect(clean, "p ?g ?diploma")
    graduate[low & !high & !postgrad_diploma] <- 0L
    graduate[high & !incomplete] <- 1L
    illiterate <- rep(NA_integer_, length(label))
  }
  rule[!is.na(graduate)] <- "recognized qualification"
  rule[is.na(label) | label %in% c("", "--", "other", "others", "unknown", "nil")] <- "missing or unspecified"
  tibble(education_raw = x, graduate_plus = graduate, illiterate = illiterate, education_rule = rule)
}

prepare_controls <- function(d, state) {
  d |> mutate(
    quota = as.integer(woman_reserved),
    age = suppressWarnings(as.numeric(winner_age)),
    age = if_else(age >= 21 & age <= 100, age, NA_real_),
    caste_reservation = na_if(as.character(caste_reservation), ""),
    district = na_if(str_squish(district), ""), block = na_if(str_squish(block), ""),
    block_id = if_else(!is.na(district) & !is.na(block), paste(district, block, sep = ":"), NA_character_),
    ambiguous = str_detect(
      coalesce(quality_flags, ""),
      "winner_candidate_ambiguous|winner_markers_conflict|serial_not_unique"
    ),
    treatment_known = !is.na(quota) & !str_detect(
      coalesce(quality_flags, ""), "gender_not_stated|reservation_control_disagree"
    ),
    winner_known = !is.na(winner) & nzchar(trimws(winner)),
    state_key = state
  )
}

fit_winners <- function(d, state) {
  out <- file.path("output", state)
  dir.create(out, recursive = TRUE, showWarnings = FALSE)
  coded <- schooling(d$winner_education, state)
  d <- bind_cols(d, coded)
  write_csv(
    count(d, year, tier, education_raw, graduate_plus, education_rule, name = "records"),
    file.path(out, "education_labels.csv")
  )
  flow <- d |>
    group_by(year, tier, quota) |>
    summarise(
      seats = n(), winners = sum(winner_known), ambiguous = sum(ambiguous),
      treatment_known = sum(treatment_known), classified_schooling = sum(!is.na(graduate_plus)),
      .groups = "drop"
    )
  write_csv(flow, file.path(out, "sample_flow.csv"))
  analysis <- d |>
    filter(winner_known, !ambiguous, treatment_known, !is.na(caste_reservation)) |>
    select(
      row_id, year, tier, state_key, quota, district, block_id, caste_reservation,
      graduate_plus, illiterate, age, education_rule
    )
  stopifnot(!anyDuplicated(analysis$row_id), all(na.omit(analysis$graduate_plus) %in% 0:1))
  write_parquet(analysis, file.path(out, "winners.parquet"), compression = "zstd")
  desc <- analysis |>
    group_by(year, tier, quota) |>
    summarise(
      n = n(), education_n = sum(!is.na(graduate_plus)),
      graduate_plus = mean(graduate_plus, na.rm = TRUE),
      illiterate = mean(illiterate, na.rm = TRUE), age_n = sum(!is.na(age)),
      age = mean(age, na.rm = TRUE), .groups = "drop"
    )
  write_csv(desc, file.path(out, "descriptive.csv"))
  results <- list()
  for (yr in sort(unique(analysis$year))) {
    for (office in unique(analysis$tier[analysis$year == yr])) {
      base <- filter(analysis, year == yr, tier == office)
      geography <- if (office != "zp_member" && all(!is.na(base$block_id))) "block_id" else "district"
      for (outcome in c("graduate_plus", "illiterate", "age")) {
        sample <- base |> filter(!is.na(.data[[outcome]]), !is.na(.data[[geography]]))
        if (nrow(sample) == 0 || n_distinct(sample[[outcome]]) < 2) next
        stopifnot(n_distinct(sample$quota) == 2, n_distinct(sample[[geography]]) > 1)
        m <- feols(as.formula(paste(outcome, "~ quota |", geography, "+ caste_reservation")),
          data = sample, vcov = as.formula(paste("~", geography)), fixef.rm = "none", notes = FALSE
        )
        ci <- as.numeric(confint(m, "quota"))
        results[[length(results) + 1L]] <- tibble(
          state = state, year = yr, tier = office, geography = geography, outcome = outcome,
          estimate = unname(coef(m)["quota"]), se = unname(se(m)["quota"]),
          conf_low = ci[1], conf_high = ci[2], p = unname(pvalue(m)["quota"]),
          n = nobs(m), clusters = n_distinct(sample[[geography]])
        )
      }
    }
  }
  results <- bind_rows(results)
  write_csv(results, file.path(out, "regressions.csv"))
  print(results, n = Inf)
  invisible(results)
}
