suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(fixest)
})
source("R/delhi.R")
dir.create("output/delhi", showWarnings = FALSE, recursive = TRUE)
manifest <- read_csv("evidence/analysis_inputs.csv", show_col_types = FALSE)
path <- "../local_elections/data/delhi/release/winners.parquet"
pin <- manifest |> filter(.data$path == .env$path)
stopifnot(nrow(pin) == 1, digest::digest(path, algo = "sha256", file = TRUE) == pin$sha256)
d <- arrow::read_parquet(path) |>
  mutate(
    ward_id = paste(year, ward_number, sep = ":"), education_raw = education,
    age = if_else(age >= 21 & age <= 100, age, NA_integer_),
    any_criminal = as.integer(pending_cases > 0),
    corporation = case_when(
      year == 2012 ~ delhi_2012_roster()$corporation[match(as.integer(sub("-.*", "", ward_number)), 1:272)],
      year == 2017 ~ unname(c(N = "North", S = "South", E = "East")[sub(".*-", "", ward_number)]),
      TRUE ~ "Delhi"
    )
  ) |>
  bind_cols(delhi_education(arrow::read_parquet(path)$education))
stopifnot(!anyDuplicated(d$ward_id), !anyNA(d$assembly_id))
flows <- d |>
  group_by(year) |>
  summarise(
    winners = n(), quota_winners = sum(quota), classified_education = sum(!is.na(graduate_plus)),
    recorded_age = sum(!is.na(age)), recorded_cases = sum(!is.na(any_criminal)), .groups = "drop"
  )
arrow::write_parquet(d, "output/delhi/winners.parquet", compression = "zstd")
write_csv(flows, "output/delhi/sample_flow.csv")
write_csv(
  d |> select(year, ward_number, source_url, source_capture, source_sha256),
  "output/delhi/source_links.csv"
)
write_csv(delhi_2012_roster(), "output/delhi/official_2012_roster.csv")
write_csv(d |> count(year, education_raw, graduate_plus, illiterate), "output/delhi/education_labels.csv")
descriptive <- d |>
  group_by(year, quota) |>
  summarise(
    n = n(), education_n = sum(!is.na(graduate_plus)), graduate_share = mean(graduate_plus, na.rm = TRUE),
    illiterate_share = mean(illiterate, na.rm = TRUE), age_n = sum(!is.na(age)), mean_age = mean(age, na.rm = TRUE),
    cases_n = sum(!is.na(any_criminal)), any_case_share = mean(any_criminal, na.rm = TRUE), .groups = "drop"
  )
write_csv(descriptive, "output/delhi/descriptive.csv")
estimates <- list()
bounds <- list()
for (year in sort(unique(d$year))) {
  yearly <- d |> filter(.data$year == .env$year)
  for (outcome in c("graduate_plus", "illiterate", "age", "any_criminal")) {
    sample <- yearly |> filter(!is.na(.data[[outcome]]))
    model <- feols(as.formula(paste(outcome, "~ quota | assembly_id + caste_reservation")),
      data = sample, cluster = ~assembly_id, fixef.rm = "none"
    )
    ci <- as.numeric(confint(model, "quota"))
    estimates[[length(estimates) + 1L]] <- tibble(
      year = year, outcome = outcome, estimate = unname(coef(model)["quota"]),
      se = unname(se(model)["quota"]), conf_low = ci[1], conf_high = ci[2],
      p = unname(pvalue(model)["quota"]), n = nobs(model), clusters = n_distinct(sample$assembly_id),
      geography = "assembly_id", caste_controls = TRUE
    )
    if (outcome %in% c("graduate_plus", "any_criminal")) {
      bounds[[length(bounds) + 1L]] <- delhi_bounds(yearly, outcome) |> mutate(year = year, .before = 1)
    }
  }
}
write_csv(bind_rows(estimates), "output/delhi/regressions.csv")
write_csv(bind_rows(bounds), "output/delhi/missing_outcome_bounds.csv")
print(bind_rows(estimates))

legacy <- bind_rows(lapply(c(2012, 2017), function(year) {
  raw <- read_csv(paste0("../local_elections/data/delhi/delhi_", year, "_final.csv"),
    col_types = cols(.default = col_character()), name_repair = "minimal"
  )
  prepare_delhi(raw, year)
}))
comparison <- legacy |>
  select(year, ward_number,
    legacy_education = education_raw,
    legacy_graduate = graduate_plus, legacy_cases = pending_cases
  ) |>
  left_join(d |> select(year, ward_number, education_raw, graduate_plus, pending_cases, source_url),
    by = c("year", "ward_number"), relationship = "one-to-one"
  ) |>
  mutate(graduate_disagreement = legacy_graduate != graduate_plus)
write_csv(comparison, "output/delhi/source_comparison.csv")
