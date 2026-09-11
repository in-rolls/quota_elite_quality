source("R/winners.R")
results <- list()
for (state in c("uttar_pradesh", "rajasthan", "kerala")) {
  d <- read_parquet(file.path("output", state, "winners.parquet"))
  specs <- read_csv(file.path("output", state, "regressions.csv"), show_col_types = FALSE) |>
    filter(outcome == "graduate_plus")
  for (i in seq_len(nrow(specs))) {
    spec <- specs[i, ]
    sample <- d |>
      filter(year == spec$year, tier == spec$tier, !is.na(.data[[spec$geography]]))
    residual_model <- feols(as.formula(paste("quota ~ 1 |", spec$geography, "+ caste_reservation")),
      data = sample, fixef.rm = "none", notes = FALSE
    )
    weights <- resid(residual_model) / sum(resid(residual_model)^2)
    observed <- !is.na(sample$graduate_plus)
    known <- sum(weights[observed] * sample$graduate_plus[observed])
    results[[length(results) + 1L]] <- tibble(
      state = state, year = spec$year, tier = spec$tier,
      classified_sample_estimate = spec$estimate,
      all_winners_lower = known + sum(pmin(weights[!observed], 0)),
      all_winners_upper = known + sum(pmax(weights[!observed], 0)),
      all_winners_n = nrow(sample), unclassified = sum(!observed)
    )
  }
}
write_csv(bind_rows(results), "output/missing_education_bounds.csv")
