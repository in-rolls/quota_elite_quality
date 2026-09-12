suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(knitr)
})
source("R/style.R")
rural <- read_csv("output/rural_estimates.csv", show_col_types = FALSE)
mumbai <- read_csv("output/mumbai/regressions.csv", show_col_types = FALSE)
delhi <- read_csv("output/delhi/regressions.csv", show_col_types = FALSE)
delhi_flow <- read_csv("output/delhi/sample_flow.csv", show_col_types = FALSE)
delhi_missing <- read_csv("output/delhi/missing_outcome_bounds.csv", show_col_types = FALSE)
get_delhi <- function(year, outcome = "graduate_plus") {
  row <- delhi |> filter(.data$year == .env$year, .data$outcome == .env$outcome)
  stopifnot(nrow(row) == 1)
  row
}
fmt <- function(x, digits = 1) formatC(x, format = "f", digits = digits)
num <- function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
get_result <- function(state, year, tier, outcome = "graduate_plus") {
  row <- rural |> filter(
    .data$state == .env$state, .data$year == .env$year,
    .data$tier == .env$tier, .data$outcome == .env$outcome
  )
  stopifnot(nrow(row) == 1)
  row
}
contrast <- function(row, scale = 100) {
  paste0(
    fmt(row$estimate * scale), " percentage points (95% CI ",
    fmt(row$conf_low * scale), " to ", fmt(row$conf_high * scale), ")"
  )
}
interval_text <- function(estimate, conf_low, conf_high, scale = 100) {
  paste0(fmt(estimate * scale), " [", fmt(conf_low * scale), ", ", fmt(conf_high * scale), "]")
}

education_table <- function(d, caption) {
  graduates <- d |>
    filter(outcome == "graduate_plus") |>
    arrange(year, match(tier, names(office_labels))) |>
    mutate(
      Office = unname(office_labels[tier]),
      `Graduate or above` = interval_text(estimate, conf_low, conf_high),
      N = num(n), G = num(clusters),
      Controls = if_else(geography == "block_id", "Block", "District")
    ) |>
    select(year, tier, Office, `Graduate or above`, N, G, Controls)
  illiteracy <- d |>
    filter(outcome == "illiterate") |>
    mutate(Illiterate = interval_text(estimate, conf_low, conf_high)) |>
    select(year, tier, Illiterate)
  table <- graduates |>
    left_join(illiteracy, by = c("year", "tier"), relationship = "one-to-one") |>
    mutate(Illiterate = coalesce(Illiterate, "--")) |>
    select(Year = year, Office, `Graduate or above`, Illiterate, N, G, Controls)
  kable(table, caption = caption, booktabs = TRUE, row.names = FALSE, longtable = TRUE)
}

age_table <- function(d) {
  table <- d |>
    filter(outcome == "age") |>
    arrange(match(state, unique(d$state)), year, match(tier, names(office_labels))) |>
    mutate(
      Office = unname(office_labels[tier]),
      `Difference [95% CI]` = interval_text(estimate, conf_low, conf_high, scale = 1),
      N = num(n), G = num(clusters)
    ) |>
    select(State = state, Year = year, Office, `Difference [95% CI]`, N, G)
  kable(table, caption = paste(
    "Age of rural officials: differences in years with pointwise 95% confidence intervals.",
    "N: winners with age recorded; G: geographic clusters. Controls match the education models."
  ), booktabs = TRUE, row.names = FALSE, longtable = TRUE)
}

delhi_comparison <- read_csv("output/delhi/source_comparison.csv", show_col_types = FALSE) |>
  group_by(year) |>
  summarise(
    joint = sum(!is.na(graduate_disagreement)),
    disagreements = sum(graduate_disagreement, na.rm = TRUE), .groups = "drop"
  )
