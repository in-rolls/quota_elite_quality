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
# Rajasthan is left out of the head range because missing education leaves its all-winner sign open.
setting_summary <- function() {
  heads <- rural |> filter(tier == "gp_head", outcome == "graduate_plus", state %in% c("Bihar", "Uttar Pradesh"))
  kerala <- rural |> filter(state == "Kerala", tier == "gp_ward", outcome == "graduate_plus")
  cases <- c(mumbai$coef_quota[mumbai$outcome == "any_criminal"], delhi$estimate[delhi$outcome == "any_criminal"])
  list(
    head_gap = 100 * range(-heads$estimate),
    kerala_bound = 100 * max(abs(c(kerala$conf_low, kerala$conf_high))),
    case_gap = 100 * range(-cases)
  )
}
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

lit_meta <- read_csv("output/meta/literature.csv", show_col_types = FALSE)[1, ]
setting_meta <- read_csv("output/meta/settings.csv", show_col_types = FALSE)
audit <- read_csv("output/audit/deposit_agreement.csv", show_col_types = FALSE)

# Reserved-minus-open differences in reporting no occupation and in log declared assets.
economic_summary <- function() {
  occupation <- c(
    rural$estimate[rural$outcome == "no_earnings" & rural$tier %in% c("gp_head", "gp_ward", "block_member")],
    delhi$estimate[delhi$outcome == "no_earnings"]
  )
  up_crime <- get_result("Uttar Pradesh", 2021, "gp_head", "criminal_record")
  up_desc <- read_csv("output/uttar_pradesh/descriptive.csv", show_col_types = FALSE) |>
    filter(year == 2021, tier == "gp_head")
  list(
    occupation_gap = 100 * range(occupation),
    rajasthan_occupation = get_result("Rajasthan", 2020, "gp_head", "no_earnings"),
    kerala_ward_occupation = 100 * range(
      rural$estimate[rural$state == "Kerala" & rural$tier == "gp_ward" & rural$outcome == "no_earnings"]
    ),
    delhi_occupation = 100 * range(delhi$estimate[delhi$outcome == "no_earnings"]),
    rajasthan_assets = 100 * (exp(get_result("Rajasthan", 2020, "gp_head", "log_assets")$estimate) - 1),
    up_assets = 100 * (exp(get_result("Uttar Pradesh", 2021, "gp_head", "log_assets")$estimate) - 1),
    up_crime = up_crime,
    social_shift = 100 * max(abs(c(
      rural$estimate[rural$outcome == "no_earnings_social_missing"] -
        rural$estimate[rural$outcome == "no_earnings"],
      delhi$estimate[delhi$outcome == "no_earnings_social_missing"] - delhi$estimate[delhi$outcome == "no_earnings"]
    ))),
    kerala_district_occupation = 100 * range(
      rural$estimate[rural$state == "Kerala" & rural$tier == "zp_member" & rural$outcome == "no_earnings"]
    ),
    up_crime_rate = 100 * weighted.mean(up_desc$criminal_record, up_desc$criminal_n)
  )
}

economic_table <- function() {
  outcomes <- c(
    no_earnings = "No occupation", no_pan = "No tax ID declared", log_assets = "Log declared assets",
    criminal_record = "Criminal record"
  )
  rows <- bind_rows(
    mumbai |>
      filter(outcome == "no_pan") |>
      mutate(state = "Mumbai", year = "2012, 2017", Office = "Councillor", estimate = coef_quota),
    rural |>
      filter(outcome %in% names(outcomes)) |>
      mutate(Office = unname(office_labels[tier]), year = as.character(year)),
    delhi |>
      filter(outcome %in% names(outcomes)) |>
      mutate(state = "Delhi", Office = "Councillor", year = as.character(year))
  ) |>
    mutate(
      Outcome = unname(outcomes[outcome]), scale = if_else(outcome == "log_assets", 1, 100),
      `Difference [95% CI]` = interval_text(estimate, conf_low, conf_high, scale),
      N = num(n)
    ) |>
    arrange(match(outcome, names(outcomes)), state, year) |>
    select(Outcome, State = state, Year = year, Office, `Difference [95% CI]`, N)
  kable(rows, caption = paste(
    "Occupation, assets and criminal records: reserved minus open seats, with the controls and clustering",
    "of the education models. No occupation and criminal record in percentage points; log declared assets",
    "in log points. No occupation counts nil, unemployed, homemaker and student entries. Mumbai, which records",
    "no occupation, reports whether the councillor declared a tax ID (PAN)."
  ), booktabs = TRUE, row.names = FALSE, longtable = TRUE)
}
