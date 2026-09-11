suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(knitr)
})
source("R/style.R")
rural <- read_csv("output/rural_estimates.csv", show_col_types = FALSE)
mumbai <- read_csv("output/mumbai/regressions.csv", show_col_types = FALSE)
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
result_table <- function(d, caption) {
  table <- d |>
    mutate(
      Office = recode(as.character(tier),
        gp_head = "Village head", gp_ward = "Ward member",
        kachahari_head = "Sarpanch", kachahari_member = "Panch",
        block_member = "Block member", zp_member = "District member"
      ),
      Outcome = recode(outcome, graduate_plus = "Graduate+", illiterate = "Illiterate", age = "Age"),
      scale = if_else(outcome == "age", 1, 100),
      Difference = fmt(estimate * scale),
      `95% CI` = paste0("[", fmt(conf_low * scale), ", ", fmt(conf_high * scale), "]"),
      N = num(n), Controls = if_else(geography == "block_id", "Block", "District")
    ) |>
    select(Year = year, Office, Outcome, Difference, `95% CI`, N, Controls)
  kable(table, caption = caption, booktabs = TRUE, row.names = FALSE)
}
