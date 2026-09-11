suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(ggplot2)
})
source("R/style.R")
bihar <- read_csv("output/bihar_2016/regressions.csv", show_col_types = FALSE) |>
  filter(primary) |>
  mutate(state = "Bihar", year = 2016)
rural <- bind_rows(bihar, bind_rows(lapply(c("uttar_pradesh", "rajasthan", "kerala"), function(state) {
  read_csv(file.path("output", state, "regressions.csv"), show_col_types = FALSE)
}))) |> mutate(state = recode(state, uttar_pradesh = "Uttar Pradesh", rajasthan = "Rajasthan", kerala = "Kerala"))
write_csv(rural, "output/rural_estimates.csv")

interval_plot <- function(d, title, limits = NULL) {
  ggplot(d, aes(x = estimate * 100, y = label)) +
    geom_vline(xintercept = 0, colour = "grey60", linewidth = 0.4) +
    geom_errorbar(aes(xmin = conf_low * 100, xmax = conf_high * 100),
      orientation = "y", width = 0.16, colour = "#165B85"
    ) +
    geom_point(colour = "#165B85", size = 2) +
    labs(x = "Reserved minus open seats (percentage points)", y = NULL, title = title) +
    scale_x_continuous(limits = limits) +
    theme_evidence()
}

heads <- rural |>
  filter(tier == "gp_head", outcome == "graduate_plus") |>
  mutate(label = factor(paste(state, year), levels = rev(paste(state, year))))
save_evidence(
  interval_plot(heads, "Rural village heads: graduate or above"),
  "output/rural_heads", 7, 3.4
)
kerala <- rural |>
  filter(state == "Kerala", outcome == "graduate_plus") |>
  mutate(
    label = factor(year, levels = rev(sort(unique(year)))),
    office = factor(office_labels[tier], levels = office_labels[c("gp_ward", "block_member", "zp_member")])
  )
save_evidence(
  interval_plot(kerala, "Kerala: graduate or above") + facet_wrap(~office, nrow = 1),
  "output/kerala/education", 9, 3.4
)
bihar_plot <- bihar |>
  filter(outcome != "age") |>
  mutate(
    label = factor(office_labels[tier], levels = rev(unname(office_labels))),
    outcome = recode(outcome, graduate_plus = "Graduate or above", illiterate = "Illiterate")
  )
save_evidence(
  interval_plot(bihar_plot, "Bihar 2016: elected officials") + facet_wrap(~outcome),
  "output/bihar_2016/education", 9, 4.5
)
mumbai <- read_csv("output/mumbai/regressions.csv", show_col_types = FALSE) |>
  filter(outcome %in% c("educ_grad_plus", "any_criminal")) |>
  rename(estimate = coef_quota) |>
  mutate(label = factor(
    recode(outcome,
      educ_grad_plus = "Graduate or above",
      any_criminal = "Any pending criminal case"
    ),
    levels = c("Any pending criminal case", "Graduate or above")
  ))
save_evidence(
  interval_plot(mumbai, "Urban: Mumbai councillors, 2012 and 2017"),
  "output/mumbai/quality", 7, 2.8
)
