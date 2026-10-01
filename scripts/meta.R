# Two meta-analyses. (a) This paper's graduate-share differences, elections nested in settings, with a
# moderator for the northern village-head settings. (b) The published reserved-vs-open schooling gaps
# among elected leaders, one per independent sample, on Cohen's d.
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})
source("R/literature.R")
dir.create("output/meta", showWarnings = FALSE, recursive = TRUE)

rural <- read_csv("output/rural_estimates.csv", show_col_types = FALSE)
mumbai <- read_csv("output/mumbai/regressions.csv", show_col_types = FALSE)
delhi <- read_csv("output/delhi/regressions.csv", show_col_types = FALSE)

own_estimates <- function(bihar_tier = "gp_head", states = c("Bihar", "Uttar Pradesh", "Rajasthan")) {
  north <- rural |>
    filter(outcome == "graduate_plus", state %in% states) |>
    filter((state == "Bihar" & tier == bihar_tier) | (state != "Bihar" & tier == "gp_head"))
  kerala <- rural |> filter(outcome == "graduate_plus", state == "Kerala", tier == "gp_ward")
  bind_rows(
    north |> transmute(setting = state, election = paste(state, year), estimate, se, northern = 1L),
    kerala |> transmute(setting = state, election = paste(state, year), estimate, se, northern = 0L),
    mumbai |>
      filter(outcome == "educ_grad_plus") |>
      transmute(setting = "Mumbai", election = "Mumbai 2012 and 2017", estimate = coef_quota, se, northern = 0L),
    delhi |>
      filter(outcome == "graduate_plus") |>
      transmute(setting = "Delhi", election = paste("Delhi", year), estimate, se, northern = 0L)
  ) |>
    mutate(yi = 100 * estimate, vi = (100 * se)^2)
}

fit_settings <- function(d, label) {
  fit_pooled <- metafor::rma.mv(yi, vi, random = ~ 1 | setting / election, data = d, test = "t", dfs = "contain")
  fit_moderated <- metafor::rma.mv(yi, vi,
    mods = ~northern, random = ~ 1 | setting / election, data = d,
    test = "t", dfs = "contain"
  )
  northern_level <- predict(fit_moderated, newmods = 1)
  tibble(
    specification = label, elections = nrow(d), settings = n_distinct(d$setting),
    pooled = fit_pooled$b[1], pooled_low = fit_pooled$ci.lb[1], pooled_high = fit_pooled$ci.ub[1],
    other_settings = fit_moderated$b[1], other_low = fit_moderated$ci.lb[1], other_high = fit_moderated$ci.ub[1],
    northern = northern_level$pred, northern_low = northern_level$ci.lb, northern_high = northern_level$ci.ub,
    northern_gap = fit_moderated$b[2], northern_gap_low = fit_moderated$ci.lb[2],
    northern_gap_high = fit_moderated$ci.ub[2],
    tau_setting_pooled = sqrt(fit_pooled$sigma2[1]), tau_setting_moderated = sqrt(fit_moderated$sigma2[1]),
    tau_election = sqrt(fit_moderated$sigma2[2])
  )
}

own <- own_estimates()
write_csv(own, "output/meta/own_inputs.csv")
settings <- bind_rows(
  fit_settings(own, "Main: northern village heads vs Kerala ward members and the two cities"),
  fit_settings(own_estimates(states = c("Bihar", "Uttar Pradesh")), "Without Rajasthan"),
  fit_settings(own_estimates(bihar_tier = "gp_ward"), "Bihar ward members in place of Bihar heads")
)
write_csv(settings, "output/meta/settings.csv")

# One schooling estimate per independent sample; the Birbhum sample enters once.
literature_inputs <- function(birbhum = "cd") {
  d <- read_literature() |>
    filter(
      population == "winners", comparison == "reserved_vs_open", family %in% c("years", "secondary_plus"),
      key != setdiff(c("cd", "beaman"), birbhum)
    ) |>
    group_by(sample_group) |>
    slice(1) |>
    ungroup()
  es <- t(vapply(seq_len(nrow(d)), function(i) standardized_difference(d[i, ]), c(yi = 0, vi = 0)))
  d |>
    mutate(yi = es[, "yi"], vi = es[, "vi"]) |>
    select(key, label, sample_group, outcome, unit, diff, se, yi, vi)
}

fit_literature <- function(d, label) {
  fit <- metafor::rma(yi, vi, data = d, method = "REML", test = "knha")
  pred <- predict(fit)
  tibble(
    specification = label, samples = nrow(d), d = fit$b[1], d_low = fit$ci.lb, d_high = fit$ci.ub,
    prediction_low = pred$pi.lb, prediction_high = pred$pi.ub, tau = sqrt(fit$tau2), i2 = fit$I2
  )
}

literature <- literature_inputs("cd")
stopifnot(!anyDuplicated(literature$sample_group))
write_csv(literature, "output/meta/literature_inputs.csv")
pooled_literature <- bind_rows(
  fit_literature(literature, "Birbhum represented by Chattopadhyay and Duflo (2001)"),
  fit_literature(literature_inputs("beaman"), "Birbhum represented by Beaman et al. (2009)")
)
write_csv(pooled_literature, "output/meta/literature.csv")
print(settings, width = Inf)
print(pooled_literature, width = Inf)
