read_study <- function(path) {
  s <- yaml::read_yaml(path)
  rows <- lapply(s$estimates, function(e) {
    e <- utils::modifyList(s$defaults, e)
    e$note <- paste(c(s$defaults$note, if (!identical(e$note, s$defaults$note)) e$note), collapse = " ")
    parts <- c("parts_reserved", "parts_open")
    row <- tibble::as_tibble(c(
      list(key = s$key, label = s$label, location = if (is.null(e$location)) s$location else e$location),
      s$setting, e[setdiff(names(e), c("note", "location", parts))], list(note = e$note)
    ))
    for (part in parts) row[[part]] <- list(e[[part]])
    row
  })
  dplyr::bind_rows(rows)
}

# Descriptive studies report shares without SEs; the computed SE treats the two seat types as
# independent samples and ignores any clustering, so it understates uncertainty for repeated villages.
proportion_se <- function(p1, n1, p2, n2) sqrt(p1 * (1 - p1) / n1 + p2 * (1 - p2) / n2)

read_literature <- function(dir = "evidence/literature/studies") {
  d <- dplyr::bind_rows(lapply(list.files(dir, pattern = "\\.yaml$", full.names = TRUE), read_study))
  for (col in c(
    "reserved", "open", "se", "t", "n_reserved", "n_open", "n_total",
    "sd_reserved", "sd_open", "se_mean_reserved", "se_mean_open"
  )) {
    if (!col %in% names(d)) d[[col]] <- NA_real_
  }
  computed <- d$se_source == "computed"
  d$se[computed] <- with(d[computed, ], proportion_se(reserved, n_reserved, open, n_open))
  from_t <- d$se_source == "from_t"
  d$se[from_t] <- abs(d$diff[from_t] / d$t[from_t])
  d |> dplyr::mutate(n = dplyr::coalesce(n_total, n_reserved + n_open))
}

# SD of a group reported only as subgroups: within-subgroup plus between-subgroup variation.
combined_sd <- function(parts) {
  m <- sum(parts$size * parts$mean) / sum(parts$size)
  sqrt((sum((parts$size - 1) * parts$sd^2) + sum(parts$size * (parts$mean - m)^2)) / (sum(parts$size) - 1))
}

# Cohen's d: reserved minus open over the pooled within-group SD, with the study's SE scaled the same
# way. Shares become d through the log odds ratio (metafor's OR2DL), and SD-unit outcomes pass through.
standardized_difference <- function(row) {
  if (row$unit == "sd") {
    return(c(yi = row$diff, vi = row$se^2))
  }
  if (row$unit == "share") {
    es <- metafor::escalc(
      measure = "OR2DL", ai = round(row$reserved * row$n_reserved), n1i = row$n_reserved,
      ci = round(row$open * row$n_open), n2i = row$n_open
    )
    return(c(yi = es$yi[1], vi = es$vi[1]))
  }
  n <- c(row$n_reserved, row$n_open)
  s <- if (!is.na(row$sd_reserved)) {
    c(row$sd_reserved, row$sd_open)
  } else if (!is.na(row$se_mean_reserved)) {
    c(row$se_mean_reserved, row$se_mean_open) * sqrt(n)
  } else {
    c(combined_sd(row$parts_reserved[[1]]), combined_sd(row$parts_open[[1]]))
  }
  pooled <- sqrt(sum((n - 1) * s^2) / (sum(n) - 2))
  c(yi = row$diff / pooled, vi = (row$se / pooled)^2)
}

# Meta-analysis ----

own_estimates <- function(report, bihar_tier = "gp_head", states = c("Bihar", "Uttar Pradesh", "Rajasthan")) {
  rural <- report$rural
  mumbai <- report$settings$mumbai$estimates
  delhi <- report$settings$delhi$estimates

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


run_meta <- function(report) {
  own <- own_estimates(report)
  settings <- bind_rows(
    fit_settings(own, "Main: northern village heads vs Kerala ward members and the two cities"),
    fit_settings(own_estimates(report, states = c("Bihar", "Uttar Pradesh")), "Without Rajasthan"),
    fit_settings(own_estimates(report, bihar_tier = "gp_ward"), "Bihar ward members in place of Bihar heads")
  )

  literature <- literature_inputs("cd")
  stopifnot(!anyDuplicated(literature$sample_group))
  pooled_literature <- bind_rows(
    fit_literature(literature, "Birbhum represented by Chattopadhyay and Duflo (2001)"),
    fit_literature(literature_inputs("beaman"), "Birbhum represented by Beaman et al. (2009)")
  )
  list(settings = settings, literature = pooled_literature)
}
