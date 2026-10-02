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
# way. Shares become d through the log odds ratio (metafor's OR2DL).
standardized_difference <- function(row) {
  stopifnot(row$unit %in% c("years", "share"))
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
      unit %in% c("years", "share"),
      key != setdiff(c("cd", "beaman"), birbhum)
    ) |>
    group_by(sample_group) |>
    slice(1) |>
    ungroup()
  es <- t(vapply(seq_len(nrow(d)), function(i) standardized_difference(d[i, ]), c(yi = 0, vi = 0)))
  d |>
    mutate(yi = es[, "yi"], vi = es[, "vi"]) |>
    select(key, label, state, years, sample_group, outcome, unit, diff, se, yi, vi)
}

bhavnani_contrasts <- function() {
  d <- read_literature() |> filter(key == "bhavnani")
  bind_rows(lapply(unique(d$sample), function(s) {
    x <- filter(d, sample == s)
    women <- filter(x, comparison == "non_scst_women_reserved_vs_open")
    sc_women <- filter(x, comparison == "scst_women_vs_open")
    sc_open <- filter(x, comparison == "scst_open_vs_open")
    stopifnot(nrow(women) == 1, nrow(sc_women) == 1, nrow(sc_open) == 1)
    tibble(
      sample = s, caste = c("Non-SC/ST", "SC/ST"),
      estimate = c(women$diff, sc_women$diff - sc_open$diff),
      se_low = c(women$se, abs(sc_women$se - sc_open$se)),
      se_high = c(women$se, sc_women$se + sc_open$se)
    ) |> mutate(low = estimate - qnorm(.975) * se_high, high = estimate + qnorm(.975) * se_high)
  }))
}

# Binary schooling thresholds use the same latent-logistic SD as OR2DL.
schooling_logit <- function(d, outcome, effects, cluster) {
  vars <- c(outcome, "quota", effects, cluster)
  d <- d[complete.cases(d[, unique(vars)]), ]
  model <- feglm(
    as.formula(paste(outcome, "~ quota |", paste(effects, collapse = " + "))),
    data = d, family = binomial(), vcov = as.formula(paste("~", cluster)), notes = FALSE
  )
  stopifnot(isTRUE(model$convStatus), "quota" %in% names(coef(model)))
  used <- fixest::obs(model)
  groups <- n_distinct(d[[cluster]][used])
  b <- unname(coef(model)["quota"])
  s <- unname(se(model)["quota"])
  scale <- sqrt(3) / pi
  stopifnot(is.finite(b), is.finite(s), s > 0, groups > 1)
  tibble(
    yi = b * scale, vi = (s * scale)^2,
    low = (b - qt(.975, groups - 1) * s) * scale,
    high = (b + qt(.975, groups - 1) * s) * scale,
    n_input = nrow(d), n = nobs(model), clusters = groups
  )
}

own_standardized <- function(report, bihar_tier = "gp_head") {
  rows <- list()
  specs <- report$rural |>
    filter(outcome == "graduate_plus") |>
    filter(
      (state == "Bihar" & tier == bihar_tier) |
        (state %in% c("Rajasthan", "Uttar Pradesh") & tier == "gp_head") |
        (state == "Kerala" & tier == "gp_ward")
    )
  for (i in seq_len(nrow(specs))) {
    spec <- specs[i, ]
    key <- if (spec$state == "Bihar") "bihar_2016" else tolower(gsub(" ", "_", spec$state))
    d <- report$settings[[key]]$analysis |> filter(tier == spec$tier)
    if (key != "bihar_2016") d <- d |> filter(year == spec$year)
    rows[[length(rows) + 1L]] <- schooling_logit(
      d, "graduate_plus", c(spec$geography, "caste_reservation"), spec$geography
    ) |>
      mutate(
        series = spec$state, id = paste(spec$state, spec$year), label = id,
        group = if_else(spec$state == "Kerala", "Our Kerala and cities", "Our northern village heads")
      )
  }
  rows[[length(rows) + 1L]] <- schooling_logit(
    report$settings$mumbai$analysis, "educ_grad_plus", c("adminward", "council"), "ward_no"
  ) |> mutate(series = "Mumbai", id = "Mumbai 2012 and 2017", label = id, group = "Our Kerala and cities")
  for (yr in sort(unique(report$settings$delhi$analysis$year))) {
    rows[[length(rows) + 1L]] <- schooling_logit(
      filter(report$settings$delhi$analysis, year == yr), "graduate_plus",
      c("assembly_id", "caste_reservation"), "assembly_id"
    ) |> mutate(series = "Delhi", id = paste("Delhi", yr), label = id, group = "Our Kerala and cities")
  }
  bind_rows(rows) |> mutate(source = "This paper", key = NA_character_)
}

combined_inputs <- function(own, birbhum = "cd") {
  lit <- literature_inputs(birbhum)
  lit <- lit |> transmute(
    series = sample_group, id = key, key,
    label = paste0(label, "\n", years), yi, vi,
    low = yi - qnorm(.975) * sqrt(vi), high = yi + qnorm(.975) * sqrt(vi),
    source = "Literature", group = "Published village heads"
  )
  d <- bind_rows(lit, own) |>
    mutate(group = factor(group, levels = c(
      "Published village heads", "Our northern village heads", "Our Kerala and cities"
    )))
  stopifnot(!anyDuplicated(d$id), all(is.finite(d$yi)), all(d$vi > 0))
  d
}

fit_combined <- function(d, label, rho = 0) {
  # Repeated elections share a setting effect. rho checks correlated sampling errors as well.
  v <- outer(sqrt(d$vi), sqrt(d$vi)) * outer(d$series, d$series, "==") * rho
  diag(v) <- d$vi
  model <- metafor::rma.mv(yi, v, random = ~ 1 | series / id, data = d, test = "t", dfs = "contain")
  prediction <- predict(model)
  tibble(
    specification = label, estimates = nrow(d), series = n_distinct(d$series),
    d = as.numeric(model$b), low = model$ci.lb, high = model$ci.ub,
    prediction_low = prediction$pi.lb, prediction_high = prediction$pi.ub,
    tau_series = sqrt(model$sigma2[1]), tau_estimate = sqrt(model$sigma2[2])
  )
}

run_combined <- function(report) {
  own <- own_standardized(report)
  d <- combined_inputs(own)
  main <- fit_combined(d, "Combined")
  sensitivity <- bind_rows(
    main,
    fit_combined(combined_inputs(own, birbhum = "beaman"), "Alternative Birbhum study"),
    fit_combined(filter(d, series != "Rajasthan"), "Without Rajasthan"),
    fit_combined(d, "Within-setting sampling correlation of 0.5", rho = .5)
  )
  groups <- metafor::rma.mv(yi, vi,
    mods = ~ group - 1, random = ~ 1 | series / id,
    data = d, test = "t", dfs = "contain"
  )
  group_summary <- tibble(group = levels(d$group), d = as.numeric(groups$b), low = groups$ci.lb, high = groups$ci.ub)
  influence <- bind_rows(lapply(unique(d$series), function(s) {
    fit_combined(filter(d, series != s), paste("Without", s))
  }))
  list(inputs = d, pooled = main, groups = group_summary, sensitivity = sensitivity, influence = influence)
}


run_meta <- function(report) {
  own <- own_estimates(report)
  settings <- bind_rows(
    fit_settings(own, "Main: northern village heads vs Kerala ward members and the two cities"),
    fit_settings(own_estimates(report, states = c("Bihar", "Uttar Pradesh")), "Without Rajasthan"),
    fit_settings(own_estimates(report, bihar_tier = "gp_ward"), "Bihar ward members in place of Bihar heads")
  )

  list(settings = settings, combined = run_combined(report))
}
