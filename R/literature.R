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

read_literature <- function(dir = "lit/studies") {
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
