read_study <- function(path) {
  s <- yaml::read_yaml(path)
  rows <- lapply(s$estimates, function(e) {
    e <- utils::modifyList(s$defaults, e)
    e$note <- paste(c(s$defaults$note, if (!identical(e$note, s$defaults$note)) e$note), collapse = " ")
    as.data.frame(c(
      list(key = s$key, label = s$label, location = if (is.null(e$location)) s$location else e$location),
      s$setting, e[setdiff(names(e), c("note", "location"))], list(note = e$note)
    ), stringsAsFactors = FALSE)
  })
  dplyr::bind_rows(rows)
}

# Descriptive studies report shares without SEs; the computed SE treats the two seat types as
# independent samples and ignores any clustering, so it understates uncertainty for repeated villages.
proportion_se <- function(p1, n1, p2, n2) sqrt(p1 * (1 - p1) / n1 + p2 * (1 - p2) / n2)

read_literature <- function(dir = "lit/studies") {
  d <- dplyr::bind_rows(lapply(list.files(dir, pattern = "\\.yaml$", full.names = TRUE), read_study))
  for (col in c("reserved", "open", "se", "n_reserved", "n_open", "n_total")) {
    if (!col %in% names(d)) d[[col]] <- NA_real_
  }
  computed <- d$se_source == "computed"
  d$se[computed] <- with(d[computed, ], proportion_se(reserved, n_reserved, open, n_open))
  d |> dplyr::mutate(n = dplyr::coalesce(n_total, n_reserved + n_open))
}
