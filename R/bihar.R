education_outcomes <- function(x) {
  x <- tolower(trimws(x))
  known <- c(
    "illiterate", "literate", "primary", "middle", "higher", "inter",
    "graduate", "post graduate"
  )
  unexpected <- setdiff(unique(na.omit(x)), c(known, "--", "others", ""))
  if (length(unexpected)) stop("Unreviewed education labels: ", paste(unexpected, collapse = ", "))
  data.frame(
    education_known = x %in% known,
    illiterate = ifelse(x %in% known, as.integer(x == "illiterate"), NA_integer_),
    graduate_plus = ifelse(x %in% known, as.integer(x %in% c("graduate", "post graduate")), NA_integer_)
  )
}

select_winners <- function(candidates) {
  candidates |>
    group_by(row_id) |>
    mutate(
      contest_rows = n(),
      recorded_winners = sum(elected == "1"),
      selected = elected == "1" |
        (recorded_winners == 0 & contest_rows == 1 & result == "Uncontested"),
      selection_basis = if_else(elected == "1", "highest_recorded_votes", "sole_uncontested")
    ) |>
    filter(selected, !is.na(candidate_name), nzchar(trimws(candidate_name))) |>
    ungroup()
}
