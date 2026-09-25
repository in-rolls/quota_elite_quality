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

# The 2016 release derives each winner (top vote, uncontested, or a tie drawn by lot) and
# names none where the source cannot decide: a serial repeated with different votes, a tie
# with no lot mark, or several uncontested candidates. Those seats stay without a winner.
select_winners <- function(candidates) {
  filter(
    candidates, coalesce(elected == "1", FALSE),
    !is.na(candidate_name), nzchar(trimws(candidate_name))
  )
}
