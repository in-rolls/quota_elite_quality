# No own earnings: the winner reports no occupation, unemployment, homemaking or study. Pensioners and
# every named trade, job or farm count as earning. Blank and unreadable entries stay missing. Social work
# often means full-time party work; `social_work` sets whether it counts as an occupation or as missing.
no_earnings <- function(x, social_work = c("occupation", "missing")) {
  social_work <- match.arg(social_work)
  label <- stringr::str_squish(stringr::str_to_lower(x))
  none <- paste0(
    "^(nil|none|no|no job|no work|not working|nothing|unemployed|un employed|jobless|",
    "house ?wife|house ?hold|household|home ?maker|housekeeping|domestic work|student)$"
  )
  missing <- is.na(label) | label %in% c("", "-", "--", "na", "n/a", "not given", "not stated", "0") |
    stringr::str_detect(label, ":$")
  if (social_work == "missing") missing <- missing | stringr::str_detect(label, "^social (worker|work|service)$")
  out <- as.integer(stringr::str_detect(label, none))
  out[missing] <- NA_integer_
  out
}
