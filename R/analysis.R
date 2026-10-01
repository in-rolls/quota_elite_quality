suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
  library(readr)
  library(tidyr)
  library(stringr)
  library(purrr)
  library(fixest)
})

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

check_inputs <- function() {
  master_directory <- Sys.getenv("LOCAL_ELECTIONS_MASTER", "../local_elections/data/master")
  inputs <- readr::read_csv("evidence/analysis_inputs.csv", show_col_types = FALSE)
  for (i in seq_len(nrow(inputs))) {
    path <- inputs$path[i]
    if (grepl("/data/master/", path, fixed = TRUE)) {
      path <- file.path(master_directory, basename(path))
    }
    if (!file.exists(path)) stop("Missing input: ", path)
    actual <- digest::digest(path, algo = "sha256", file = TRUE)
    if (actual != inputs$sha256[i]) stop("Input changed; review before updating the manifest: ", path)
  }
  cat("All analysis input hashes match.\n")

  state_sources <- jsonlite::read_json(file.path(dirname(master_directory), "sources.json"))
  for (i in which(!is.na(inputs$state_source_provider))) {
    provider <- inputs$state_source_provider[i]
    if (!identical(state_sources[[provider]]$ref, inputs$state_source_ref[i])) {
      stop("State input revision changed; review the rebuilt results: ", provider)
    }
  }
  cat("UP and Rajasthan source revisions match the shared pipeline.\n")
}

source("R/rural.R")
source("R/urban.R")
source("R/literature.R")

run_analysis <- function() {
  check_inputs()
  settings <- list()
  for (year in names(ELECTIONS)) {
    settings[[paste0("bihar_", year)]] <- run_bihar(year)
  }
  bounds <- list()
  for (state in c("uttar_pradesh", "rajasthan", "kerala")) {
    settings[[state]] <- fit_winners(prepare_rural(state), state)
    bounds[[state]] <- missing_education_bounds(settings[[state]]$analysis, settings[[state]]$estimates, state)
  }
  settings$mumbai <- run_mumbai()
  settings$delhi <- run_delhi()
  bihar <- bind_rows(lapply(c(2016, 2021), function(year) {
    settings[[paste0("bihar_", year)]]$estimates |>
      filter(primary) |>
      mutate(state = "Bihar", year = year)
  }))
  rural <- bind_rows(bihar, bind_rows(lapply(c("uttar_pradesh", "rajasthan", "kerala"), function(state) {
    settings[[state]]$estimates
  }))) |>
    mutate(state = recode(state, uttar_pradesh = "Uttar Pradesh", rajasthan = "Rajasthan", kerala = "Kerala"))
  report <- list(settings = settings, rural = rural, bounds = bind_rows(bounds))
  report$meta <- run_meta(report)
  report$deposit <- compare_deposits()
  report
}
