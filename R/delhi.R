delhi_2012_roster <- function() {
  # Delhi SEC notification, 27 January 2012, pp. 4-8 and Annexure III.
  # Within corporation and caste category, alternate sorted wards start with women.
  sc <- c(
    3, 5, 9, 16, 17, 26, 31, 35, 37, 42, 46, 64, 65, 71, 74, 82, 87, 92, 95, 151,
    104, 123, 130, 133, 140, 143, 156, 167, 171, 175, 179, 182, 194, 200, 203,
    210, 213, 218, 226, 233, 237, 243, 245, 255, 262, 265
  )
  tibble::tibble(
    ward_number = 1:272,
    corporation = dplyr::case_when(
      ward_number %in% c(1:100, 149:152) ~ "North",
      ward_number >= 209 ~ "East", TRUE ~ "South"
    ),
    caste_reservation = ifelse(ward_number %in% sc, "SC", "NONE")
  ) |>
    dplyr::group_by(corporation, caste_reservation) |>
    dplyr::mutate(quota = as.integer(dplyr::row_number() %% 2 == 1)) |>
    dplyr::ungroup()
}

delhi_education <- function(x) {
  x <- tolower(trimws(x))
  degrees <- c("graduate", "graduate professional", "post graduate", "doctorate")
  below <- c("illiterate", "literate", "5th pass", "5th class", "6th class", "8th pass", "10th pass", "12th pass")
  classified <- x %in% c(degrees, below)
  tibble::tibble(
    graduate_plus = ifelse(classified, as.integer(x %in% degrees), NA_integer_),
    illiterate = ifelse(classified, as.integer(x == "illiterate"), NA_integer_)
  )
}

delhi_integer <- function(x) {
  valid <- !is.na(x) & grepl("^[0-9]+$", trimws(x))
  result <- rep(NA_integer_, length(x))
  result[valid] <- as.integer(trimws(x[valid]))
  result
}

prepare_delhi <- function(raw, year) {
  stopifnot(year %in% c(2012, 2017))
  raw$source_row <- seq_len(nrow(raw))
  raw <- raw[which(tolower(trimws(raw[["Election Outcome"]])) == "winner"), ]
  reservation <- tolower(trimws(raw[["Reservation Status"]]))
  stopifnot(all(reservation %in% c("women", "general", "scw", "sc")))
  ward <- trimws(raw[["Ward Number"]])
  if (year == 2012) {
    roster <- delhi_2012_roster()
    matched <- match(as.integer(ward), roster$ward_number)
    stopifnot(!anyNA(matched))
    corporation <- roster$corporation[matched]
    stopifnot(all(as.integer(reservation %in% c("women", "scw")) == roster$quota[matched]))
    source_caste <- ifelse(reservation %in% c("scw", "sc"), "SC", "NONE")
    stopifnot(all(source_caste == roster$caste_reservation[matched]))
  } else {
    direction <- sub(".*-", "", ward)
    stopifnot(all(direction %in% c("N", "S", "E")), all(direction == trimws(raw$Direction)))
    corporation <- unname(c(N = "North", S = "South", E = "East")[direction])
  }
  age <- delhi_integer(raw$Age)
  age[!is.na(age) & (age < 21 | age > 100)] <- NA_integer_
  education_raw <- raw[[if (year == 2012) "Education" else "EDUCATION"]]
  cases_raw <- raw[[if (year == 2012) "Pending Criminal Cases" else "Pending Criminal Cases(Affidavit)"]]
  cases <- delhi_integer(cases_raw)
  d <- tibble::tibble(
    year = as.integer(year), source_row = raw$source_row, ward_id = paste(year, ward, sep = ":"),
    ward_number = ward, corporation = corporation,
    reservation_raw = raw[["Reservation Status"]],
    quota = as.integer(reservation %in% c("women", "scw")),
    caste_reservation = ifelse(reservation %in% c("scw", "sc"), "SC", "NONE"),
    education_raw = education_raw, age_raw = raw$Age, age = age,
    cases_raw = cases_raw, pending_cases = cases, any_criminal = as.integer(cases > 0)
  ) |>
    dplyr::bind_cols(delhi_education(education_raw))
  stopifnot(!anyDuplicated(d$ward_id))
  d
}

delhi_bounds <- function(d, outcome) {
  residual_model <- stats::lm(quota ~ factor(assembly_id) + factor(caste_reservation), data = d)
  weights <- residuals(residual_model) / sum(residuals(residual_model)^2)
  observed <- !is.na(d[[outcome]])
  known <- sum(weights[observed] * d[[outcome]][observed])
  tibble::tibble(
    outcome = outcome, lower = known + sum(pmin(weights[!observed], 0)),
    upper = known + sum(pmax(weights[!observed], 0)), missing = sum(!observed), n = nrow(d)
  )
}
