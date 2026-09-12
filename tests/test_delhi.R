source("R/delhi.R")

testthat::test_that("Delhi recodes preserve unknown qualifications and cases", {
  edu <- delhi_education(c("Graduate Professional", "5th class", "Illiterate", "Others", "not Given", NA))
  testthat::expect_equal(edu$graduate_plus, c(1L, 0L, 0L, NA_integer_, NA_integer_, NA_integer_))
  testthat::expect_equal(edu$illiterate, c(0L, 0L, 1L, NA_integer_, NA_integer_, NA_integer_))
  testthat::expect_equal(
    delhi_integer(c("0", "9", "na", "scan unavailable", "", NA, "-1", "1.5")),
    c(0L, 9L, rep(NA_integer_, 6))
  )
})

testthat::test_that("Delhi winners conserve seats and match the official 2012 schedule", {
  d <- arrow::read_parquet("output/delhi/winners.parquet")
  flow <- readr::read_csv("output/delhi/sample_flow.csv", show_col_types = FALSE)
  testthat::expect_equal(anyDuplicated(d$ward_id), 0L)
  testthat::expect_equal(flow$winners, c(272, 272, 250))
  testthat::expect_equal(flow$classified_education, c(226, 262, 244))
  testthat::expect_equal(flow$recorded_cases, c(237, 267, 248))
  testthat::expect_equal(sum(flow$winners), nrow(d))
  testthat::expect_identical(is.na(d$graduate_plus), is.na(d$illiterate))
  testthat::expect_true(all(d$graduate_plus + d$illiterate <= 1, na.rm = TRUE))
  roster <- delhi_2012_roster()
  testthat::expect_equal(sum(roster$quota), 138L)
  x <- d |> dplyr::filter(year == 2012)
  matched <- match(as.integer(x$ward_number), roster$ward_number)
  testthat::expect_equal(x$quota, roster$quota[matched])
  testthat::expect_equal(x$caste_reservation, roster$caste_reservation[matched])
  testthat::expect_equal(x$corporation, roster$corporation[matched])
})

testthat::test_that("Delhi estimates and inference match explicit OLS and manual clustered covariance", {
  d <- arrow::read_parquet("output/delhi/winners.parquet")
  results <- readr::read_csv("output/delhi/regressions.csv", show_col_types = FALSE)
  for (i in seq_len(nrow(results))) {
    row <- results[i, ]
    sample <- d |> dplyr::filter(year == row$year, !is.na(.data[[row$outcome]]))
    m <- lm(as.formula(paste(row$outcome, "~ quota + factor(assembly_id) + factor(caste_reservation)")),
      data = sample
    )
    x <- residuals(lm(quota ~ factor(assembly_id) + factor(caste_reservation), data = sample))
    groups <- dplyr::n_distinct(sample$assembly_id)
    scores <- tapply(x * residuals(m), sample$assembly_id, sum)
    parameters <- 1 + dplyr::n_distinct(sample$caste_reservation)
    adjustment <- groups / (groups - 1) * (nrow(sample) - 1) / (nrow(sample) - parameters)
    manual_se <- sqrt(adjustment * sum(scores^2)) / sum(x^2)
    estimate <- unname(coef(m)["quota"])
    testthat::expect_equal(estimate, row$estimate, tolerance = 1e-9)
    testthat::expect_equal(manual_se, row$se, tolerance = 1e-9)
    testthat::expect_equal(nobs(m), row$n)
    testthat::expect_equal(estimate + c(-1, 1) * qt(.975, groups - 1) * manual_se,
      c(row$conf_low, row$conf_high),
      tolerance = 1e-9
    )
    testthat::expect_equal(2 * pt(-abs(estimate / manual_se), groups - 1), row$p, tolerance = 1e-9)
  }
})

testthat::test_that("missing-outcome extrema are attained by binary completions", {
  d <- arrow::read_parquet("output/delhi/winners.parquet")
  results <- readr::read_csv("output/delhi/missing_outcome_bounds.csv", show_col_types = FALSE)
  for (i in seq_len(nrow(results))) {
    row <- results[i, ]
    sample <- d |> dplyr::filter(year == row$year)
    x <- residuals(lm(quota ~ factor(assembly_id) + factor(caste_reservation), data = sample))
    missing <- is.na(sample[[row$outcome]])
    extrema <- vapply(c(FALSE, TRUE), function(upper) {
      completed <- sample[[row$outcome]]
      completed[missing] <- as.integer(if (upper) x[missing] > 0 else x[missing] < 0)
      unname(coef(lm(completed ~ quota + factor(assembly_id) + factor(caste_reservation), data = sample))["quota"])
    }, numeric(1))
    testthat::expect_equal(extrema, c(row$lower, row$upper), tolerance = 1e-9)
    testthat::expect_equal(sum(missing), row$missing)
  }
})
