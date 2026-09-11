source("R/winners.R")

testthat::test_that("schooling preserves unknowns and distinguishes incomplete degrees", {
  up <- schooling(c("Graduate", "निरक्षर", NA, "जूनियर हाई\nहाई स्कूल"), "uttar_pradesh")
  testthat::expect_equal(up$graduate_plus, c(1L, 0L, NA_integer_, NA_integer_))
  raj <- schooling(c("Professional Graduate", "Other", "literate"), "rajasthan")
  testthat::expect_equal(raj$graduate_plus, c(1L, NA_integer_, 0L))
  kerala <- schooling(c(
    "BA,BED", "PRE-DEGREE", "SSLC FAILED", "BA FAILED", "DIPLOMA", NA,
    "MA ECONOMICS", "B-COM", "9 TH STANDARD"
  ), "kerala")
  testthat::expect_equal(kerala$graduate_plus, c(1L, 0L, 0L, NA_integer_, 0L, NA_integer_, 1L, 1L, 0L))
})

testthat::test_that("winner samples conserve unique seats and retain outcome missingness", {
  for (state in c("uttar_pradesh", "rajasthan", "kerala")) {
    d <- read_parquet(file.path("output", state, "winners.parquet"))
    desc <- read_csv(file.path("output", state, "descriptive.csv"), show_col_types = FALSE)
    testthat::expect_equal(anyDuplicated(d$row_id), 0L)
    testthat::expect_equal(sum(desc$n), nrow(d))
    testthat::expect_equal(sum(desc$education_n), sum(!is.na(d$graduate_plus)))
    testthat::expect_true(all(d$quota %in% 0:1))
    testthat::expect_true(all(na.omit(d$age) >= 21 & na.omit(d$age) <= 100))
  }
})

testthat::test_that("new state estimates agree with explicit fixed-effect OLS", {
  cases <- list(
    c("uttar_pradesh", "2010", "gp_head"), c("rajasthan", "2020", "gp_head"),
    c("kerala", "2020", "gp_ward")
  )
  for (case in cases) {
    state <- case[1]
    results <- read_csv(file.path("output", state, "regressions.csv"), show_col_types = FALSE) |>
      filter(year == as.integer(case[2]), tier == case[3], outcome == "graduate_plus")
    d <- read_parquet(file.path("output", state, "winners.parquet")) |>
      filter(
        year == as.integer(case[2]), tier == case[3], !is.na(graduate_plus),
        !is.na(.data[[results$geography]])
      ) |>
      as.data.frame()
    f <- as.formula(paste("graduate_plus ~ quota + factor(", results$geography, ") + factor(caste_reservation)"))
    m <- lm(f, data = d)
    testthat::expect_equal(unname(coef(m)["quota"]), results$estimate, tolerance = 1e-8)
    testthat::expect_equal(nobs(m), results$n)
    testthat::expect_equal(dplyr::n_distinct(d[[results$geography]]), results$clusters)
    controls <- as.formula(paste("quota ~ factor(", results$geography, ") + factor(caste_reservation)"))
    x <- residuals(lm(controls, data = d))
    scores <- tapply(x * residuals(m), d[[results$geography]], sum)
    groups <- results$clusters
    parameters <- 1 + dplyr::n_distinct(d$caste_reservation)
    adjustment <- groups / (groups - 1) * (nrow(d) - 1) / (nrow(d) - parameters)
    manual_se <- sqrt(adjustment * sum(scores^2)) / sum(x^2)
    testthat::expect_equal(manual_se, results$se, tolerance = 1e-8)
  }
})

testthat::test_that("Mumbai contains one complete qualification observation per spell", {
  d <- read_parquet("output/mumbai/winners.parquet")
  testthat::expect_equal(anyDuplicated(d$councillor_spell_id), 0L)
  testthat::expect_equal(nrow(d), 449L)
  testthat::expect_false(any(d$educ5 %in% c("Unknown", "Other")))
  testthat::expect_false(anyNA(d$any_criminal))
})

testthat::test_that("missing-education bounds collapse to OLS with complete data", {
  b <- read_csv("output/missing_education_bounds.csv", show_col_types = FALSE)
  complete <- filter(b, state == "uttar_pradesh", year == 2015)
  testthat::expect_equal(complete$all_winners_lower, complete$classified_sample_estimate, tolerance = 1e-9)
  testthat::expect_equal(complete$all_winners_upper, complete$classified_sample_estimate, tolerance = 1e-9)
  testthat::expect_true(all(b$all_winners_lower <= b$all_winners_upper))
  testthat::expect_true(all(b$all_winners_upper[b$state == "uttar_pradesh"] < 0))
})


testthat::test_that("incomplete degrees and ambiguous postgraduate diplomas remain missing", {
  d <- schooling(c(
    "DEGREE (DISCONTINUE), NTTC, DTP", "B-SC 1 YEAR", "PG DIPLOMA",
    "P G DIPLOMA IN CLINICAL NUTRITION", "L L B,HRM( P G DIPLOMA)"
  ), "kerala")
  testthat::expect_equal(d$graduate_plus, c(NA_integer_, NA_integer_, NA_integer_, NA_integer_, 1L))
})


testthat::test_that("education indicators describe disjoint categories on the same sample", {
  for (state in c("bihar_2016", "uttar_pradesh", "rajasthan")) {
    d <- read_parquet(file.path("output", state, "winners.parquet"))
    testthat::expect_identical(is.na(d$graduate_plus), is.na(d$illiterate))
    known <- !is.na(d$graduate_plus)
    middle <- 1 - d$graduate_plus[known] - d$illiterate[known]
    testthat::expect_true(all(middle %in% 0:1))
  }
})

testthat::test_that("all reported intervals use cluster rather than observation degrees of freedom", {
  rural <- read_csv("output/rural_estimates.csv", show_col_types = FALSE)
  mumbai <- read_csv("output/mumbai/regressions.csv", show_col_types = FALSE) |>
    rename(estimate = coef_quota)
  d <- bind_rows(rural, mumbai)
  critical <- qt(0.975, df = d$clusters - 1)
  testthat::expect_equal(d$conf_low, d$estimate - critical * d$se, tolerance = 1e-10)
  testthat::expect_equal(d$conf_high, d$estimate + critical * d$se, tolerance = 1e-10)
  testthat::expect_equal(d$p, 2 * pt(-abs(d$estimate / d$se), df = d$clusters - 1), tolerance = 1e-10)
})


testthat::test_that("Mumbai education and cases agree with explicit OLS and clustered covariance", {
  d <- as.data.frame(read_parquet("output/mumbai/winners.parquet"))
  results <- read_csv("output/mumbai/regressions.csv", show_col_types = FALSE)
  x <- residuals(lm(quota ~ factor(adminward) + factor(council), data = d))
  for (outcome in c("educ_grad_plus", "any_criminal")) {
    m <- lm(as.formula(paste(outcome, "~ quota + factor(adminward) + factor(council)")), data = d)
    row <- results |> filter(.data$outcome == .env$outcome)
    scores <- tapply(x * residuals(m), d$ward_no, sum)
    groups <- dplyr::n_distinct(d$ward_no)
    adjustment <- groups / (groups - 1) * (nrow(d) - 1) / (nrow(d) - m$rank)
    manual_se <- sqrt(adjustment * sum(scores^2)) / sum(x^2)
    testthat::expect_equal(unname(coef(m)["quota"]), row$coef_quota, tolerance = 1e-9)
    testthat::expect_equal(manual_se, row$se, tolerance = 1e-9)
  }
})
