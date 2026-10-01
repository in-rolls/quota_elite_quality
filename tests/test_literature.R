source("R/literature.R")

lit <- read_literature()

testthat::test_that("literature rows carry the fields the table and later pooling need", {
  required <- c("key", "outcome", "family", "unit", "population", "comparison", "diff", "se", "se_source", "status")
  testthat::expect_true(all(required %in% names(lit)))
  testthat::expect_false(anyNA(lit[required]))
  testthat::expect_true(all(lit$status == "confirmed"))
  testthat::expect_true(all(lit$se > 0))
  testthat::expect_true(all(lit$key %in% yaml::read_yaml("lit/tables.yaml")$study_order))
})

testthat::test_that("shares are proportions and unadjusted differences equal reserved minus open", {
  shares <- lit[lit$unit == "share" & !is.na(lit$reserved), ]
  testthat::expect_true(all(shares$reserved >= 0 & shares$reserved <= 1 & shares$open >= 0 & shares$open <= 1))
  raw <- lit[!lit$adjusted & !is.na(lit$reserved), ]
  # Sources round means and differences separately, so allow one unit in the last reported digit.
  testthat::expect_true(all(abs(raw$diff - (raw$reserved - raw$open)) <= 0.0101))
})

testthat::test_that("computed SEs follow the independent-proportions formula", {
  deininger <- lit[lit$key == "deininger" & lit$family == "secondary_plus", ]
  testthat::expect_equal(deininger$se, sqrt(0.2865 * 0.7135 / 180 + 0.6269 * 0.3731 / 459))
})

testthat::test_that("SEs backed out of t-statistics use the reported t", {
  kl <- lit[lit$key == "karekurvelee" & lit$family == "criminal", ]
  testthat::expect_equal(kl$se, 0.266 / 12.616)
})

testthat::test_that("overlapping samples share a sample group", {
  groups <- unique(lit[c("key", "sample_group")])
  testthat::expect_equal(groups$sample_group[groups$key == "cd"], groups$sample_group[groups$key == "beaman"])
})

testthat::test_that("numbers quoted in the Existing evidence section match the study files", {
  text <- readLines("manuscript/main.Rmd")
  text <- paste(text[seq(grep("^# Existing evidence", text), grep("^# Data and empirical", text) - 1)], collapse = " ")
  quoted <- c(
    cd = "−2.79 \\(SE 0.54\\)", beaman = "−2.10 \\(SE 0.55\\)", ban = "−2.62 \\(SE 0.77\\)",
    bamezai = "0.92 SD", bamezai = "−0.36 SD"
  )
  for (q in quoted) testthat::expect_match(text, q)
  value <- function(key, outcome) lit$diff[lit$key == key & lit$outcome == outcome & lit$population == "winners"]
  testthat::expect_equal(round(value("cd", "Years of schooling"), 2), -2.79)
  testthat::expect_equal(round(value("beaman", "Years of schooling"), 2), -2.10)
  testthat::expect_equal(round(value("ban", "Years of schooling"), 2), -2.62)
  testthat::expect_equal(round(value("bamezai", "Schooling, SD of all citizens"), 2), -0.92)
  testthat::expect_equal(round(value("bamezai", "Husband's schooling, SD of all citizens"), 2), -0.36)
})
