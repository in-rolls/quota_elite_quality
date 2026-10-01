source("R/occupation.R")

testthat::test_that("occupation recode separates no own earnings from named work and missing entries", {
  x <- c("Nil", "House Wife", "unemployed", "Farmer", "Pensioner", "Social Worker", "Spouse Profession:", NA, "")
  testthat::expect_equal(no_earnings(x), c(1L, 1L, 1L, 0L, 0L, 0L, NA, NA, NA))
  testthat::expect_equal(no_earnings(x, "missing")[6], NA_integer_)
})
