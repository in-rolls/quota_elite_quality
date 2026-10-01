source("R/paper.R")

testthat::test_that("the abstract's setting summary matches the saved estimates", {
  ss <- setting_summary()
  head_rows <- rural$tier == "gp_head" & rural$outcome == "graduate_plus" & rural$state %in% c("Bihar", "Uttar Pradesh")
  heads <- rural[head_rows, ]
  testthat::expect_equal(ss$head_gap, 100 * c(-max(heads$estimate), -min(heads$estimate)))
  testthat::expect_true(all(heads$conf_high < 0))
  kerala <- rural[rural$state == "Kerala" & rural$tier == "gp_ward" & rural$outcome == "graduate_plus", ]
  testthat::expect_true(all(kerala$conf_low < 0 & kerala$conf_high > 0))
  testthat::expect_true(all(delhi$conf_high[delhi$outcome == "any_criminal"] < 0))
})

testthat::test_that("reserved-seat winners are younger on average in every setting", {
  testthat::expect_true(all(rural$estimate[rural$outcome == "age"] < 0))
  testthat::expect_true(all(delhi$estimate[delhi$outcome == "age"] < 0))
  testthat::expect_lt(mumbai$coef_quota[mumbai$outcome == "councillor_age"], 0)
})
