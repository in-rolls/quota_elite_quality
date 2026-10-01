testthat::test_that("subgroup SDs combine within and between variation", {
  beaman <- read_literature()
  beaman <- beaman[beaman$key == "beaman" & beaman$family == "years", ]
  testthat::expect_equal(round(combined_sd(beaman$parts_reserved[[1]]), 3), 3.751)
  testthat::expect_equal(round(combined_sd(beaman$parts_open[[1]]), 3), 3.063)
})

testthat::test_that("Cohen's d for Ban and Rao uses the pooled Table 8 SDs", {
  ban <- read_literature()
  ban <- ban[ban$key == "ban" & ban$family == "years", ]
  pooled <- sqrt((26 * 4.287^2 + 78 * 3.426^2) / 104)
  testthat::expect_equal(unname(standardized_difference(ban)), c(-2.620 / pooled, (0.766 / pooled)^2))
})

testthat::test_that("meta-analysis inputs count each sample once and match the computed estimates", {
  lit <- literature_inputs()
  testthat::expect_false(anyDuplicated(lit$sample_group) > 0)
  own <- own_estimates(report)
  rural <- report$rural
  up <- rural[rural$state == "Uttar Pradesh" & rural$tier == "gp_head" & rural$outcome == "graduate_plus", ]
  testthat::expect_equal(sort(own$yi[own$setting == "Uttar Pradesh"]), sort(100 * up$estimate))
})
