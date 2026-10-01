source("R/literature.R")

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

testthat::test_that("meta-analysis inputs count each sample once and match the saved estimates", {
  lit <- readr::read_csv("output/meta/literature_inputs.csv", show_col_types = FALSE)
  testthat::expect_false(anyDuplicated(lit$sample_group) > 0)
  own <- readr::read_csv("output/meta/own_inputs.csv", show_col_types = FALSE)
  rural <- readr::read_csv("output/rural_estimates.csv", show_col_types = FALSE)
  up <- rural[rural$state == "Uttar Pradesh" & rural$tier == "gp_head" & rural$outcome == "graduate_plus", ]
  testthat::expect_equal(sort(own$yi[own$setting == "Uttar Pradesh"]), sort(100 * up$estimate))
})
