source("scripts/bihar_winners.R")

testthat::test_that("unknown schooling never becomes observed non-graduate or literate", {
  d <- education_outcomes(c("Graduate", "Post Graduate", "Illiterate", "--", "Others", NA))
  testthat::expect_equal(d$graduate_plus, c(1L, 1L, 0L, NA, NA, NA))
  testthat::expect_equal(d$illiterate, c(0L, 0L, 1L, NA, NA, NA))
  testthat::expect_error(education_outcomes("unreviewed"), "Unreviewed")
})

testthat::test_that("absorbed effects match explicit OLS and clustered score sums", {
  d <- read_parquet("output/bihar_2016/winners.parquet") %>%
    filter(tier == "gp_head", !is.na(graduate_plus), !is.na(block_id)) %>%
    as.data.frame()
  explicit <- lm(graduate_plus ~ quota + block_id + caste_reservation, data = d)
  absorbed <- feols(graduate_plus ~ quota | block_id + caste_reservation,
    data = d, vcov = ~block_id,
    ssc = ssc(K.adj = FALSE, G.adj = FALSE)
  )
  x <- residuals(lm(quota ~ block_id + caste_reservation, data = d))
  scores <- tapply(x * residuals(explicit), d$block_id, sum)
  manual_se <- sqrt(sum(scores^2, na.rm = TRUE)) / sum(x^2)
  testthat::expect_equal(unname(coef(explicit)["quota"]),
    unname(coef(absorbed)["quota"]),
    tolerance = 1e-9
  )
  testthat::expect_equal(manual_se, unname(se(absorbed)["quota"]), tolerance = 1e-9)
  saved <- read_csv("output/bihar_2016/regressions.csv", show_col_types = FALSE) %>%
    filter(tier == "gp_head", outcome == "graduate_plus", primary)
  testthat::expect_equal(saved$estimate, unname(coef(explicit)["quota"]), tolerance = 1e-9)
})

testthat::test_that("uncontested records qualify only when the source has one candidate", {
  candidates <- tibble(
    row_id = c("a", "a", "b", "c", "d", "d", "e"),
    candidate_name = c("A", "B", "C", "D", "E", "F", "G"),
    elected = c("0", "1", "0", "0", "0", "0", "0"),
    result = c("0", "0", "Uncontested", "Vacant", "Uncontested", "Uncontested", "0")
  )
  d <- select_winners(candidates)
  testthat::expect_equal(d$candidate_name, c("B", "C"))
  testthat::expect_equal(d$selection_basis, c("highest_recorded_votes", "sole_uncontested"))
})
