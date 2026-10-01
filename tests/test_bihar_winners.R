testthat::test_that("unknown schooling never becomes observed non-graduate or literate", {
  d <- education_outcomes(c("Graduate", "Post Graduate", "Illiterate", "--", "Others", NA))
  testthat::expect_equal(d$graduate_plus, c(1L, 1L, 0L, NA, NA, NA))
  testthat::expect_equal(d$illiterate, c(0L, 0L, 1L, NA, NA, NA))
  testthat::expect_error(education_outcomes("unreviewed"), "Unreviewed")
})

testthat::test_that("absorbed effects match explicit OLS and clustered score sums", {
  d <- report$settings$bihar_2016$analysis |>
    filter(tier == "gp_head", !is.na(graduate_plus), !is.na(block_id)) |>
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
  saved <- report$settings$bihar_2016$estimates |>
    filter(tier == "gp_head", outcome == "graduate_plus", primary)
  testthat::expect_equal(saved$estimate, unname(coef(explicit)["quota"]), tolerance = 1e-9)
})

testthat::test_that("only winners the release names are selected", {
  candidates <- tibble(
    row_id = c("a", "a", "b", "c", "c", "d"),
    candidate_name = c("A", "B", "C", "D", "E", " "),
    elected = c("0", "1", "1", NA, NA, "1")
  )
  testthat::expect_equal(select_winners(candidates)$candidate_name, c("B", "C"))
  sole <- tibble(row_id = c("e", "f"), candidate_name = c("F", "G"), elected = NA_character_)
  testthat::expect_equal(
    select_winners(bind_rows(candidates, sole), sole = "e")$candidate_name, c("B", "C", "F")
  )
})
