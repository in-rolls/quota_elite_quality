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
  testthat::expect_setequal(lit$key, c("cd", "ban", "afridi", "deininger"))
  testthat::expect_false("bamezai" %in% literature_inputs("beaman")$key)
  own <- own_estimates(report)
  rural <- report$rural
  up <- rural[rural$state == "Uttar Pradesh" & rural$tier == "gp_head" & rural$outcome == "graduate_plus", ]
  testthat::expect_equal(sort(own$yi[own$setting == "Uttar Pradesh"]), sort(100 * up$estimate))
})

testthat::test_that("the combined pool excludes incompatible estimands and overlapping Bihar evidence", {
  d <- report$meta$combined$inputs
  testthat::expect_equal(sum(d$source == "Literature"), 4)
  testthat::expect_equal(sum(d$source == "This paper"), 12)
  testthat::expect_equal(sum(d$series == "Bihar"), 1)
  testthat::expect_false(any(d$key == "bamezai", na.rm = TRUE))
  testthat::expect_false(anyDuplicated(d$id) > 0)
  testthat::expect_equal(n_distinct(d$series), 10)
  testthat::expect_equal(sum(d$series == "Uttar Pradesh"), 3)
  testthat::expect_true(all(d$n <= d$n_input, na.rm = TRUE))
  bamezai <- read_literature() |> filter(key == "bamezai", comparison == "reserved_vs_open", population == "winners")
  testthat::expect_error(standardized_difference(bamezai))
})

testthat::test_that("standardized logistic estimates preserve coefficients and clustered uncertainty", {
  d <- report$settings$mumbai$analysis
  m <- glm(educ_grad_plus ~ quota + adminward + factor(council), data = d, family = binomial())
  v <- sandwich::vcovCL(m, cluster = d$ward_no, type = "HC1")
  got <- report$meta$combined$inputs |> filter(series == "Mumbai")
  testthat::expect_equal(got$yi, unname(coef(m)["quota"]) * sqrt(3) / pi, tolerance = 1e-6)
  testthat::expect_equal(got$vi, unname(v["quota", "quota"]) * 3 / pi^2, tolerance = 1e-5)
  testthat::expect_equal(got$n, nobs(m))
  testthat::expect_equal(got$low, got$yi - qt(.975, got$clusters - 1) * sqrt(got$vi))
})

testthat::test_that("published binary conversions agree with the odds-ratio formula", {
  a <- read_literature() |> filter(key == "afridi", family == "secondary_plus")
  cells <- c(round(a$reserved * a$n_reserved), round(a$open * a$n_open))
  failures <- c(a$n_reserved, a$n_open) - cells
  log_or <- log(cells[1] / failures[1]) - log(cells[2] / failures[2])
  expected <- c(log_or * sqrt(3) / pi, sum(1 / c(cells, failures)) * 3 / pi^2)
  testthat::expect_equal(unname(standardized_difference(a)), expected)
})

testthat::test_that("Bhavnani contrasts hold caste reservation fixed and bound missing covariance", {
  x <- bhavnani_contrasts()
  odisha <- x |> filter(sample == "Odisha 2022", caste == "SC/ST")
  testthat::expect_equal(odisha$estimate, -3.30 - (-1.01))
  testthat::expect_equal(c(odisha$se_low, odisha$se_high), c(.12, .74))
  reds <- x |> filter(sample == "REDS survey 2014-16", caste == "SC/ST")
  testthat::expect_equal(reds$estimate, -2.86 - (-2.28))
  testthat::expect_equal(c(reds$se_low, reds$se_high), c(.57, 2.71))
  testthat::expect_false("bhavnani" %in% literature_inputs()$key)
})
