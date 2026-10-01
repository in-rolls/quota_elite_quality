lit <- read_literature()

testthat::test_that("literature studies have source-registry entries", {
  sources <- readr::read_csv("evidence/sources.csv", show_col_types = FALSE)
  testthat::expect_true(all(lit$key %in% sources$citation))
})

testthat::test_that("registered artifacts exist and match their recorded hashes", {
  sources <- readr::read_csv("evidence/sources.csv", show_col_types = FALSE)
  artifacts <- sources[!is.na(sources$artifact) & nzchar(sources$artifact), ]
  paths <- file.path("evidence", artifacts$artifact)
  testthat::expect_true(all(file.exists(paths)), info = paste(paths[!file.exists(paths)], collapse = ", "))
  hashed <- which(!is.na(artifacts$artifact_sha256) & nzchar(artifacts$artifact_sha256))
  for (i in hashed) {
    testthat::expect_equal(
      digest::digest(paths[i], algo = "sha256", file = TRUE), artifacts$artifact_sha256[i],
      info = paths[i]
    )
  }
})

testthat::test_that("literature rows carry the fields the table and later pooling need", {
  required <- c("key", "outcome", "family", "unit", "population", "comparison", "diff", "se", "se_source", "status")
  testthat::expect_true(all(required %in% names(lit)))
  testthat::expect_false(anyNA(lit[required]))
  testthat::expect_true(all(lit$status == "confirmed"))
  testthat::expect_true(all(lit$se > 0))
  testthat::expect_true(all(lit$key %in% yaml::read_yaml("evidence/literature/tables.yaml")$study_order))
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
