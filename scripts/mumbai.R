# Mumbai (BMC) councillors: does the women's quota change who gets elected?
# Reads the Praja ward-by-wave table held in the sibling local_reservations
# clone (affidavit fields for the 2012 and 2017 councils; the 2007 council has
# none). The performance side of the same data lives in ../quota_unquote.
# Run from the repo root: Rscript scripts/mumbai.R

suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(stringr)
  library(purrr)
  library(fixest)
  library(knitr)
})

ratings_path <- Sys.getenv(
  "MUMBAI_RATINGS",
  "../local_elections/data/maharashtra/mumbai/praja_ward_ratings_2011_2018.csv"
)
ratings_sha256 <- "2f4bbbadd1783e84e57d55bce8a29764f37e89c5e40b0b8522fe0d4bc2cc1032"
got <- digest::digest(ratings_path, algo = "sha256", file = TRUE)
if (got != ratings_sha256) stop("ratings file differs from the pinned SHA-256: ", got)

out_dir <- "output/mumbai"
dir.create(out_dir, showWarnings = FALSE, recursive = TRUE)
write_md <- function(x, file, caption) {
  txt <- c(caption, "", kable(x, format = "pipe", digits = 3))
  writeLines(txt, file.path(out_dir, file))
  cat("\n", file, "\n", sep = "")
  print(kable(x, format = "pipe", digits = 3))
}

d <- read_csv(ratings_path, show_col_types = FALSE, guess_max = 2000) |>
  mutate(
    council = as.integer(council),
    quota = as.integer(woman_reserved),
    female = as.integer(councillor_woman),
    adminward = factor(adminward),
    any_criminal = as.integer(councillor_criminal_cases > 0)
  )

# The deposit's quota flag: 76 women's seats in the 2007 council (one third),
# 114 from 2012 (one half). Wrong counts would mean a miscoded treatment.
counts <- d |>
  group_by(council, survey_year) |>
  summarise(quota = sum(quota), .groups = "drop")
print(counts)
stopifnot(all(counts$quota[counts$council == 2007] == 76))
stopifnot(all(counts$quota[counts$council >= 2012] == 114))

source("R/mumbai.R")
d <- d |> mutate(
  educ5 = map_chr(normalize_educ(councillor_education), recode_educ_mumbai),
  educ_hs_or_less = as.integer(educ5 %in% c("Below 10th", "10th (SSC)")),
  educ_grad_plus = as.integer(educ5 %in% c("Bachelor's", "Master's / professional"))
)
cat("\nEducation strings not matched (should be empty):\n")
print(d |> filter(educ5 == "Other") |> count(councillor_education, sort = TRUE))

# One row per councillor spell: affidavit fields repeat across the waves of a term.
cand <- d |>
  filter(!is.na(councillor_age)) |>
  group_by(councillor_spell_id) |>
  slice_min(survey_year, n = 1, with_ties = FALSE) |>
  ungroup()

quality_tab <- cand |>
  mutate(seat = if_else(quota == 1, "Reserved for women", "Open")) |>
  group_by(council, seat) |>
  summarise(
    n = n(),
    `share female` = mean(female),
    `share HS or less` = mean(educ_hs_or_less[educ5 != "Unknown"]),
    `share graduate+` = mean(educ_grad_plus[educ5 != "Unknown"]),
    `mean age` = mean(councillor_age, na.rm = TRUE),
    `share any criminal case` = mean(any_criminal, na.rm = TRUE),
    `mean criminal cases` = mean(councillor_criminal_cases, na.rm = TRUE),
    .groups = "drop"
  )
write_md(
  quality_tab, "tab_quality.md",
  "Who gets elected in Mumbai: councillors by seat type and council (affidavit data; none for the 2007 council)."
)

quality_reg <- map_dfr(
  c("educ_hs_or_less", "educ_grad_plus", "councillor_age", "any_criminal"),
  function(y) {
    m <- feols(as.formula(paste(y, "~ quota | adminward + council")), data = cand, cluster = ~ward_no)
    ci <- as.numeric(confint(m, "quota"))
    tibble(
      outcome = y, coef_quota = coef(m)[["quota"]], se = se(m)[["quota"]],
      conf_low = ci[1], conf_high = ci[2],
      p = pvalue(m)[["quota"]], n = nobs(m), clusters = n_distinct(cand$ward_no)
    )
  }
)
write_md(
  quality_reg, "tab_quality_reg.md",
  paste(
    "Reserved-seat effect on councillor characteristics;",
    "administrative-ward and council effects, SE clustered by ward."
  )
)
cat("\nDone. Outputs in", out_dir, "\n")

write_csv(quality_tab, file.path(out_dir, "descriptive.csv"))
write_csv(quality_reg, file.path(out_dir, "regressions.csv"))
arrow::write_parquet(
  cand |> select(
    councillor_spell_id,
    council, quota, adminward, ward_no, educ5, educ_hs_or_less,
    educ_grad_plus, councillor_age, any_criminal
  ),
  file.path(out_dir, "winners.parquet"),
  compression = "zstd"
)
