suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
  library(readr)
  library(knitr)
})

source_dir <- Sys.getenv("BIHAR_MASTER", "../local_elections/data/master")
sp <- read_parquet(file.path(source_dir, "candidates_bihar.parquet")) |>
  as_tibble() |>
  filter(year == 2016, tier == "kachahari_head") |>
  mutate(age_n = suppressWarnings(as.numeric(candidate_age)))

tab <- sp |>
  filter(age_n < 100) |>
  group_by(caste_reservation, woman_reserved) |>
  summarize(
    prop_illiterate = round(mean(candidate_education == "Illiterate", na.rm = TRUE), 2),
    prop_graduate_or_more = round(
      mean(candidate_education %in% c("Graduate", "Post Graduate"), na.rm = TRUE), 2
    ),
    mean_age = round(mean(age_n, na.rm = TRUE), 2),
    n = n(),
    .groups = "drop"
  )

kable(tab, format = "pipe", caption = "Bihar 2016 Sarpanch")


dir.create("output/legacy", recursive = TRUE, showWarnings = FALSE)
write_csv(tab, "output/legacy/bihar_candidates.csv")
