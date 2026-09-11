library(dplyr)
library(readr)
library(knitr)

sp <- read_csv(Sys.getenv("BIHAR_SARPANCH", "../local_elections_bihar/data/sarpanch.csv"))
sp$age_n <- as.numeric(sp$age)


sp_fin <- sp |>
  filter(sp$age_n < 100)

tab <- sp_fin |>
  group_by(reservation_status) |>
  summarize(
    prop_illiterate = round(mean(educ == "Illiterate", na.rm = TRUE), 2),
    prop_graduate_or_more = round(mean(educ %in% c("Graduate", "Post Graduate"), na.rm = TRUE), 2),
    mean_age = round(mean(age_n, na.rm = TRUE), 2),
    n = n()
  )

kable(tab, format = "pipe", caption = "Bihar 2016 Sarpanch")


dir.create("output/legacy", recursive = TRUE, showWarnings = FALSE)
write_csv(tab, "output/legacy/bihar_candidates.csv")
