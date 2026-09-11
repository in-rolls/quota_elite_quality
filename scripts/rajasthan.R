library(dplyr)
library(readr)
library(knitr)


raj_2019 <- readr::read_csv(file.path(
  Sys.getenv(
    "RAJASTHAN_MUNICIPAL",
    "../local_elections/data/rajasthan/municipal"
  ),
  "rajasthan_municipal_2019.csv"
))
ed <- readr::read_csv(file.path(
  Sys.getenv(
    "RAJASTHAN_MUNICIPAL",
    "../local_elections/data/rajasthan/municipal"
  ),
  "rajasthan_municipal_2019_education_winning.csv"
))

dat <- left_join(
  raj_2019[, c("sr_no", "district", "ward_category", "age")],
  ed[, c("sr_no", "elected_member_educ")],
  by = "sr_no",
  copy = FALSE,
  keep = FALSE,
  relationship = "one-to-one"
)

tab_4 <- dat |>
  group_by(ward_cat = tolower(ward_category)) |>
  summarize(
    prop_hs_or_less = round(mean(elected_member_educ %in% c(
      "PRIMARY",
      "LITERA TE", "LITERATE", "ILLITERA TE", "10th",
      "12th", "3th", "4th", "5 th", "5th", "5TH", "6th",
      "6TH", "7th", "8 th", "8 TH", "8th", "8TH", "9th",
      "9TH"
    ), na.rm = TRUE), 2),
    mean_age = round(mean(age)),
    n = n()
  )

kable(tab_4, format = "pipe", caption = "Rajasthan 2019 Municipal")


dir.create("output/legacy", recursive = TRUE, showWarnings = FALSE)
write_csv(tab_4, "output/legacy/rajasthan_municipal.csv")
