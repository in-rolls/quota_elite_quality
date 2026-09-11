# Load lib
library(dplyr)
library(readr)
library(knitr)

# Load dat
kerala_lsgi <- read_csv(
  Sys.getenv(
    "KERALA_LEGACY",
    "../local_elections_kerala/data/lsgi-election-kerala.csv"
  ),
  show_col_types = FALSE
)

# Rename
names(kerala_lsgi)[names(kerala_lsgi) == "Educational Qualification"] <- "educ"

# Recode
kerala_lsgi$educ[is.na(kerala_lsgi$educ)] <- "Unknown"
kerala_lsgi$norm_educ <- tolower(gsub("\\s+", " ", gsub("\\.", "", kerala_lsgi$educ)))

#
source("R/kerala_legacy.R")

kerala_lsgi$recoded_education <- sapply(kerala_lsgi$norm_educ, recode_education)

# Define the desired order
desired_order <- c("General", "Woman", "SC", "SC Woman", "ST", "ST Woman")
kerala_lsgi$Reservation <- factor(kerala_lsgi$Reservation, levels = desired_order)

result <- kerala_lsgi |>
  group_by(Reservation, Year) |>
  summarize(
    sixth_or_lower = sum(recoded_education == "6th grade or lower") / n(),
    seventh_to_tenth = sum(recoded_education == "7th to 10th grade") / n(),
    eleventh_hs = sum(recoded_education == "11th or High School") / n(),
    prop_Bachelors_or_above = sum(recoded_education == "Bachelor's degree or above") / n(),
    prop_Masters_or_above = sum(recoded_education == "Master's degree or above") / n(),
    prop_Other = sum(recoded_education == "Other") / n(),
    n = n()
  ) |>
  arrange(Year, Reservation) |>
  mutate(across(c(
    "sixth_or_lower", "seventh_to_tenth",
    "eleventh_hs", "prop_Bachelors_or_above", "prop_Masters_or_above",
    "prop_Other"
  ), ~ round(., digits = 2)))

dir.create("output/legacy", recursive = TRUE, showWarnings = FALSE)
write_csv(result, "output/legacy/kerala_year.csv")

result <- kerala_lsgi |>
  group_by(`LGI Type`, Reservation) |>
  summarize(
    hs_or_below = sum(recoded_education %in% c(
      "6th grade or lower", "7th to 10th grade", "11th or High School"
    )) / n(),
    prop_bachelors = sum(recoded_education == "Bachelor's degree or above") / n(),
    prop_masters_or_above = sum(recoded_education == "Master's degree or above") / n(),
    prop_other = sum(recoded_education == "Other") / n(),
    n = n()
  ) |>
  arrange(`LGI Type`, Reservation) |>
  mutate(across(c("hs_or_below", "prop_bachelors", "prop_masters_or_above", "prop_other"), ~ round(., digits = 2)))

write_csv(result, "output/legacy/kerala_body.csv")
