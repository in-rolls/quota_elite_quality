library(dplyr)
library(purrr)
library(readr)
library(knitr)


up_all <- list.files(Sys.getenv("UP_2015_CANDIDATES", "data/up_2015"), pattern = "\\.csv$", full.names = TRUE)
if (!length(up_all)) stop("Set UP_2015_CANDIDATES to the original 2015 candidate CSV directory.")

up_all_dat <- up_all |>
  map_dfr(read_csv, col_select = c("पद का आरक्षण", "शैक्षिक योग्यता"), .id = "source", show_col_types = FALSE)

names(up_all_dat) <- c("source", "reservation_status", "education")

up_all_dat <- up_all_dat |> mutate(position = case_when(
  grepl("पंचायत प्रमुख", source) ~ "area panchayat pramukh",
  grepl("क्षेत्र पंचायत सदस्य", source) ~ "area panchayat member",
  grepl("ग्राम पंचायत प्रधान", source) ~ "gram panchayat pradhan",
  grepl("जिला पंचायत अध्यक्", source) ~ "zila panchayat adhyaksh",
  grepl("जिला पंचायत सदस्य", source) ~ "zila panchayat pradhan"
))

tab <- up_all_dat |>
  group_by(reservation_status) |>
  summarize(
    prop_illiterate = round(mean(education == "निरक्षर", na.rm = TRUE), 2),
    prop_college_or_more = round(mean(education %in% c("परास्नातक", "स्नातक", "पी० एच० डी०"), na.rm = TRUE), 2),
    n = n()
  )

kable(tab, format = "pipe", caption = "UP 2015")

tab_2 <- up_all_dat |>
  group_by(reservation_status, position) |>
  summarize(
    prop_illiterate = round(mean(education == "निरक्षर", na.rm = TRUE), 2),
    prop_college_or_more = round(mean(education %in% c("परास्नातक", "स्नातक", "पी० एच० डी०"), na.rm = TRUE), 2),
    n = n()
  )


up_2021 <- readr::read_csv(Sys.getenv(
  "UP_2021_CANDIDATES",
  "../local_elections_up/data/up_gram_panchayat_pradhan_2021.csv.zip"
))

tab_3 <- up_2021 |>
  group_by(reservation) |>
  summarize(
    prop_illiterate = round(mean(education_2021 == "निरक्षर", na.rm = TRUE), 2),
    prop_college_or_more = round(mean(education_2021 %in% c("परास्नातक", "स्नातक", "पी० एच० डी०"), na.rm = TRUE), 2),
    n = n()
  )

kable(tab_3, format = "pipe", caption = "UP 2021")
