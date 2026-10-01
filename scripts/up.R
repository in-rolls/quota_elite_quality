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


# 2021 candidates come from the central master; the seat's raw reservation label
# is the grouping the original 2021 file carried.
master_dir <- Sys.getenv("LOCAL_ELECTIONS_MASTER", "../local_elections/data/master")
up_seats <- arrow::read_parquet(file.path(master_dir, "master_uttar_pradesh.parquet")) |>
  filter(year == 2021) |>
  select(row_id, reservation = reservation_raw)
up_2021 <- arrow::read_parquet(file.path(master_dir, "candidates_uttar_pradesh.parquet")) |>
  filter(year == 2021) |>
  inner_join(up_seats, by = "row_id", relationship = "many-to-one")
stopifnot(nrow(up_2021) == 373096L)

tab_3 <- up_2021 |>
  group_by(reservation) |>
  summarize(
    prop_illiterate = round(mean(candidate_education == "निरक्षर", na.rm = TRUE), 2),
    prop_college_or_more = round(
      mean(candidate_education %in% c("परास्नातक", "स्नातक", "पी० एच० डी०"), na.rm = TRUE), 2
    ),
    n = n()
  )

kable(tab_3, format = "pipe", caption = "UP 2021")
