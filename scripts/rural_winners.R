source("R/winners.R")

for (state in c("uttar_pradesh", "rajasthan", "kerala")) {
  seats <- read_seats(state)
  seats <- if (state == "uttar_pradesh") {
    filter(seats, year %in% c(2010, 2015, 2021))
  } else if (state == "rajasthan") {
    filter(seats, year == 2020, tier == "gp_head")
  } else {
    filter(seats, year %in% c(2010, 2015, 2020))
  }
  d <- left_join(seats, winner_extras(seats), by = "row_id", relationship = "one-to-one")
  if (state == "uttar_pradesh") {
    candidates <- read_parquet(file.path(master_dir(), "candidates_uttar_pradesh.parquet")) |>
      filter(year == 2021, elected == "1") |>
      select(row_id, candidate_education, candidate_age)
    stopifnot(!anyDuplicated(candidates$row_id))
    d <- left_join(d, candidates, by = "row_id", relationship = "one-to-one") |>
      mutate(
        winner_education = if_else(year == 2021, candidate_education, winner_education),
        winner_age = if_else(year == 2021, candidate_age, winner_age)
      )
  }
  stopifnot(nrow(d) == nrow(seats))
  d <- prepare_controls(d, state)
  fit_winners(d, state)
}
