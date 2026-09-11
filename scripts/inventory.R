suppressPackageStartupMessages({
  library(arrow)
  library(dplyr)
  library(readr)
  library(stringr)
})
root <- Sys.getenv("RESERVATIONS_MASTER", "../local_elections/data/master")
extras <- read_parquet(file.path(root, "master_extras.parquet")) |>
  filter(column %in% c(
    "winner_education", "winner_age",
    "winner_occupation", "criminal_history", "movable_property",
    "immovable_property"
  ))
rows <- list()
for (path in list.files(root, "^master_.*parquet$", full.names = TRUE)) {
  if (basename(path) == "master_extras.parquet") next
  seats <- read_parquet(path) |> as_tibble()
  joined <- inner_join(seats |> select(row_id, state, year, tier, woman_reserved, winner),
    extras,
    by = "row_id", relationship = "one-to-many"
  )
  rows[[length(rows) + 1L]] <- joined |>
    group_by(state, year, tier, column) |>
    summarise(
      records = n(), reservation_present = sum(!is.na(woman_reserved)),
      named_winner = sum(!is.na(winner) & nzchar(trimws(winner))), .groups = "drop"
    )
}
write_csv(bind_rows(rows), "output/inventory/pooled_winner_fields.csv")
