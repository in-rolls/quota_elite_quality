# Forest plots for the two meta-analyses in scripts/meta.R.
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
  library(ggplot2)
})
source("R/style.R")
source("R/literature.R")

blue <- "#165B85"
diamond <- function(y, est, low, high) {
  tibble(x = c(low, est, high, est), y = c(y, y + 0.3, y, y - 0.3))
}

# Panel of every published schooling comparison among elected leaders, including both Birbhum studies;
# the pooled estimate uses one per sample.
lit <- read_literature() |>
  filter(population == "winners", comparison == "reserved_vs_open", family %in% c("years", "secondary_plus"))
es <- t(vapply(seq_len(nrow(lit)), function(i) standardized_difference(lit[i, ]), c(yi = 0, vi = 0)))
lit <- lit |>
  mutate(yi = es[, "yi"], vi = es[, "vi"], low = yi - 1.96 * sqrt(vi), high = yi + 1.96 * sqrt(vi)) |>
  arrange(desc(match(key, yaml::read_yaml("evidence/literature/tables.yaml")$study_order))) |>
  mutate(
    row = row_number() + 1,
    label = paste0(label, ", ", sub(" \\(.*", "", state), ": ", tolower(sub(",.*", "", outcome)))
  )
pooled <- read_csv("output/meta/literature.csv", show_col_types = FALSE)[1, ]
lit_plot <- ggplot(lit, aes(x = yi, y = row)) +
  geom_vline(xintercept = 0, colour = "grey60", linewidth = 0.4) +
  geom_errorbar(aes(xmin = low, xmax = high), orientation = "y", width = 0.2, colour = blue) +
  geom_point(colour = blue, size = 2) +
  geom_polygon(
    data = diamond(0.6, pooled$d, pooled$d_low, pooled$d_high), aes(x = x, y = y),
    fill = "grey25", inherit.aes = FALSE
  ) +
  scale_y_continuous(breaks = c(0.6, lit$row), labels = c("Pooled, one per sample", lit$label)) +
  labs(x = "Reserved minus open seats (Cohen's d)", y = NULL) +
  theme_evidence()
save_evidence(lit_plot, "output/meta/forest_literature", width = 7.5, height = 3.6)

own <- read_csv("output/meta/own_inputs.csv", show_col_types = FALSE) |>
  mutate(
    group = if_else(northern == 1, "Northern village heads", "Kerala ward members and city councillors"),
    low = yi - 1.96 * sqrt(vi), high = yi + 1.96 * sqrt(vi)
  ) |>
  arrange(northern, desc(election))
fits <- read_csv("output/meta/settings.csv", show_col_types = FALSE)[1, ]
other_rows <- sum(own$northern == 0)
own <- own |> mutate(row = row_number() + if_else(northern == 1, 2, 0) + 1)
own_plot <- ggplot(own, aes(x = yi, y = row)) +
  geom_vline(xintercept = 0, colour = "grey60", linewidth = 0.4) +
  geom_errorbar(aes(xmin = low, xmax = high), orientation = "y", width = 0.2, colour = blue) +
  geom_point(colour = blue, size = 2) +
  geom_polygon(
    data = diamond(0.8, fits$other_settings, fits$other_low, fits$other_high), aes(x = x, y = y),
    fill = "grey25", inherit.aes = FALSE
  ) +
  geom_polygon(
    data = diamond(other_rows + 2.2, fits$northern, fits$northern_low, fits$northern_high), aes(x = x, y = y),
    fill = "grey25", inherit.aes = FALSE
  ) +
  scale_y_continuous(
    breaks = c(0.8, other_rows + 2.2, own$row),
    labels = c("Kerala and cities, pooled", "Northern heads, pooled", own$election)
  ) +
  labs(x = "Reserved minus open seats, graduate share (percentage points)", y = NULL) +
  theme_evidence()
save_evidence(own_plot, "output/meta/forest_settings", width = 7, height = 4.2)
