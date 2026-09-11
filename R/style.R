office_labels <- c(
  gp_head = "Village head", gp_ward = "Ward member",
  kachahari_head = "Sarpanch", kachahari_member = "Panch",
  block_member = "Block council member", zp_member = "District council member"
)

theme_evidence <- function() {
  ggplot2::theme_minimal(base_size = 11, base_family = "sans") +
    ggplot2::theme(
      panel.grid.minor = ggplot2::element_blank(),
      panel.grid.major.y = ggplot2::element_blank(),
      strip.text = ggplot2::element_text(face = "bold"),
      plot.caption = ggplot2::element_text(hjust = 0),
      plot.title.position = "plot"
    )
}

save_evidence <- function(plot, path, width, height) {
  ggplot2::ggsave(paste0(path, ".pdf"), plot, width = width, height = height)
  ggplot2::ggsave(paste0(path, ".png"), plot, width = width, height = height, dpi = 180)
}
