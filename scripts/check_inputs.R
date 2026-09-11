inputs <- readr::read_csv("evidence/analysis_inputs.csv", show_col_types = FALSE)
for (i in seq_len(nrow(inputs))) {
  path <- inputs$path[i]
  if (!file.exists(path)) stop("Missing input: ", path)
  actual <- digest::digest(path, algo = "sha256", file = TRUE)
  if (actual != inputs$sha256[i]) stop("Input changed; review before updating the manifest: ", path)
}
cat("All analysis input hashes match.\n")
