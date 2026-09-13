master_directory <- Sys.getenv("RESERVATIONS_MASTER", "../local_elections/data/master")
inputs <- readr::read_csv("evidence/analysis_inputs.csv", show_col_types = FALSE)
for (i in seq_len(nrow(inputs))) {
  path <- inputs$path[i]
  if (grepl("/data/master/", path, fixed = TRUE)) {
    path <- file.path(master_directory, basename(path))
  }
  if (!file.exists(path)) stop("Missing input: ", path)
  actual <- digest::digest(path, algo = "sha256", file = TRUE)
  if (actual != inputs$sha256[i]) stop("Input changed; review before updating the manifest: ", path)
}
cat("All analysis input hashes match.\n")

state_sources <- jsonlite::read_json(file.path(dirname(master_directory), "sources.json"))
for (i in which(!is.na(inputs$state_source_provider))) {
  provider <- inputs$state_source_provider[i]
  if (!identical(state_sources[[provider]]$ref, inputs$state_source_ref[i])) {
    stop("State input revision changed; review the rebuilt results: ", provider)
  }
}
cat("UP and Rajasthan source revisions match the shared pipeline.\n")
