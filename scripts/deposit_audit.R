# Compare the councillor qualifications in Karekurve-Ramachandra and Lee's replication deposits with
# the winners' own MyNeta affidavit summaries. Delhi uses the AJPS deposit (doi:10.7910/DVN/0QOVCH);
# Mumbai uses the CPS deposit (doi:10.7910/DVN/IO9SLQ) mirrored in local_elections.
suppressPackageStartupMessages({
  library(dplyr)
  library(readr)
})
dir.create("output/audit", showWarnings = FALSE, recursive = TRUE)
dir.create("data/audit", showWarnings = FALSE, recursive = TRUE)

pinned <- function(path, sha256) {
  got <- digest::digest(path, algo = "sha256", file = TRUE)
  if (got != sha256) stop(path, " differs from its pinned SHA-256: ", got)
  path
}
fetch <- function(url, path, sha256) {
  if (!file.exists(path)) utils::download.file(url, path, mode = "wb", quiet = TRUE)
  pinned(path, sha256)
}

# MyNeta's winner tables list serial, name, constituency, party, cases, education, assets and liabilities.
myneta_winners <- function(html) {
  rows <- xml2::xml_find_all(xml2::read_html(html), "//tr[td]")
  cells <- lapply(rows, function(r) trimws(xml2::xml_text(xml2::xml_find_all(r, "td"))))
  cells <- Filter(function(x) length(x) >= 7 && grepl("^[0-9]+$", x[1]), cells)
  tibble(
    constituency = vapply(cells, `[`, "", 3), cases = vapply(cells, `[`, "", 5),
    education = vapply(cells, `[`, "", 6), assets = vapply(cells, `[`, "", 7)
  ) |>
    mutate(
      cases = suppressWarnings(as.integer(cases)),
      assets = suppressWarnings(as.numeric(gsub("[^0-9]", "", sub("~.*", "", assets))))
    )
}

ladder <- c(
  "Illiterate", "Literate", "5th Pass", "8th Pass", "10th Pass", "12th Pass",
  "Graduate", "Graduate Professional", "Post Graduate", "Doctorate"
)
graduate <- function(x) ifelse(x %in% ladder, as.integer(match(x, ladder) >= 7), NA_integer_)

# Agreement expected if the deposit's values were assigned to winners at random.
shuffled_agreement <- function(a, b, draws = 2000) {
  set.seed(20261001)
  mean(replicate(draws, mean(a == sample(b))))
}

deposit_path <- fetch(
  "https://dataverse.harvard.edu/api/access/datafile/3645819?format=original",
  "data/audit/karekurve_lee_ajps_datasetfull.dta",
  "3a6fe9a81ee1d9b7d2d029ba19012fcd95b451068ec1209e2e3aa77103e2935e"
)
legacy_path <- pinned(
  "../local_elections/data/delhi/delhi_2012_final.csv",
  "3800f3abca2472ed723beafce58cc0eee9b65bf5cd12a2666550cddca5def8ca"
)
delhi_myneta_path <- pinned(
  "../local_elections/data/delhi/qualification_audit/raw/winners_2012.json.gz",
  "a4fb17404d627cf9ffe44825a8a14b75aa2b811147234f184f5e8ead574b25a9"
)

# The deposit's 2012 rows carry no ward number, so winners join the legacy file on votes and party.
deposit <- haven::read_dta(deposit_path) |>
  haven::zap_labels() |>
  filter(year == 2012, electionoutcome == "Winner") |>
  transmute(votes = as.integer(votespolled), party, edu_recoded, deposit_assets = totalassets)
legacy <- read_csv(legacy_path, show_col_types = FALSE) |>
  filter(`Election Outcome` == "Winner") |>
  transmute(ward = `Ward Number`, votes = as.integer(`Votes Polled`), party = Party, legacy_education = Education)
myneta <- myneta_winners(jsonlite::fromJSON(gzfile(delhi_myneta_path))$body) |>
  mutate(ward = as.integer(sub("^WARD\\s*([0-9]+).*", "\\1", constituency)))
unique_key <- function(d) {
  d |>
    add_count(votes, party) |>
    filter(n == 1) |>
    select(-n)
}
delhi <- unique_key(deposit) |>
  inner_join(unique_key(legacy), by = c("votes", "party"), relationship = "one-to-one") |>
  inner_join(myneta, by = "ward", relationship = "one-to-one") |>
  mutate(myneta_rank = match(education, ladder) - 1L)
stopifnot(nrow(delhi) >= 200)

# The deposit's five-level code is a recode of the legacy labels; check that before comparing.
recode_map <- delhi |>
  filter(!is.na(edu_recoded)) |>
  distinct(legacy_education, edu_recoded)
stopifnot(!anyDuplicated(recode_map$legacy_education))

ranked <- delhi |> filter(!is.na(edu_recoded), !is.na(myneta_rank))
graded <- delhi |>
  mutate(deposit_graduate = graduate(legacy_education), myneta_graduate = graduate(education)) |>
  filter(!is.na(deposit_graduate), !is.na(myneta_graduate))
valued <- delhi |> filter(!is.na(deposit_assets), !is.na(assets))

# The live page carries changing markup, so the parsed winners table is pinned rather than the file.
mumbai_myneta_path <- "data/audit/myneta_bmc2012_winners.html"
if (!file.exists(mumbai_myneta_path)) {
  utils::download.file(
    "https://www.myneta.info/bmc2012/index.php?action=show_winners&sort=default", mumbai_myneta_path,
    quiet = TRUE
  )
}
mumbai_deposit_path <- pinned(
  "../local_elections/data/maharashtra/mumbai/dataverse_IO9SLQ/mumbai_full.tab",
  "84e22d9a14809a17a8ce0af5c4e65d0c27250bebc9b6261a813607ead9e17dbb"
)
# Survey waves 2013 to 2016 rate the council elected in 2012; keep one row per ward.
mumbai_deposit <- read_tsv(mumbai_deposit_path, show_col_types = FALSE, guess_max = 5000) |>
  filter(year %in% 2013:2016, !is.na(education_level)) |>
  arrange(year) |>
  distinct(ward, .keep_all = TRUE) |>
  transmute(
    ward = as.integer(ward), deposit_cases = as.integer(no_of_criminal_cases),
    deposit_graduate = case_when(
      grepl("appeared|part|^[FST]\\.?Y\\.|F\\.Y\\.J", education_level, ignore.case = TRUE) ~ 0L,
      grepl("^(B|M|L|D)\\.|^B\\s|graduate|engineering|diploma in medical|L\\.C\\.E\\.H", education_level,
        ignore.case = TRUE
      ) ~ 1L,
      TRUE ~ 0L
    )
  )
mumbai_myneta <- myneta_winners(readr::read_file(mumbai_myneta_path))
table_hash <- digest::digest(mumbai_myneta, algo = "sha256")
bmc_table_sha256 <- "ec48c59bea6034b8ee4b96bf3b3398b4eedabe5d2c19a75372061d730a3a084e"
if (table_hash != bmc_table_sha256) stop("MyNeta BMC 2012 winners table changed: ", table_hash)
mumbai <- mumbai_myneta |>
  mutate(
    ward = as.integer(sub("^\\(([0-9]+).*", "\\1", constituency)),
    myneta_graduate = graduate(education)
  ) |>
  inner_join(mumbai_deposit, by = "ward", relationship = "one-to-one")
mumbai_graded <- mumbai |> filter(!is.na(myneta_graduate))
mumbai_cased <- mumbai |> filter(!is.na(cases), !is.na(deposit_cases))

audit <- bind_rows(
  tibble(
    deposit = "AJPS (Delhi 2012)", field = "education rank",
    n = nrow(ranked), agreement = mean(ranked$edu_recoded == pmin(ranked$myneta_rank, 4)),
    shuffled = NA_real_, spearman = cor(ranked$edu_recoded, ranked$myneta_rank, method = "spearman")
  ),
  tibble(
    deposit = "AJPS (Delhi 2012)", field = "graduate", n = nrow(graded),
    agreement = mean(graded$deposit_graduate == graded$myneta_graduate),
    shuffled = shuffled_agreement(graded$deposit_graduate, graded$myneta_graduate), spearman = NA_real_
  ),
  tibble(
    deposit = "AJPS (Delhi 2012)", field = "declared assets", n = nrow(valued),
    agreement = mean(valued$deposit_assets == valued$assets), shuffled = NA_real_,
    spearman = cor(valued$deposit_assets, valued$assets, method = "spearman")
  ),
  tibble(
    deposit = "CPS (Mumbai 2012)", field = "graduate", n = nrow(mumbai_graded),
    agreement = mean(mumbai_graded$deposit_graduate == mumbai_graded$myneta_graduate),
    shuffled = shuffled_agreement(mumbai_graded$deposit_graduate, mumbai_graded$myneta_graduate),
    spearman = NA_real_
  ),
  tibble(
    deposit = "CPS (Mumbai 2012)", field = "pending cases", n = nrow(mumbai_cased),
    agreement = mean(mumbai_cased$deposit_cases == mumbai_cased$cases), shuffled = NA_real_,
    spearman = cor(mumbai_cased$deposit_cases, mumbai_cased$cases, method = "spearman")
  )
)
# If the 2012 labels were attached to rows a fixed distance away, some offset would match the profiles.
all_rows <- read_csv(legacy_path, show_col_types = FALSE)
winner_rows <- which(all_rows$`Election Outcome` == "Winner")
profile <- myneta$education[match(all_rows$`Ward Number`[winner_rows], myneta$ward)]
offsets <- tibble(offset = -40:40) |>
  mutate(exact = vapply(offset, function(k) {
    rows <- winner_rows + k
    keep <- rows >= 1 & rows <= nrow(all_rows) & !is.na(profile)
    mean(tolower(all_rows$Education[rows[keep]]) == tolower(profile[keep]), na.rm = TRUE)
  }, 0))
write_csv(offsets, "output/audit/delhi_2012_row_offsets.csv")

write_csv(audit, "output/audit/deposit_agreement.csv")
write_csv(
  graded |> count(legacy_education, education, name = "winners"),
  "output/audit/delhi_2012_education_crosswalk.csv"
)
print(audit)
