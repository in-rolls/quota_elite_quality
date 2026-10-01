# Mumbai ----

# Mumbai (BMC) councillors: does the women's quota change who gets elected?
# Reads the Praja ward-by-wave table held in the sibling local_elections
# clone (affidavit fields for the 2012 and 2017 councils; the 2007 council has
# none). The performance side of the same data lives in ../quota_governance.


normalize_educ <- function(x) str_squish(gsub("\\(.*?\\)", "", gsub("\\.", "", tolower(x))))
recode_educ_mumbai <- function(e) {
  if (is.na(e) || e == "") {
    return("Unknown")
  }
  if (str_detect(e, "^(upto )?(fourth|fifth|sixth|seventh|eighth|ninth)$")) {
    return("Below 10th")
  }
  if (str_detect(e, "^(ssc|upto ssc|matriculation|ssc, dme)$")) {
    return("10th (SSC)")
  }
  if (str_detect(e, paste0(
    "^(hsc|upto hsc|eleventh|upto twelfth|inter arts|fyjc|thirteenth|fourteenth|fybcom|fyba|",
    "sybcom|syba|ty bio -technology|under graduate|iti diploma|technical diploma|diploma in .*|",
    "dme|nctvt|d ?ed|dpharm|dhms|civil engineering)$"
  ))) {
    return("11th to some college / diploma")
  }
  if (str_detect(e, paste0(
    "^(b ?com|bcom.*|ba|ba.*|bsc.*|graduate|be.*|barch|bams|bums|bhms|bafa|tybcom|bms.*|",
    "ba llb|bcom, llb|bachelor of dental surgery|lceh)$"
  ))) {
    return("Bachelor's")
  }
  if (str_detect(e, "^(post graduate|ma|mms|mbbs|md.*|phd)$")) {
    return("Master's / professional")
  }
  "Other"
}

prepare_mumbai <- function() {
  ratings_path <- Sys.getenv(
    "MUMBAI_RATINGS",
    "../local_elections/data/maharashtra/release/mumbai/praja_ward_ratings_2011_2018.csv"
  )
  ratings_sha256 <- "2f4bbbadd1783e84e57d55bce8a29764f37e89c5e40b0b8522fe0d4bc2cc1032"
  got <- digest::digest(ratings_path, algo = "sha256", file = TRUE)
  if (got != ratings_sha256) stop("ratings file differs from the pinned SHA-256: ", got)

  d <- read_csv(ratings_path, show_col_types = FALSE, guess_max = 2000) |>
    mutate(
      council = as.integer(council),
      quota = as.integer(woman_reserved),
      female = as.integer(councillor_woman),
      adminward = factor(adminward),
      any_criminal = as.integer(councillor_criminal_cases > 0),
      # A tax ID (PAN) marks participation in the formal economy; "not given" means none was declared.
      no_pan = case_when(
        tolower(councillor_pan_card) == "yes" ~ 0L,
        tolower(councillor_pan_card) %in% c("no", "not given") ~ 1L,
        TRUE ~ NA_integer_
      )
    )

  # The deposit's quota flag: 76 women's seats in the 2007 council (one third),
  # 114 from 2012 (one half). Wrong counts would mean a miscoded treatment.
  counts <- d |>
    group_by(council, survey_year) |>
    summarise(quota = sum(quota), .groups = "drop")
  print(counts)
  stopifnot(all(counts$quota[counts$council == 2007] == 76))
  stopifnot(all(counts$quota[counts$council >= 2012] == 114))

  d <- d |> mutate(
    educ5 = map_chr(normalize_educ(councillor_education), recode_educ_mumbai),
    educ_hs_or_less = as.integer(educ5 %in% c("Below 10th", "10th (SSC)")),
    educ_grad_plus = as.integer(educ5 %in% c("Bachelor's", "Master's / professional"))
  )
  cat("\nEducation strings not matched (should be empty):\n")
  print(d |> filter(educ5 == "Other") |> count(councillor_education, sort = TRUE))

  # One row per councillor spell: affidavit fields repeat across the waves of a term.
  cand <- d |>
    filter(!is.na(councillor_age)) |>
    group_by(councillor_spell_id) |>
    slice_min(survey_year, n = 1, with_ties = FALSE) |>
    ungroup()

  cand
}

run_mumbai <- function(cand = prepare_mumbai()) {
  quality_tab <- cand |>
    mutate(seat = if_else(quota == 1, "Reserved for women", "Open")) |>
    group_by(council, seat) |>
    summarise(
      n = n(),
      `share female` = mean(female),
      `share HS or less` = mean(educ_hs_or_less[educ5 != "Unknown"]),
      `share graduate+` = mean(educ_grad_plus[educ5 != "Unknown"]),
      `mean age` = mean(councillor_age, na.rm = TRUE),
      `share any criminal case` = mean(any_criminal, na.rm = TRUE),
      `share no PAN declared` = mean(no_pan, na.rm = TRUE),
      `mean criminal cases` = mean(councillor_criminal_cases, na.rm = TRUE),
      .groups = "drop"
    )

  quality_reg <- map_dfr(
    c("educ_hs_or_less", "educ_grad_plus", "councillor_age", "any_criminal", "no_pan"),
    function(y) {
      m <- feols(as.formula(paste(y, "~ quota | adminward + council")), data = cand, cluster = ~ward_no)
      ci <- as.numeric(confint(m, "quota"))
      tibble(
        outcome = y, coef_quota = coef(m)[["quota"]], se = se(m)[["quota"]],
        conf_low = ci[1], conf_high = ci[2],
        p = pvalue(m)[["quota"]], n = nobs(m), clusters = n_distinct(cand$ward_no)
      )
    }
  )

  list(analysis = cand, descriptive = quality_tab, estimates = quality_reg)
}

# Delhi ----

delhi_2012_roster <- function() {
  # Delhi SEC notification, 27 January 2012, pp. 4-8 and Annexure III.
  # Within corporation and caste category, alternate sorted wards start with women.
  sc <- c(
    3, 5, 9, 16, 17, 26, 31, 35, 37, 42, 46, 64, 65, 71, 74, 82, 87, 92, 95, 151,
    104, 123, 130, 133, 140, 143, 156, 167, 171, 175, 179, 182, 194, 200, 203,
    210, 213, 218, 226, 233, 237, 243, 245, 255, 262, 265
  )
  tibble::tibble(
    ward_number = 1:272,
    corporation = dplyr::case_when(
      ward_number %in% c(1:100, 149:152) ~ "North",
      ward_number >= 209 ~ "East", TRUE ~ "South"
    ),
    caste_reservation = ifelse(ward_number %in% sc, "SC", "NONE")
  ) |>
    dplyr::group_by(corporation, caste_reservation) |>
    dplyr::mutate(quota = as.integer(dplyr::row_number() %% 2 == 1)) |>
    dplyr::ungroup()
}

delhi_education <- function(x) {
  x <- tolower(trimws(x))
  degrees <- c("graduate", "graduate professional", "post graduate", "doctorate")
  below <- c("illiterate", "literate", "5th pass", "5th class", "6th class", "8th pass", "10th pass", "12th pass")
  classified <- x %in% c(degrees, below)
  tibble::tibble(
    graduate_plus = ifelse(classified, as.integer(x %in% degrees), NA_integer_),
    illiterate = ifelse(classified, as.integer(x == "illiterate"), NA_integer_)
  )
}

delhi_integer <- function(x) {
  valid <- !is.na(x) & grepl("^[0-9]+$", trimws(x))
  result <- rep(NA_integer_, length(x))
  result[valid] <- as.integer(trimws(x[valid]))
  result
}

prepare_delhi <- function(raw, year) {
  stopifnot(year %in% c(2012, 2017))
  raw$source_row <- seq_len(nrow(raw))
  raw <- raw[which(tolower(trimws(raw[["Election Outcome"]])) == "winner"), ]
  reservation <- tolower(trimws(raw[["Reservation Status"]]))
  stopifnot(all(reservation %in% c("women", "general", "scw", "sc")))
  ward <- trimws(raw[["Ward Number"]])
  if (year == 2012) {
    roster <- delhi_2012_roster()
    matched <- match(as.integer(ward), roster$ward_number)
    stopifnot(!anyNA(matched))
    corporation <- roster$corporation[matched]
    stopifnot(all(as.integer(reservation %in% c("women", "scw")) == roster$quota[matched]))
    source_caste <- ifelse(reservation %in% c("scw", "sc"), "SC", "NONE")
    stopifnot(all(source_caste == roster$caste_reservation[matched]))
  } else {
    direction <- sub(".*-", "", ward)
    stopifnot(all(direction %in% c("N", "S", "E")), all(direction == trimws(raw$Direction)))
    corporation <- unname(c(N = "North", S = "South", E = "East")[direction])
  }
  age <- delhi_integer(raw$Age)
  age[!is.na(age) & (age < 21 | age > 100)] <- NA_integer_
  education_raw <- raw[[if (year == 2012) "Education" else "EDUCATION"]]
  cases_raw <- raw[[if (year == 2012) "Pending Criminal Cases" else "Pending Criminal Cases(Affidavit)"]]
  cases <- delhi_integer(cases_raw)
  d <- tibble::tibble(
    year = as.integer(year), source_row = raw$source_row, ward_id = paste(year, ward, sep = ":"),
    ward_number = ward, corporation = corporation,
    reservation_raw = raw[["Reservation Status"]],
    quota = as.integer(reservation %in% c("women", "scw")),
    caste_reservation = ifelse(reservation %in% c("scw", "sc"), "SC", "NONE"),
    education_raw = education_raw, age_raw = raw$Age, age = age,
    cases_raw = cases_raw, pending_cases = cases, any_criminal = as.integer(cases > 0)
  ) |>
    dplyr::bind_cols(delhi_education(education_raw))
  stopifnot(!anyDuplicated(d$ward_id))
  d
}

delhi_bounds <- function(d, outcome) {
  residual_model <- stats::lm(quota ~ factor(assembly_id) + factor(caste_reservation), data = d)
  weights <- residuals(residual_model) / sum(residuals(residual_model)^2)
  observed <- !is.na(d[[outcome]])
  known <- sum(weights[observed] * d[[outcome]][observed])
  tibble::tibble(
    outcome = outcome, lower = known + sum(pmin(weights[!observed], 0)),
    upper = known + sum(pmax(weights[!observed], 0)), missing = sum(!observed), n = nrow(d)
  )
}

prepare_delhi_winners <- function() {
  manifest <- read_csv("evidence/analysis_inputs.csv", show_col_types = FALSE)
  path <- "../local_elections/data/delhi/release/winners.parquet"
  pin <- manifest |> filter(.data$path == .env$path)
  stopifnot(nrow(pin) == 1, digest::digest(path, algo = "sha256", file = TRUE) == pin$sha256)
  raw <- arrow::read_parquet(path)
  d <- raw |>
    mutate(
      ward_id = paste(year, ward_number, sep = ":"), education_raw = education,
      age = if_else(age >= 21 & age <= 100, age, NA_integer_),
      any_criminal = as.integer(pending_cases > 0),
      no_earnings = no_earnings(occupation), no_earnings_social_missing = no_earnings(occupation, "missing"),
      corporation = case_when(
        year == 2012 ~ delhi_2012_roster()$corporation[match(as.integer(sub("-.*", "", ward_number)), 1:272)],
        year == 2017 ~ unname(c(N = "North", S = "South", E = "East")[sub(".*-", "", ward_number)]),
        TRUE ~ "Delhi"
      )
    ) |>
    bind_cols(delhi_education(raw$education))
  stopifnot(!anyDuplicated(d$ward_id), !anyNA(d$assembly_id))
  d
}

run_delhi <- function(d = prepare_delhi_winners()) {
  flows <- d |>
    group_by(year) |>
    summarise(
      winners = n(), quota_winners = sum(quota), classified_education = sum(!is.na(graduate_plus)),
      recorded_age = sum(!is.na(age)), recorded_cases = sum(!is.na(any_criminal)), .groups = "drop"
    )
  descriptive <- d |>
    group_by(year, quota) |>
    summarise(
      n = n(), education_n = sum(!is.na(graduate_plus)), graduate_share = mean(graduate_plus, na.rm = TRUE),
      illiterate_share = mean(illiterate, na.rm = TRUE), age_n = sum(!is.na(age)), mean_age = mean(age, na.rm = TRUE),
      cases_n = sum(!is.na(any_criminal)), any_case_share = mean(any_criminal, na.rm = TRUE), .groups = "drop"
    )
  estimates <- list()
  bounds <- list()
  for (year in sort(unique(d$year))) {
    yearly <- d |> filter(.data$year == .env$year)
    for (outcome in c(
      "graduate_plus", "illiterate", "age", "any_criminal", "no_earnings", "no_earnings_social_missing"
    )) {
      sample <- yearly |> filter(!is.na(.data[[outcome]]))
      model <- feols(as.formula(paste(outcome, "~ quota | assembly_id + caste_reservation")),
        data = sample, cluster = ~assembly_id, fixef.rm = "none"
      )
      ci <- as.numeric(confint(model, "quota"))
      estimates[[length(estimates) + 1L]] <- tibble(
        year = year, outcome = outcome, estimate = unname(coef(model)["quota"]),
        se = unname(se(model)["quota"]), conf_low = ci[1], conf_high = ci[2],
        p = unname(pvalue(model)["quota"]), n = nobs(model), clusters = n_distinct(sample$assembly_id),
        geography = "assembly_id", caste_controls = TRUE
      )
      if (outcome %in% c("graduate_plus", "any_criminal")) {
        bounds[[length(bounds) + 1L]] <- delhi_bounds(yearly, outcome) |> mutate(year = year, .before = 1)
      }
    }
  }

  legacy <- bind_rows(lapply(c(2012, 2017), function(year) {
    raw <- read_csv(paste0("../local_elections/data/delhi/raw/delhi_", year, "_final.csv"),
      col_types = cols(.default = col_character()), name_repair = "minimal"
    )
    prepare_delhi(raw, year)
  }))
  comparison <- legacy |>
    select(year, ward_number,
      legacy_education = education_raw,
      legacy_graduate = graduate_plus, legacy_cases = pending_cases
    ) |>
    left_join(d |> select(year, ward_number, education_raw, graduate_plus, pending_cases, source_url),
      by = c("year", "ward_number"), relationship = "one-to-one"
    ) |>
    mutate(graduate_disagreement = legacy_graduate != graduate_plus)
  list(
    analysis = d, flow = flows, descriptive = descriptive,
    estimates = bind_rows(estimates), bounds = bind_rows(bounds), source_comparison = comparison,
    education_labels = count(d, year, education_raw, graduate_plus, illiterate),
    source_links = select(d, year, ward_number, source_url, source_capture, source_sha256)
  )
}

# Replication deposit comparisons ----

# Compare the councillor qualifications in Karekurve-Ramachandra and Lee's replication deposits with
# the winners' own MyNeta affidavit summaries. Delhi uses the AJPS deposit (doi:10.7910/DVN/0QOVCH);
# Mumbai uses the CPS deposit (doi:10.7910/DVN/IO9SLQ) mirrored in local_elections.

pinned <- function(path, sha256) {
  got <- digest::digest(path, algo = "sha256", file = TRUE)
  if (got != sha256) stop(path, " differs from its pinned SHA-256: ", got)
  path
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

compare_deposits <- function() {
  deposit_path <- pinned(
    "../local_elections/data/delhi/interim/qualification_audit/sources/varun_datasetfull.tab",
    "b003eafd768714efce7b4f4aa66033fb4cafd3c00a58f611ae69436546f17136"
  )
  legacy_path <- pinned(
    "../local_elections/data/delhi/raw/delhi_2012_final.csv",
    "3800f3abca2472ed723beafce58cc0eee9b65bf5cd12a2666550cddca5def8ca"
  )
  delhi_myneta_path <- pinned(
    "../local_elections/data/delhi/interim/qualification_audit/raw/winners_2012.json.gz",
    "a4fb17404d627cf9ffe44825a8a14b75aa2b811147234f184f5e8ead574b25a9"
  )

  # The deposit's 2012 rows carry no ward number, so winners join the legacy file on votes and party.
  deposit <- read_tsv(deposit_path, show_col_types = FALSE) |>
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

  mumbai_deposit_path <- pinned(
    "../local_elections/data/maharashtra/raw/mumbai/dataverse_IO9SLQ/mumbai_full.tab",
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
  mumbai_myneta <- myneta_winners("https://www.myneta.info/bmc2012/index.php?action=show_winners&sort=default")
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
  list(
    agreement = audit, row_offsets = offsets,
    education_crosswalk = graded |> count(legacy_education, education, name = "winners")
  )
}
