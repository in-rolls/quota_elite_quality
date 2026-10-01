# Bihar ----

education_outcomes <- function(x) {
  x <- tolower(trimws(x))
  known <- c(
    "illiterate", "literate", "primary", "middle", "higher", "inter",
    "graduate", "post graduate"
  )
  unexpected <- setdiff(unique(na.omit(x)), c(known, "--", "others", ""))
  if (length(unexpected)) stop("Unreviewed education labels: ", paste(unexpected, collapse = ", "))
  data.frame(
    education_known = x %in% known,
    illiterate = ifelse(x %in% known, as.integer(x == "illiterate"), NA_integer_),
    graduate_plus = ifelse(x %in% known, as.integer(x %in% c("graduate", "post graduate")), NA_integer_)
  )
}

# The 2016 release derives each winner (top vote, uncontested, or a tie drawn by lot) and
# names none where the source cannot decide: a serial repeated with different votes, a tie
# with no lot mark, or several uncontested candidates. Those seats stay without a winner.
# In 2021 a seat's only nominee has no result record, so no candidate row is flagged;
# `sole` names those seats, and their one candidate is the winner.
select_winners <- function(candidates, sole = character()) {
  filter(
    candidates, coalesce(elected == "1", FALSE) | row_id %in% sole,
    !is.na(candidate_name), nzchar(trimws(candidate_name))
  )
}

# 2021 records no candidate education, so that election contributes age alone.
ELECTIONS <- list(
  "2016" = list(
    candidates = 644537L, seats = 227317L,
    outcomes = c("graduate_plus", "illiterate", "age")
  ),
  "2021" = list(candidates = 924708L, seats = 247671L, outcomes = "age")
)

prepare_bihar <- function(year = "2016") {
  election <- ELECTIONS[[year]]
  source_dir <- Sys.getenv("LOCAL_ELECTIONS_MASTER", "../local_elections/data/master")
  paths <- file.path(source_dir, c("candidates_bihar.parquet", "master_bihar.parquet"))
  candidates <- read_parquet(paths[1]) |>
    as_tibble() |>
    filter(.data$year == as.integer(.env$year))
  seats <- read_parquet(paths[2]) |>
    as_tibble() |>
    filter(.data$year == as.integer(.env$year))
  stopifnot(
    nrow(candidates) == election$candidates, nrow(seats) == election$seats,
    !anyDuplicated(candidates$candidate_id), !anyDuplicated(seats$row_id),
    all(candidates$row_id %in% seats$row_id)
  )

  sole <- seats$row_id[seats$winner_basis %in% "sole_candidate"]
  stopifnot(all(table(candidates$row_id[candidates$row_id %in% sole]) == 1L))
  winners <- select_winners(candidates, sole)
  stopifnot(!anyDuplicated(winners$row_id))
  winners <- winners |>
    left_join(
      select(seats, row_id, selection_basis = winner_basis),
      by = "row_id", relationship = "one-to-one"
    ) |>
    mutate(
      quota = as.integer(woman_reserved),
      age = suppressWarnings(as.numeric(candidate_age)),
      age = if_else(age >= 21 & age <= 100, age, NA_real_),
      block_id = interaction(district, block, drop = TRUE, lex.order = TRUE),
      caste_reservation = factor(caste_reservation)
    )
  stopifnot(!anyNA(winners$selection_basis))
  winners <- bind_cols(winners, education_outcomes(winners$candidate_education))
  flow <- seats |>
    select(row_id, tier, woman_reserved) |>
    left_join(winners |> select(row_id, selection_basis, education_known, age),
      by = "row_id", relationship = "one-to-one"
    ) |>
    group_by(tier, woman_reserved) |>
    summarise(
      source_seats = n(), identified_winners = sum(!is.na(selection_basis)),
      uncontested_winners = sum(selection_basis %in% c("uncontested", "sole_candidate")),
      lot_winners = sum(selection_basis == "lot", na.rm = TRUE),
      education_unknown = sum(!education_known, na.rm = TRUE),
      age_missing = sum(!is.na(selection_basis) & is.na(age)), .groups = "drop"
    )
  analysis <- winners |>
    filter(!is.na(quota), !is.na(caste_reservation)) |>
    select(
      row_id, tier, district, block, block_id, caste_reservation, quota,
      selection_basis, education_known, illiterate, graduate_plus, age
    )
  list(
    analysis = analysis, flow = flow,
    education_labels = count(winners, candidate_education, education_known, sort = TRUE)
  )
}

run_bihar <- function(year = "2016", prepared = prepare_bihar(year)) {
  election <- ELECTIONS[[year]]
  analysis <- prepared$analysis
  descriptive <- analysis |>
    group_by(tier, quota) |>
    summarise(
      n = n(), education_n = sum(education_known), age_n = sum(!is.na(age)),
      illiterate = mean(illiterate, na.rm = TRUE),
      graduate_plus = mean(graduate_plus, na.rm = TRUE),
      age = mean(age, na.rm = TRUE), .groups = "drop"
    )

  results <- list()
  for (office in unique(as.character(analysis$tier))) {
    for (geography in c("district", if (office != "zp_member") "block_id")) {
      for (outcome in election$outcomes) {
        d <- analysis |> filter(tier == office, !is.na(.data[[outcome]]), !is.na(.data[[geography]]))
        model <- feols(
          as.formula(paste(outcome, "~ quota |", geography, "+ caste_reservation")),
          data = d, vcov = as.formula(paste("~", geography)),
          fixef.rm = "none", notes = FALSE
        )
        interval <- as.numeric(unlist(confint(model, "quota")))
        results[[length(results) + 1L]] <- tibble(
          tier = office, geography = geography, outcome = outcome,
          estimate = unname(coef(model)["quota"]), se = unname(se(model)["quota"]),
          conf_low = interval[1], conf_high = interval[2], p = unname(pvalue(model)["quota"]),
          n = nobs(model), clusters = n_distinct(d[[geography]]),
          primary = geography == ifelse(office == "zp_member", "district", "block_id")
        )
      }
    }
  }
  results <- bind_rows(results)
  c(prepared, list(descriptive = descriptive, estimates = results))
}

# Uttar Pradesh, Rajasthan and Kerala ----

master_dir <- function() Sys.getenv("LOCAL_ELECTIONS_MASTER", "../local_elections/data/master")

read_seats <- function(state) {
  path <- file.path(master_dir(), paste0("master_", state, ".parquet"))
  d <- read_parquet(path) |> as_tibble()
  stopifnot(!anyDuplicated(d$row_id))
  d
}

winner_extras <- function(seats) {
  d <- read_parquet(file.path(master_dir(), "master_extras.parquet")) |>
    filter(row_id %in% seats$row_id, column %in% c(
      "winner_education", "winner_age", "winner_occupation",
      "movable_property", "immovable_property", "criminal_history"
    ))
  stopifnot(!anyDuplicated(paste(d$row_id, d$column)))
  d <- d |> pivot_wider(names_from = column, values_from = value)
  for (col in c("winner_occupation", "movable_property", "immovable_property", "criminal_history")) {
    if (!col %in% names(d)) d[[col]] <- NA_character_
  }
  d
}

# UP 2021 reports movable and immovable property separately; Rajasthan reports one total. Both are
# declared rupee holdings, logged after adding one so zero holdings stay in the sample.
economic_outcomes <- function(d) {
  total <- if ("total_assets" %in% names(d)) {
    suppressWarnings(as.numeric(d$total_assets))
  } else {
    rep(NA_real_, nrow(d))
  }
  split <- suppressWarnings(as.numeric(d$movable_property) + as.numeric(d$immovable_property))
  assets <- coalesce(total, split)
  d |> mutate(
    no_earnings = no_earnings(winner_occupation),
    no_earnings_social_missing = no_earnings(winner_occupation, social_work = "missing"),
    log_assets = if_else(assets >= 0, log1p(assets), NA_real_),
    criminal_record = case_when(
      criminal_history == "हाँ" ~ 1L, criminal_history == "नहीं" ~ 0L, TRUE ~ NA_integer_
    )
  )
}

schooling <- function(x, state) {
  label <- str_squish(str_to_lower(x))
  graduate <- rep(NA_integer_, length(label))
  rule <- rep("unclassified", length(label))
  if (state == "uttar_pradesh") {
    high <- c("graduate", "postgraduate", "स्नातक", "परास्नातक", "पी० एच० डी०")
    low <- c(
      "illiterate", "literate", "primary", "junior high school", "high school", "intermediate",
      "निरक्षर", "प्राईमरी", "जूनियर हाईस्कूल", "हाईस्कूल", "इंटर"
    )
    graduate[label %in% high] <- 1L
    graduate[label %in% low] <- 0L
    illiterate <- ifelse(!is.na(graduate), as.integer(label %in% c("illiterate", "निरक्षर")), NA_integer_)
  } else if (state == "rajasthan") {
    high <- c("graduate", "postgraduate", "professional graduate", "professional post graduate")
    low <- c("5th", "8th", "secondary", "higher secondary", "literate", "illiterate")
    graduate[label %in% high] <- 1L
    graduate[label %in% low] <- 0L
    illiterate <- ifelse(!is.na(graduate), as.integer(label == "illiterate"), NA_integer_)
  } else {
    clean <- str_squish(str_replace_all(str_replace_all(label, "[,-]", " "), "[.]", ""))
    degrees <- paste0(
      "(^|[^a-z])(ba|b a|bcom|b com|bsc|b sc|btech|b tech|be|b e|bba|bca|bpharm|b pharm|bpt|",
      "bed|b ed|llb|l l b|llm|l l m|mbbs|bams|bhms|bums|ma|m a|msc|m sc|mcom|m com|mba|mtech|m tech|msw|",
      "mca|phd|graduate|graduation|degree|postgraduate)([^a-z]|$)"
    )
    school <- paste0(
      "(^|[^a-z])(sslc|s s l c|s s lc|s sl c|ss lc|ssc|hsc|vhse|v h s e|vhsc|puc|iti|itc|ttc|",
      "t t c|diploma|pdc|p d c|pree?.?degree|pree?.?digree|plus ?(one|two|2)|plustwo|",
      "high school|higher secondary|primary|literate|illiterate|seventh|eighth|ninth|nineth|",
      "tenth|fourth|fifth|sixth|vii|viii|ix|iv|vi|v|x)([^a-z]|$)|^[1-9] ?(st|nd|rd|th)?( std|",
      " standard| class| pass)?$|^1[012] ?(th)?( std| standard| class| pass)?$|^\\+2$"
    )
    low <- !is.na(clean) & str_detect(clean, school)
    high <- !is.na(clean) & str_detect(clean, degrees)
    # Pre-degree is school; incomplete degrees remain unclassified.
    high <- high & !str_detect(clean, "pree?.?degree|pree?.?digree")
    incomplete <- !is.na(clean) & str_detect(clean, paste0(
      "fail|incomplete|pursuing|studying|ongoing|under.?graduate|not complet|",
      "course complet|discont|[123] ?year|first year|second year|final year"
    ))
    postgrad_diploma <- !is.na(clean) & str_detect(clean, "p ?g ?diploma")
    graduate[low & !high & !postgrad_diploma] <- 0L
    graduate[high & !incomplete] <- 1L
    illiterate <- rep(NA_integer_, length(label))
  }
  rule[!is.na(graduate)] <- "recognized qualification"
  rule[is.na(label) | label %in% c("", "--", "other", "others", "unknown", "nil")] <- "missing or unspecified"
  tibble(education_raw = x, graduate_plus = graduate, illiterate = illiterate, education_rule = rule)
}

prepare_controls <- function(d, state) {
  d |> mutate(
    quota = as.integer(woman_reserved),
    age = suppressWarnings(as.numeric(winner_age)),
    age = if_else(age >= 21 & age <= 100, age, NA_real_),
    caste_reservation = na_if(as.character(caste_reservation), ""),
    district = na_if(str_squish(district), ""), block = na_if(str_squish(block), ""),
    block_id = if_else(!is.na(district) & !is.na(block), paste(district, block, sep = ":"), NA_character_),
    ambiguous = str_detect(
      coalesce(quality_flags, ""),
      "winner_candidate_ambiguous|winner_markers_conflict|serial_not_unique"
    ),
    treatment_known = !is.na(quota) & !str_detect(
      coalesce(quality_flags, ""), "gender_not_stated|reservation_control_disagree"
    ),
    winner_known = !is.na(winner) & nzchar(trimws(winner)),
    state_key = state
  )
}

prepare_rural <- function(state) {
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
  if (state == "rajasthan") {
    assets <- read_parquet(file.path(master_dir(), "candidates_rajasthan.parquet")) |>
      filter(year == 2020, elected == "1") |>
      select(row_id, total_assets = candidate_total_assets)
    stopifnot(!anyDuplicated(assets$row_id))
    d <- left_join(d, assets, by = "row_id", relationship = "one-to-one")
  }
  stopifnot(nrow(d) == nrow(seats))
  d <- prepare_controls(d, state)
  d <- bind_cols(d, schooling(d$winner_education, state)) |> economic_outcomes()
  flow <- d |>
    group_by(year, tier, quota) |>
    summarise(
      seats = n(), winners = sum(winner_known), ambiguous = sum(ambiguous),
      treatment_known = sum(treatment_known), classified_schooling = sum(!is.na(graduate_plus)),
      .groups = "drop"
    )
  analysis <- d |>
    filter(winner_known, !ambiguous, treatment_known, !is.na(caste_reservation)) |>
    select(
      row_id, year, tier, state_key, quota, district, block_id, caste_reservation,
      graduate_plus, illiterate, age, education_rule,
      no_earnings, no_earnings_social_missing, log_assets, criminal_record
    )
  stopifnot(!anyDuplicated(analysis$row_id), all(na.omit(analysis$graduate_plus) %in% 0:1))
  list(
    analysis = analysis, flow = flow,
    occupation_labels = count(d, year, tier,
      occupation = str_squish(str_to_lower(winner_occupation)), no_earnings, name = "records"
    ),
    education_labels = count(d, year, tier, education_raw, graduate_plus, education_rule, name = "records")
  )
}

fit_winners <- function(prepared, state) {
  analysis <- prepared$analysis
  desc <- analysis |>
    group_by(year, tier, quota) |>
    summarise(
      n = n(), education_n = sum(!is.na(graduate_plus)),
      graduate_plus = mean(graduate_plus, na.rm = TRUE),
      illiterate = mean(illiterate, na.rm = TRUE), age_n = sum(!is.na(age)),
      age = mean(age, na.rm = TRUE), occupation_n = sum(!is.na(no_earnings)),
      no_earnings = mean(no_earnings, na.rm = TRUE), assets_n = sum(!is.na(log_assets)),
      median_assets = median(expm1(log_assets), na.rm = TRUE), criminal_n = sum(!is.na(criminal_record)),
      criminal_record = mean(criminal_record, na.rm = TRUE), .groups = "drop"
    )
  results <- list()
  for (yr in sort(unique(analysis$year))) {
    for (office in unique(analysis$tier[analysis$year == yr])) {
      base <- filter(analysis, year == yr, tier == office)
      geography <- if (office != "zp_member" && all(!is.na(base$block_id))) "block_id" else "district"
      outcomes <- c(
        "graduate_plus", "illiterate", "age", "no_earnings", "no_earnings_social_missing",
        "log_assets", "criminal_record"
      )
      for (outcome in outcomes) {
        sample <- base |> filter(!is.na(.data[[outcome]]), !is.na(.data[[geography]]))
        if (nrow(sample) == 0 || n_distinct(sample[[outcome]]) < 2) next
        stopifnot(n_distinct(sample$quota) == 2, n_distinct(sample[[geography]]) > 1)
        m <- feols(as.formula(paste(outcome, "~ quota |", geography, "+ caste_reservation")),
          data = sample, vcov = as.formula(paste("~", geography)), fixef.rm = "none", notes = FALSE
        )
        ci <- as.numeric(confint(m, "quota"))
        results[[length(results) + 1L]] <- tibble(
          state = state, year = yr, tier = office, geography = geography, outcome = outcome,
          estimate = unname(coef(m)["quota"]), se = unname(se(m)["quota"]),
          conf_low = ci[1], conf_high = ci[2], p = unname(pvalue(m)["quota"]),
          n = nobs(m), clusters = n_distinct(sample[[geography]])
        )
      }
    }
  }
  results <- bind_rows(results)
  c(prepared, list(descriptive = desc, estimates = results))
}

missing_education_bounds <- function(d, specs, state) {
  results <- list()
  specs <- filter(specs, outcome == "graduate_plus")
  for (i in seq_len(nrow(specs))) {
    spec <- specs[i, ]
    sample <- d |>
      filter(year == spec$year, tier == spec$tier, !is.na(.data[[spec$geography]]))
    residual_model <- feols(as.formula(paste("quota ~ 1 |", spec$geography, "+ caste_reservation")),
      data = sample, fixef.rm = "none", notes = FALSE
    )
    weights <- resid(residual_model) / sum(resid(residual_model)^2)
    observed <- !is.na(sample$graduate_plus)
    known <- sum(weights[observed] * sample$graduate_plus[observed])
    results[[length(results) + 1L]] <- tibble(
      state = state, year = spec$year, tier = spec$tier,
      classified_sample_estimate = spec$estimate,
      all_winners_lower = known + sum(pmin(weights[!observed], 0)),
      all_winners_upper = known + sum(pmax(weights[!observed], 0)),
      all_winners_n = nrow(sample), unclassified = sum(!observed)
    )
  }
  bind_rows(results)
}
