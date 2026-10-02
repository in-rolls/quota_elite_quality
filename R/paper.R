suppressPackageStartupMessages({
  library(ggplot2)
  library(knitr)
})

office_labels <- c(
  gp_head = "Village head", gp_ward = "Ward member",
  kachahari_head = "Sarpanch", kachahari_member = "Panch",
  block_member = "Block member", zp_member = "District member"
)

office_name <- function(tier) unname(office_labels[as.character(tier)])

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

# Manuscript values and tables ----

rural <- report$rural
mumbai <- report$settings$mumbai$estimates
delhi <- report$settings$delhi$estimates
delhi_flow <- report$settings$delhi$flow
delhi_missing <- report$settings$delhi$bounds
get_delhi <- function(year, outcome = "graduate_plus") {
  row <- delhi |> filter(.data$year == .env$year, .data$outcome == .env$outcome)
  stopifnot(nrow(row) == 1)
  row
}
fmt <- function(x, digits = 1) formatC(x, format = "f", digits = digits)
# Rajasthan is left out of the head range because missing education leaves its all-winner sign open.
setting_summary <- function() {
  heads <- rural |> filter(tier == "gp_head", outcome == "graduate_plus", state %in% c("Bihar", "Uttar Pradesh"))
  kerala <- rural |> filter(state == "Kerala", tier == "gp_ward", outcome == "graduate_plus")
  cases <- c(mumbai$coef_quota[mumbai$outcome == "any_criminal"], delhi$estimate[delhi$outcome == "any_criminal"])
  list(
    head_gap = 100 * range(-heads$estimate),
    kerala_bound = 100 * max(abs(c(kerala$conf_low, kerala$conf_high))),
    case_gap = 100 * range(-cases)
  )
}
num <- function(x) format(x, big.mark = ",", scientific = FALSE, trim = TRUE)
get_result <- function(state, year, tier, outcome = "graduate_plus") {
  row <- rural |> filter(
    .data$state == .env$state, .data$year == .env$year,
    .data$tier == .env$tier, .data$outcome == .env$outcome
  )
  stopifnot(nrow(row) == 1)
  row
}
contrast <- function(row, scale = 100) {
  paste0(
    fmt(row$estimate * scale), " percentage points (95% CI ",
    fmt(row$conf_low * scale), " to ", fmt(row$conf_high * scale), ")"
  )
}
interval_text <- function(estimate, conf_low, conf_high, scale = 100) {
  paste0(fmt(estimate * scale), " [", fmt(conf_low * scale), ", ", fmt(conf_high * scale), "]")
}

education_table <- function(d, caption) {
  graduates <- d |>
    filter(outcome == "graduate_plus") |>
    arrange(year, match(tier, names(office_labels))) |>
    mutate(
      Office = office_name(tier),
      `Graduate or above` = interval_text(estimate, conf_low, conf_high),
      N = num(n), G = num(clusters),
      Controls = if_else(geography == "block_id", "Block", "District")
    ) |>
    select(year, tier, Office, `Graduate or above`, N, G, Controls)
  illiteracy <- d |>
    filter(outcome == "illiterate") |>
    mutate(Illiterate = interval_text(estimate, conf_low, conf_high)) |>
    select(year, tier, Illiterate)
  table <- graduates |>
    left_join(illiteracy, by = c("year", "tier"), relationship = "one-to-one") |>
    mutate(Illiterate = coalesce(Illiterate, "--")) |>
    select(Year = year, Office, `Graduate or above`, Illiterate, N, G, Controls)
  kable(table, caption = caption, booktabs = TRUE, row.names = FALSE, longtable = TRUE)
}

age_table <- function(d) {
  table <- d |>
    filter(outcome == "age") |>
    arrange(match(state, unique(d$state)), year, match(tier, names(office_labels))) |>
    mutate(
      Office = office_name(tier),
      `Difference [95% CI]` = interval_text(estimate, conf_low, conf_high, scale = 1),
      N = num(n), G = num(clusters)
    ) |>
    select(State = state, Year = year, Office, `Difference [95% CI]`, N, G)
  kable(table, caption = paste(
    "Age of rural officials: differences in years with pointwise 95% confidence intervals.",
    "N: winners with age recorded; G: geographic clusters. Controls match the education models."
  ), booktabs = TRUE, row.names = FALSE, longtable = TRUE)
}

delhi_comparison <- report$settings$delhi$source_comparison |>
  group_by(year) |>
  summarise(
    joint = sum(!is.na(graduate_disagreement)),
    disagreements = sum(graduate_disagreement, na.rm = TRUE), .groups = "drop"
  )

setting_meta <- report$meta$settings
combined <- report$meta$combined
combined_meta <- combined$pooled
bhavnani <- bhavnani_contrasts()
audit <- report$deposit$agreement

# Reserved-minus-open differences in reporting no occupation and in log declared assets.
economic_summary <- function() {
  occupation <- c(
    rural$estimate[rural$outcome == "no_earnings" & rural$tier %in% c("gp_head", "gp_ward", "block_member")],
    delhi$estimate[delhi$outcome == "no_earnings"]
  )
  up_crime <- get_result("Uttar Pradesh", 2021, "gp_head", "criminal_record")
  up_desc <- report$settings$uttar_pradesh$descriptive |>
    filter(year == 2021, tier == "gp_head")
  list(
    occupation_gap = 100 * range(occupation),
    rajasthan_occupation = get_result("Rajasthan", 2020, "gp_head", "no_earnings"),
    kerala_ward_occupation = 100 * range(
      rural$estimate[rural$state == "Kerala" & rural$tier == "gp_ward" & rural$outcome == "no_earnings"]
    ),
    delhi_occupation = 100 * range(delhi$estimate[delhi$outcome == "no_earnings"]),
    rajasthan_assets = 100 * (exp(get_result("Rajasthan", 2020, "gp_head", "log_assets")$estimate) - 1),
    up_assets = 100 * (exp(get_result("Uttar Pradesh", 2021, "gp_head", "log_assets")$estimate) - 1),
    up_crime = up_crime,
    social_shift = 100 * max(abs(c(
      rural$estimate[rural$outcome == "no_earnings_social_missing"] -
        rural$estimate[rural$outcome == "no_earnings"],
      delhi$estimate[delhi$outcome == "no_earnings_social_missing"] - delhi$estimate[delhi$outcome == "no_earnings"]
    ))),
    kerala_district_occupation = 100 * range(
      rural$estimate[rural$state == "Kerala" & rural$tier == "zp_member" & rural$outcome == "no_earnings"]
    ),
    up_crime_rate = 100 * weighted.mean(up_desc$criminal_record, up_desc$criminal_n)
  )
}

economic_table <- function() {
  outcomes <- c(
    no_earnings = "No occupation", no_pan = "No tax ID declared", log_assets = "Log declared assets",
    criminal_record = "Criminal record"
  )
  rows <- bind_rows(
    mumbai |>
      filter(outcome == "no_pan") |>
      mutate(state = "Mumbai", year = "2012, 2017", Office = "Councillor", estimate = coef_quota),
    rural |>
      filter(outcome %in% names(outcomes)) |>
      mutate(Office = office_name(tier), year = as.character(year)),
    delhi |>
      filter(outcome %in% names(outcomes)) |>
      mutate(state = "Delhi", Office = "Councillor", year = as.character(year))
  ) |>
    mutate(
      Outcome = unname(outcomes[outcome]), scale = if_else(outcome == "log_assets", 1, 100),
      digits = if_else(outcome %in% c("log_assets", "criminal_record"), 2, 1),
      `Difference [95% CI]` = sprintf(
        "%.*f [%.*f, %.*f]", digits, estimate * scale, digits, conf_low * scale, digits, conf_high * scale
      ),
      N = num(n)
    ) |>
    arrange(match(outcome, names(outcomes)), state, year) |>
    select(Outcome, State = state, Year = year, Office, `Difference [95% CI]`, N)
  kable(rows, caption = paste(
    "Occupation, assets and criminal records: reserved minus open seats, with the controls and clustering",
    "of the education models. No occupation and criminal record in percentage points; log declared assets",
    "in log points. No occupation counts nil, unemployed, homemaker and student entries. Mumbai, which records",
    "no occupation, reports whether the councillor declared a tax ID (PAN)."
  ), booktabs = TRUE, row.names = FALSE, longtable = TRUE)
}

# Figures ----

render_figures <- function(report) {
  rural <- report$rural
  bihar <- filter(rural, state == "Bihar")

  interval_plot <- function(d, title, limits = NULL) {
    ggplot(d, aes(x = estimate * 100, y = label)) +
      geom_vline(xintercept = 0, colour = "grey60", linewidth = 0.4) +
      geom_errorbar(aes(xmin = conf_low * 100, xmax = conf_high * 100),
        orientation = "y", width = 0.16, colour = "#165B85"
      ) +
      geom_point(colour = "#165B85", size = 2) +
      labs(x = "Reserved minus open seats (percentage points)", y = NULL, title = title) +
      scale_x_continuous(limits = limits) +
      theme_evidence()
  }

  heads <- rural |>
    filter(tier == "gp_head", outcome == "graduate_plus") |>
    mutate(label = factor(paste(state, year), levels = rev(paste(state, year))))
  save_evidence(
    interval_plot(heads, "Rural village heads: graduate or above"),
    "figs/rural_heads", 7, 3.4
  )
  kerala <- rural |>
    filter(state == "Kerala", outcome == "graduate_plus") |>
    mutate(
      label = factor(year, levels = rev(sort(unique(year)))),
      office = factor(office_name(tier), levels = office_labels[c("gp_ward", "block_member", "zp_member")])
    )
  save_evidence(
    interval_plot(kerala, "Kerala: graduate or above") + facet_wrap(~office, nrow = 1),
    "figs/kerala_education", 9, 3.4
  )
  bihar_plot <- bihar |>
    filter(year == 2016, outcome == "graduate_plus") |>
    mutate(
      label = factor(office_name(tier), levels = rev(unname(office_labels)))
    )
  save_evidence(
    interval_plot(bihar_plot, "Bihar 2016: graduate or above"),
    "figs/bihar_2016_education", 7, 4.2
  )
  mumbai <- report$settings$mumbai$estimates |>
    filter(outcome %in% c("educ_grad_plus", "any_criminal", "no_pan")) |>
    rename(estimate = coef_quota) |>
    mutate(label = factor(
      recode(outcome,
        educ_grad_plus = "Graduate or above",
        any_criminal = "Any pending criminal case",
        no_pan = "No tax ID declared"
      ),
      levels = c("No tax ID declared", "Any pending criminal case", "Graduate or above")
    ))
  save_evidence(
    interval_plot(mumbai, "Urban: Mumbai councillors, 2012 and 2017"),
    "figs/mumbai_quality", 7, 3.1
  )
  delhi <- report$settings$delhi$estimates |>
    filter(outcome %in% c("graduate_plus", "no_earnings", "any_criminal")) |>
    mutate(
      label = factor(year, levels = c(2022, 2017, 2012)),
      outcome = factor(outcome,
        levels = c("graduate_plus", "no_earnings", "any_criminal"),
        labels = c("Graduate or above", "No occupation", "Any pending criminal case")
      )
    )
  save_evidence(
    interval_plot(delhi, "Urban: Delhi councillors") + facet_wrap(~outcome, nrow = 1),
    "figs/delhi_quality", 9, 3.4
  )


  blue <- "#165B85"
  comparisons <- combined$inputs |>
    transmute(panel = as.character(group), label, estimate = yi, low, high, pooled = FALSE)
  summaries <- combined$groups |>
    transmute(panel = group, label = "Group summary", estimate = d, low, high, pooled = TRUE)
  overall <- combined_meta |>
    transmute(
      panel = "Combined literature and this paper", label = "Overall summary",
      estimate = d, low, high, pooled = TRUE
    )
  plot_data <- bind_rows(comparisons, summaries, overall) |>
    mutate(panel = factor(panel, levels = c(levels(combined$inputs$group), overall$panel))) |>
    group_by(panel) |>
    mutate(row = rev(row_number())) |>
    ungroup()
  # Unique labels let each panel keep its own ordering without a legend.
  plot_data <- plot_data |> mutate(position = paste(panel, row, sep = ":"))
  labels <- setNames(plot_data$label, plot_data$position)
  plot_data <- plot_data |> mutate(position = factor(position, levels = rev(position)))
  synthesis_plot <- ggplot(plot_data, aes(x = estimate, y = position)) +
    geom_vline(xintercept = 0, colour = "grey60", linewidth = 0.4) +
    geom_errorbar(aes(xmin = low, xmax = high), orientation = "y", width = 0.2, colour = blue) +
    geom_point(data = filter(plot_data, !pooled), colour = blue, size = 2) +
    geom_point(data = filter(plot_data, pooled), shape = 18, size = 3, colour = "grey25") +
    facet_grid(panel ~ .,
      scales = "free_y", space = "free_y", switch = "y",
      labeller = as_labeller(c(
        "Published village heads" = "Published\nvillage heads",
        "Our northern village heads" = "This paper:\nnorthern heads",
        "Our Kerala and cities" = "This paper:\nKerala and cities",
        "Combined literature and this paper" = "Combined"
      ))
    ) +
    scale_y_discrete(labels = labels) +
    labs(x = "Schooling difference, reserved minus open seats (SD units)", y = NULL) +
    theme_evidence() +
    theme(strip.text.y.left = element_text(angle = 0), strip.placement = "outside")
  save_evidence(synthesis_plot, "figs/forest_combined", width = 8.5, height = 8.3)
}

# Literature appendix ----

render_literature_table <- function() {
  # Typeset the literature table from the study files and table specification in evidence/literature/.

  tab <- yaml::read_yaml("evidence/literature/tables.yaml")
  lit <- read_literature() |> mutate(level = if_else(office == "Municipal councillor", "municipal", "village"))

  latex_escape <- function(x) {
    x <- gsub("\\", "\\textbackslash{}", x, fixed = TRUE)
    x <- gsub("([&%$#_{}])", "\\\\\\1", x, perl = TRUE)
    gsub("(?<=\\d)-(?=\\d)", "--", x, perl = TRUE)
  }

  unit_suffix <- c(share = " (\\%)", years = " (years)", sd = "", index = "", count = "")

  value <- function(x, unit) {
    ifelse(is.na(x), "", ifelse(unit == "share", formatC(100 * x, format = "f", digits = 1), as.character(x)))
  }

  # Print a reported difference to its SE's precision so -2.620 (0.766) is not shown as -2.62.
  difference <- function(d) {
    digits <- ifelse(d$unit == "share", 1, nchar(sub("^[^.]*\\.?", "", as.character(d$se))))
    scale <- ifelse(d$unit == "share", 100, 1)
    dagger <- ifelse(d$se_source == "reported", "", "$^\\dagger$")
    minus <- function(x) sub("^-", "$-$", x)
    paste0(
      minus(mapply(formatC, scale * d$diff, format = "f", digits = digits)), " (",
      mapply(formatC, scale * d$se, format = "f", digits = digits), ")", dagger
    )
  }

  section_rows <- function(sec) {
    d <- lit |>
      filter(population == sec$population, comparison %in% unlist(sec$comparison)) |>
      filter(level == if (is.null(sec$level)) "village" else sec$level) |>
      arrange(match(key, tab$study_order))
    if (nrow(d) == 0) {
      return(character())
    }
    first <- !duplicated(d$key)
    small <- function(x) paste0(" \\newline \\textit{", latex_escape(x), "}")
    study <- ifelse(first, paste0(latex_escape(d$label), small(d$location)), "")
    setting <- ifelse(first, latex_escape(paste0(d$state, "; ", d$years, "; ", d$office)), "")
    design <- ifelse(first, latex_escape(paste0(d$design, if_else(d$adjusted, "; adjusted", ""))), "")
    outcome <- paste0(latex_escape(d$outcome), unit_suffix[d$unit])
    extra <- unlist(tab$comparison_labels)[d$comparison]
    outcome <- ifelse(is.na(extra), outcome, paste0(outcome, small(extra)))
    rows <- paste(
      study, setting, design, outcome,
      value(d$reserved, d$unit), value(d$open, d$unit), difference(d), format(d$n, big.mark = ","),
      sep = " & "
    )
    rule <- ifelse(first & seq_along(first) > 1, "\\addlinespace[3pt]\n", "")
    c(
      "\\midrule",
      sprintf("\\multicolumn{8}{l}{\\textit{%s}} \\\\", latex_escape(sec$heading)),
      "\\nopagebreak",
      "\\addlinespace[2pt]",
      paste0(rule, rows, " \\\\")
    )
  }

  widths <- c(0.15, 0.17, 0.13, 0.20, 0.06, 0.06, 0.10, 0.05)
  align <- rep(c("\\RaggedRight", "\\raggedleft"), each = 4)
  colspec <- paste(sprintf(">{%s\\arraybackslash}p{%.2f\\linewidth}", align, widths), collapse = "")
  notes <- latex_escape(paste(unlist(tab$notes), collapse = " "))
  headers <- paste(
    "\\textbf{Study}", "\\textbf{Setting}", "\\textbf{Design}", "\\textbf{Outcome}",
    "\\textbf{Reserved}", "\\textbf{Open}", "\\textbf{Difference (SE)}", "\\textbf{N}",
    sep = " & "
  )
  out <- c(
    "{\\scriptsize",
    "\\setlength{\\LTleft}{0pt}",
    "\\setlength{\\LTright}{0pt}",
    "\\setlength{\\tabcolsep}{3pt}",
    "\\renewcommand{\\arraystretch}{1.0}",
    sprintf("\\begin{longtable}{%s}", colspec),
    sprintf("\\caption{%s}\\label{%s} \\\\", latex_escape(tab$caption), tab$label),
    "\\toprule", paste(headers, "\\\\"),
    "\\endfirsthead",
    "\\multicolumn{8}{l}{\\textit{Table \\thetable{} (continued)}} \\\\",
    "\\toprule", paste(headers, "\\\\"),
    "\\endhead",
    "\\bottomrule",
    "\\endfoot",
    unlist(lapply(tab$sections, section_rows)),
    "\\end{longtable}",
    paste0("\\noindent ", notes, "\\par"),
    "}"
  )
  writeLines(out, "tabs/literature_table.tex")
}
