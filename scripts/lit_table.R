# Typeset the literature table from lit/tables.yaml and lit/studies/*.yaml; content lives in those files.
suppressPackageStartupMessages(library(dplyr))
source("R/literature.R")

tab <- yaml::read_yaml("lit/tables.yaml")
lit <- read_literature()

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
  dagger <- ifelse(d$se_source == "computed", "$^\\dagger$", "")
  minus <- function(x) sub("^-", "$-$", x)
  paste0(
    minus(mapply(formatC, scale * d$diff, format = "f", digits = digits)), " (",
    mapply(formatC, scale * d$se, format = "f", digits = digits), ")", dagger
  )
}

section_rows <- function(sec) {
  d <- lit |>
    filter(population == sec$population, comparison %in% unlist(sec$comparison)) |>
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
  "\\begin{landscape}",
  "{\\scriptsize",
  "\\setlength{\\LTleft}{0pt}",
  "\\setlength{\\LTright}{0pt}",
  "\\setlength{\\tabcolsep}{3pt}",
  "\\renewcommand{\\arraystretch}{1.1}",
  sprintf("\\begin{longtable}{%s}", colspec),
  sprintf("\\caption{%s}\\label{%s} \\\\", latex_escape(tab$caption), tab$label),
  "\\toprule", paste(headers, "\\\\"),
  "\\endfirsthead",
  "\\multicolumn{8}{l}{\\textit{Table \\thetable{} (continued)}} \\\\",
  "\\toprule", paste(headers, "\\\\"),
  "\\endhead",
  "\\bottomrule",
  sprintf("\\multicolumn{8}{p{0.97\\linewidth}}{%s} \\\\", notes),
  "\\endlastfoot",
  unlist(lapply(tab$sections, section_rows)),
  "\\end{longtable}",
  "}",
  "\\end{landscape}"
)
writeLines(out, "output/literature_table.tex")
