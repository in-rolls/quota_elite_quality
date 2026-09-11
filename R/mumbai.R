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
