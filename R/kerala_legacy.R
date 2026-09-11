sixth_or_lower <- c(
  "2nd std", "3rd standard", "4th", "4 th standard", "4 th", "fourth standard", "iv", "4", "fourth",
  "5th", "fifth", "v th std", "5 th std", "5 th", "5", "v std", "v th standard", "vth std",
  "five", "four", "3rd std", "ivth std", "v",
  "6th", "sixth", "6 th standard", "vi th std", "6", "vi th standard"
)

seventh_to_tenth <- c(
  "seventh", "std 7", "vii", "7th", "vii th std", "seventh standard",
  "7 th class", "7 th", "7 class", "7 th standard", "seven", "7 std",
  "7 standard", "viiith std", "7",
  "8th", "viii", "8 std", "8 th std", "eighth standard",
  "8 th standard", "eight standard", "eighth", "8 th class",
  "8 standard", "8", "8 th", "eight std", "eight",
  "nineth", "nine", "ninth", "9th", "9 th", "ix std", "ixth",
  "ninenth standard",
  "ix th std", "ninth standard", "9 th std", "9 th standard",
  "ixth standard", "std 9", "9 standard", "9", "ix", "ix class",
  "tenth", "10th", "10 th pass", "10 th std", "10", "x th", "x"
)

hs <- c(
  "sslc", "s s l c", "ssslc", "high school", "ss lc", "plustwo", "s s lc", "higher secondary", "ssc",
  "pre degree", "pre-degree", "predegree", "pree degree", "pree-degree", "s sl c", "ssl c",
  "sslcpassed", "plus one",
  "p d c", "p dc", "pdc", "pree digree", "pre digree", "pre-digree",
  "pre- degree", "preedegree", "pre -degree", "pre - degree", # apparently 11th and 12th
  "plus two", "+2", "plus-two", "pluse two", "plus2"
)

ba <- c(
  "ba", "b a", "badegree",
  "babed", "b ed", "b-ed", "bed",
  "bcom", "bachelor of arts",
  "ballb", "llb", "l l b", "bcomllb",
  "hindi bed",
  "b-com", "b com",
  "btech", "b-tech", "b tech",
  "bsc", "b s c", "b sc",
  "b pharm", "bscbed",
  "bpt", "bca", "bba", "baeconomics"
)

recode_education <- function(education_level) {
  if (grepl(paste0("\\b(", paste(sixth_or_lower, collapse = "|"), ")\\b"), education_level)) {
    "6th grade or lower"
  } else if (grepl(paste0("\\b(", paste(seventh_to_tenth, collapse = "|"), ")\\b"), education_level)) {
    "7th to 10th grade"
  } else if (grepl(paste0("\\b(", paste(hs, collapse = "|"), ")\\b"), education_level)) {
    "11th or High School"
  } else if (grepl(paste0("\\b(", paste(ba, collapse = "|"), ")\\b"), education_level)) {
    "Bachelor's degree or above"
  } else if (grepl("ma|m a|mcom|m com|msc|mba|mtech|post graduation|post graduate|phd|llm|m tech", education_level)) {
    "Master's degree or above"
  } else {
    "Other"
  }
}
