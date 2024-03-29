# Load lib
library(dplyr)
library(readr)
library(knitr)

# Load dat
kerala_lsgi <- read_csv("data/lsgi-election-kerala.csv")

# Rename
names(kerala_lsgi)[names(kerala_lsgi) == "Educational Qualification"] <- "educ"

# Recode
kerala_lsgi$educ[is.na(kerala_lsgi$educ)] <- "Unknown"
kerala_lsgi$norm_educ <- tolower(gsub("\\s+", " ", gsub("\\.", "", kerala_lsgi$educ)))

# 
sixth_or_lower <- c("2nd std", "3rd standard", "4th", "4 th standard", "4 th", "fourth standard", "iv", "4", "fourth",
                 "5th", "fifth", "v th std", "5 th std", "5 th", "5", "v std", "v th standard", "vth std",
                 "five", "four", "3rd std", "ivth std", "v",
                 "6th", "sixth", "6 th standard", "vi th std", "6", "vi th standard")

seventh_to_tenth <- c("seventh", "std 7", "vii", "7th", "vii th std", "seventh standard", 
                      "7 th class", "7 th", "7 class", "7 th standard", "seven", "7 std",
                      "7 standard", "viiith std", "7",
                      "8th", "viii", "8 std", "8 th std", "eighth standard", 
                      "8 th standard", "eight standard", "eighth", "8 th class",
                      "8 standard", "8","8 th", "eight std", "eight",
                      "nineth", "nine", "ninth", "9th", "9 th", "ix std", "ixth",
                      "ninenth standard",
                      "ix th std", "ninth standard", "9 th std", "9 th standard",
                      "ixth standard", "std 9", "9 standard", "9", "ix", "ix class",
                      "tenth", "10th", "10 th pass", "10 th std", "10", "x th", "x")

hs <- c(
  "sslc", "s s l c", "ssslc", "high school", "ss lc", "plustwo", "s s lc", "higher secondary", "ssc",
  "pre degree", "pre-degree", "predegree", "pree degree", "pree-degree", "s sl c", "ssl c",
  "sslcpassed", "plus one",
  "p d c", "p dc", "pdc", "pree digree", "pre digree", "pre-digree",
  "pre- degree", "preedegree", "pre -degree", "pre - degree",  #apparently 11th and 12th
  "plus two", "+2", "plus-two", "pluse two", "plus2"
)

ba <- c("ba", "b a", "badegree", 
        "babed", "b ed", "b-ed", "bed",
        "bcom", "bachelor of arts",
        "ballb", "llb", "l l b", "bcomllb",
        "hindi bed",
        "b-com", "b com", 
        "btech", "b-tech", "b tech",
        "bsc","b s c", "b sc", 
        "b pharm", "bscbed",
        "bpt", "bca", "bba", "baeconomics")

recode_education <- function(education_level) {
  if (grepl(paste0("\\b(", paste(sixth_or_lower, collapse = "|"), ")\\b"), education_level)) {
    return("6th grade or lower")
  } else if (grepl(paste0("\\b(", paste(seventh_to_tenth, collapse = "|"), ")\\b"), education_level)) {
    return("7th to 10th grade")
  } else if (grepl(paste0("\\b(", paste(hs, collapse = "|"), ")\\b"), education_level)) {
    return("11th or High School")
  } else if (grepl(paste0("\\b(", paste(ba, collapse = "|"), ")\\b"), education_level)) {
    return("Bachelor's degree or above")
  } else if (grepl("ma|m a|mcom|m com|msc|mba|mtech|post graduation|post graduate|phd|llm|m tech", education_level)) {
    return("Master's degree or above")
  } else {
    return("Other")
  }
}

kerala_lsgi$recoded_education = sapply(kerala_lsgi$norm_educ, recode_education)
b <- table(kerala_lsgi$norm_educ[kerala_lsgi$recoded_education == "Other"])
sort(-b)[0:100]

# Define the desired order
desired_order <- c("General", "Woman", "SC", "SC Woman", "ST", "ST Woman")
kerala_lsgi$Reservation <- factor(kerala_lsgi$Reservation, levels = desired_order)

result <- kerala_lsgi %>%
  group_by(Reservation, Year) %>%
  summarize(
    sixth_or_lower = sum(recoded_education == "6th grade or lower") / n(),
    seventh_to_tenth = sum(recoded_education == "7th to 10th grade") / n(),
    eleventh_hs = sum(recoded_education == "11th or High School") / n(),
    prop_Bachelors_or_above = sum(recoded_education == "Bachelor's degree or above") / n(),
    prop_Masters_or_above = sum(recoded_education == "Master's degree or above") / n(),
    prop_Other = sum(recoded_education == "Other") / n(),
    n = n()
  ) %>%
  arrange(Year, Reservation) %>%
  mutate(across(c("sixth_or_lower", "seventh_to_tenth", "eleventh_hs", "prop_Bachelors_or_above", "prop_Masters_or_above", "prop_Other"), ~ round(., digits = 2)))

kable(result)

result <- kerala_lsgi %>%
  group_by(`LGI Type`, Reservation) %>%
  summarize(
    hs_or_below = (sum(recoded_education == "6th grade or lower") + 
                   sum(recoded_education == "7th to 10th grade") + 
                   sum(recoded_education == "11th or High School")) / n(),
    prop_bachelors = sum(recoded_education == "Bachelor's degree or above") / n(),
    prop_masters_or_above = sum(recoded_education == "Master's degree or above") / n(),
    prop_other = sum(recoded_education == "Other") / n(),
    n = n()
  ) %>%
  arrange(`LGI Type`, Reservation) %>%
  mutate(across(c("hs_or_below", "prop_bachelors", "prop_masters_or_above", "prop_other"), ~ round(., digits = 2)))

kable(result)
