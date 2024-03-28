# Load lib
library(dplyr)
library(readr)
library(knitr)

# Load dat
a <- read_csv("/Users/soodoku/Downloads/lsgi-election-kerala.csv")

names(a)[names(a) == "Educational Qualification"] <- "educ"

# Recode
a$educ[is.na(a$educ)] <- "Unknown"

# Define function to recode education levels
recode_education <- function(education_level) {
  if (grepl("SSLC|S.S.L.C|S S L C|S. S. L. C|S.S.L.C.|PLUS TWO|4TH STANDARD|5TH STANDARD|FIFTH STANDARD|6 TH STD|6TH STANDARD|6TH STD|SEVENTH STD|VII STD|7TH CLASS|7TH STANDARD|7 TH STD|7TH STD|8TH CLASS|8 TH STD|VIII|8TH STANDARD|9TH STANDARD|NINETH|NINETH STANDARD|9 TH|9TH|9TH STD|TENTH CLASS|10TH|+2|PLUS 2", education_level)) {
    return("HS or less")
  } else if (grepl("BA|B.A|B COM|B. COM|BCOM|B.COM|BSC|B.SC|BPT|BCOM|BCA|BBA|B TECH", education_level)) {
    return("Bachelor's degree or above")
  } else if (grepl("M.A|MA|M A|M.COM|MSC|MBA", education_level)) {
    return("Master's degree or above")
  } else {
    return("Other")
  }
}

a$recoded_education = sapply(a$educ, recode_education)
b <- table(a$educ[a$recoded_education == "Other"])
sort(-b)[0:100]

# Tabulate

result <- a %>%
  group_by(Reservation, Year) %>%
  summarize(
    prop_HS_or_less = sum(recoded_education == "HS or less") / n(),
    prop_Bachelors_or_above = sum(recoded_education == "Bachelor's degree or above") / n(),
    prop_Masters_or_above = sum(recoded_education == "Master's degree or above") / n(),
    prop_Other = sum(recoded_education == "Other") / n()
  )

kable(result)
