## Candidate Quality

### Load libs
library(tidyverse)
library(readr)
library(knitr)
library(kableExtra)

### Read in the Bihar 2016 Sarpanch data

raj_2019 <- readr::read_csv("rajasthan_municipal_2019.csv")
ed <- readr::read_csv("rajasthan_municipal_2019_education_winning.csv")

dat <- left_join(
  raj_2019[, c("sr_no", "district", "ward_category", "age")],
  ed[, c("sr_no", "elected_member_educ")],
  by = "sr_no",
  copy = FALSE,
  keep = FALSE
)

tab_4 <- dat %>% 
  group_by(ward_cat = tolower(ward_category)) %>% 
  summarize(prop_hs_or_less = round(mean(elected_member_educ %in% c("PRIMARY", "LITERA TE", "LITERATE", "ILLITERA TE", "10th", "12th", "3th", "4th", "5 th", "5th", "5TH", "6th", "6TH", "7th", "8 th", "8 TH", "8th", "8TH", "9th", "9TH"), na.rm = T), 2), 
            n = n())

kable(tab_4, format = "pipe", caption = "Rajasthan 2019 Municipal")




