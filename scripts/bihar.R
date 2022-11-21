## Candidate Quality

### Load libs
library(tidyverse)
library(readr)
library(knitr)
library(kableExtra)

### Read in the Bihar 2016 Sarpanch data
sp <- read_csv("https://raw.githubusercontent.com/in-rolls/bihar_2016_panchayat_elex/master/data/sarpanch.csv")
sp$age_n <- as.numeric(sp$age)

sum(sp$age_n > 100, na.rm = T)

sp_fin <- sp %>%
  filter(sp$age_n < 100)

### Analysis
tab <- sp_fin %>% 
  group_by(reservation_status) %>%
  summarize(prop_illiterate = round(mean(educ == 'Illiterate', na.rm = T), 2), 
            prop_graduate_or_more = round(mean(educ %in% c('Graduate', 'Post Graduate'), na.rm = T), 2), 
            mean_age = round(mean(age_n, na.rm = T), 2), 
            n = n())

kable(tab, format = "pipe", caption = "Bihar 2016 Sarpanch")

