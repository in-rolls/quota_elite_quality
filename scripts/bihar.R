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
  summarize(prop_illiterate = mean(educ == 'Illiterate', na.rm = T),
            prop_graduate_or_more = mean(educ %in% c('Graduate', 'Post Graduate'), na.rm = T),
            mean_age = mean(age_n, na.rm = T), 
            n = n())

kable(tab, format = "pipe", caption = "Bihar 2016 Sarpanch")

