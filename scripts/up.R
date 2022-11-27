## Candidate Quality

### Load libs
library(tidyverse)
library(readr)
library(knitr)
library(kableExtra)

### Read in the Bihar 2016 Sarpanch data

up_all <- fs::dir_ls("data/up_2015/", regexp = "\\.csv$")

up_all_dat <- up_all %>% 
  map_dfr(read_csv, col_select = c("पद का आरक्षण", "शैक्षिक योग्यता"), .id = "source", show_col_types = FALSE)

names(up_all_dat) <- c("source", "reservation_status", "education")

up_all_dat %<>% mutate(position = case_when(grepl("पंचायत प्रमुख", source) ~ "area panchayat pramukh",
                                           grepl("क्षेत्र पंचायत सदस्य", source) ~ "area panchayat member", 
                                           grepl("ग्राम पंचायत प्रधान", source) ~ "gram panchayat pradhan", 
                                           grepl("जिला पंचायत अध्यक्", source) ~ "zila panchayat adhyaksh", 
                                           grepl("जिला पंचायत सदस्य", source) ~ "zila panchayat pradhan"))

### Analysis
tab <- up_all_dat %>% 
  group_by(reservation_status) %>% 
  summarize(prop_illiterate = round(mean(education == "निरक्षर", na.rm = T), 2), 
            prop_college_or_more = round(mean(education %in% c('परास्नातक', 'स्नातक', 'पी० एच० डी०'), na.rm = T), 2), 
            n = n())

kable(tab, format = "pipe", caption = "UP 2015")

tab_2 <- up_all_dat %>% 
  group_by(reservation_status, position) %>% 
  summarize(prop_illiterate = round(mean(education == "निरक्षर", na.rm = T), 2), 
            prop_college_or_more = round(mean(education %in% c('परास्नातक', 'स्नातक', 'पी० एच० डी०'), na.rm = T), 2), 
            n = n())