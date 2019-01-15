library(haven)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(survey)
library(ggthemes)

hh <- read_sav("Documents - Harvard University/2018-2019/September-December/American Public Opinion/Final Memo/2018-11_White-House.sav")

hh <- as_factor(hh)


#White College Educated Immigration Approval
hh$education <- hh$H5
table(hh$education)

white_education <- hh %>% rename (edu = H5, rac = QRACE) %>% mutate (whites = rac == "White" & edu == "4-year college degree or more")

xtabs(~ edu + whites, white_education)

