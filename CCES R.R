library(haven)
library(tidyverse)
library(ggplot2)
library(survey)
library(ggthemes)

CCES <- read_dta("Documents - Harvard University/2018-2019/September-December/American Public Opinion/Final Memo/CCES16_Common_OUTPUT_Feb2018_VV.dta")
CCES <- as_factor(CCES)


CCES$state <- CCES$inputstate

CCES$white <- CCES$race
white <- CCES %>% rename(race = race) %>% mutate(w = race == "White")

des <- svydesign(id=~1, weights = ~commonweight, data=CCES)


svymean(~white, design = des)




CCES_white <- svyby(~ white, ~ state, design=des, svymean)
