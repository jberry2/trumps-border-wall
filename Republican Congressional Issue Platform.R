library(haven)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(survey)
library(ggthemes)

hh <- read_sav("Documents - Harvard University/2018-2019/September-December/American Public Opinion/Final Memo/2018-11_Republican-Congressional.sav")

hh <- as_factor(hh)
hh_design <- svydesign(data = hh, ids = ~1, weights = ~nweight)

# White Rural fiscal cons Support
white_rural <- hh %>% rename(area = D107, rac3 = QRACE) %>% mutate(w_rural = rac3 == "White" & area == "Rural")

white_rural$ec_support3 <- white_rural$Q24

des <- svydesign(id=~1, weights = ~nweight, data=white_rural)

hh.white_rural <- svyby( ~ ec_support3, ~ w_rural, design=des, svymean)

# White nonrural EC Support
white_non_rural <- hh %>% rename(area = D107, rac3 = QRACE) %>% mutate(w_non_rural = rac3 == "White" & area != "Rural")

white_non_rural$ec_support3 <- white_non_rural$Q24

des <- svydesign(id=~1, weights = ~nweight, data=white_non_rural)

hh.white_non_rural <- svyby( ~ ec_support3, ~ w_non_rural, design=des, svymean)

# White non_white economy Support
non_white <- hh %>% rename(rac4 = QRACE) %>% mutate(n_white = rac4 != "White")

non_white$ec_support3 <- non_white$Q24

des <- svydesign(id=~1, weights = ~nweight, data=non_white)

hh.non_white <- svyby( ~ ec_support3, ~ n_white, design=des, svymean)

# White suburb economy Support
white_suburban <- hh %>% rename(area = D107, rac3 = QRACE) %>% mutate(w_suburban = rac3 == "White" & area == "Suburban")

white_suburban$ec_support3 <- white_suburban$Q24

des <- svydesign(id=~1, weights = ~nweight, data=white_suburban)

hh.white_suburban <- svyby( ~ ec_support3, ~ w_suburban, design=des, svymean)

# White urban economy Support
white_urban <- hh %>% rename(area = D107, rac3 = QRACE) %>% mutate(w_urban = rac3 == "White" & area == "Urban")

white_urban$ec_support3 <- white_urban$Q24
table(white_urban$ec_support3)

des <- svydesign(id=~1, weights = ~nweight, data=white_urban)

hh.white_urban <- svyby( ~ ec_support3, ~ w_urban, design=des, svymean)

###################
# White Rural fiscal cons Support
white_rural <- hh %>% rename(area = D107, rac3 = QRACE) %>% mutate(w_rural = rac3 == "White" & area == "Rural")

white_rural$ec_support3 <- white_rural$Q25

des <- svydesign(id=~1, weights = ~nweight, data=white_rural)

hh.white_rural <- svyby( ~ ec_support3, ~ w_rural, design=des, svymean)

# White nonrural EC Support
white_non_rural <- hh %>% rename(area = D107, rac3 = QRACE) %>% mutate(w_non_rural = rac3 == "White" & area != "Rural")

white_non_rural$ec_support3 <- white_non_rural$Q25

des <- svydesign(id=~1, weights = ~nweight, data=white_non_rural)

hh.white_non_rural <- svyby( ~ ec_support3, ~ w_non_rural, design=des, svymean)

# White non_white economy Support
non_white <- hh %>% rename(rac4 = QRACE) %>% mutate(n_white = rac4 != "White")

non_white$ec_support3 <- non_white$Q25

des <- svydesign(id=~1, weights = ~nweight, data=non_white)

hh.non_white <- svyby( ~ ec_support3, ~ n_white, design=des, svymean)

# White suburb economy Support
white_suburban <- hh %>% rename(area = D107, rac3 = QRACE) %>% mutate(w_suburban = rac3 == "White" & area == "Suburban")

white_suburban$ec_support3 <- white_suburban$Q25

des <- svydesign(id=~1, weights = ~nweight, data=white_suburban)

hh.white_suburban <- svyby( ~ ec_support3, ~ w_suburban, design=des, svymean)

# White urban economy Support
white_urban <- hh %>% rename(area = D107, rac3 = QRACE) %>% mutate(w_urban = rac3 == "White" & area == "Urban")

white_urban$ec_support3 <- white_urban$Q25
table(white_urban$ec_support3)

des <- svydesign(id=~1, weights = ~nweight, data=white_urban)

hh.white_urban <- svyby( ~ ec_support3, ~ w_urban, design=des, svymean)

###################
# White Rural fiscal cons Support
white_rural <- hh %>% rename(area = D107, rac3 = QRACE) %>% mutate(w_rural = rac3 == "White" & area == "Rural")

white_rural$ec_support3 <- white_rural$Q11r5

des <- svydesign(id=~1, weights = ~nweight, data=white_rural)

hh.white_rural <- svyby( ~ ec_support3, ~ w_rural, design=des, svymean)

# White nonrural EC Support
white_non_rural <- hh %>% rename(area = D107, rac3 = QRACE) %>% mutate(w_non_rural = rac3 == "White" & area != "Rural")

white_non_rural$ec_support3 <- white_non_rural$Q11r5

des <- svydesign(id=~1, weights = ~nweight, data=white_non_rural)

hh.white_non_rural <- svyby( ~ ec_support3, ~ w_non_rural, design=des, svymean)

# White non_white economy Support
non_white <- hh %>% rename(rac4 = QRACE) %>% mutate(n_white = rac4 != "White")

non_white$ec_support3 <- non_white$Q11r5

des <- svydesign(id=~1, weights = ~nweight, data=non_white)

hh.non_white <- svyby( ~ ec_support3, ~ n_white, design=des, svymean)

# White suburb economy Support
white_suburban <- hh %>% rename(area = D107, rac3 = QRACE) %>% mutate(w_suburban = rac3 == "White" & area == "Suburban")

white_suburban$ec_support3 <- white_suburban$Q11r5

des <- svydesign(id=~1, weights = ~nweight, data=white_suburban)

hh.white_suburban <- svyby( ~ ec_support3, ~ w_suburban, design=des, svymean)

# White urban economy Support
white_urban <- hh %>% rename(area = D107, rac3 = QRACE) %>% mutate(w_urban = rac3 == "White" & area == "Urban")

white_urban$ec_support3 <- white_urban$Q11r5
table(white_urban$ec_support3)

des <- svydesign(id=~1, weights = ~nweight, data=white_urban)

hh.white_urban <- svyby( ~ ec_support3, ~ w_urban, design=des, svymean)

###################
# White Rural fiscal cons Support
white_rural <- hh %>% rename(area = D107, rac3 = QRACE) %>% mutate(w_rural = rac3 == "White" & area == "Rural")

white_rural$ec_support3 <- white_rural$Q20

des <- svydesign(id=~1, weights = ~nweight, data=white_rural)

hh.white_rural <- svyby( ~ ec_support3, ~ w_rural, design=des, svymean)

# White nonrural EC Support
white_non_rural <- hh %>% rename(area = D107, rac3 = QRACE) %>% mutate(w_non_rural = rac3 == "White" & area != "Rural")

white_non_rural$ec_support3 <- white_non_rural$Q20

des <- svydesign(id=~1, weights = ~nweight, data=white_non_rural)

hh.white_non_rural <- svyby( ~ ec_support3, ~ w_non_rural, design=des, svymean)

# White non_white economy Support
non_white <- hh %>% rename(rac4 = QRACE) %>% mutate(n_white = rac4 != "White")

non_white$ec_support3 <- non_white$Q20

des <- svydesign(id=~1, weights = ~nweight, data=non_white)

hh.non_white <- svyby( ~ ec_support3, ~ n_white, design=des, svymean)

# White suburb economy Support
white_suburban <- hh %>% rename(area = D107, rac3 = QRACE) %>% mutate(w_suburban = rac3 == "White" & area == "Suburban")

white_suburban$ec_support3 <- white_suburban$Q20

des <- svydesign(id=~1, weights = ~nweight, data=white_suburban)

hh.white_suburban <- svyby( ~ ec_support3, ~ w_suburban, design=des, svymean)

# White urban economy Support
white_urban <- hh %>% rename(area = D107, rac3 = QRACE) %>% mutate(w_urban = rac3 == "White" & area == "Urban")

white_urban$ec_support3 <- white_urban$Q20
table(white_urban$ec_support3)

des <- svydesign(id=~1, weights = ~nweight, data=white_urban)

hh.white_urban <- svyby( ~ ec_support3, ~ w_urban, design=des, svymean)

###################
# White Rural fiscal cons Support
white_rural <- hh %>% rename(area = D107, rac3 = QRACE) %>% mutate(w_rural = rac3 == "White" & area == "Rural")

white_rural$ec_support3 <- white_rural$Q23

des <- svydesign(id=~1, weights = ~nweight, data=white_rural)

hh.white_rural <- svyby( ~ ec_support3, ~ w_rural, design=des, svymean)

# White nonrural EC Support
white_non_rural <- hh %>% rename(area = D107, rac3 = QRACE) %>% mutate(w_non_rural = rac3 == "White" & area != "Rural")

white_non_rural$ec_support3 <- white_non_rural$Q23

des <- svydesign(id=~1, weights = ~nweight, data=white_non_rural)

hh.white_non_rural <- svyby( ~ ec_support3, ~ w_non_rural, design=des, svymean)

# White non_white economy Support
non_white <- hh %>% rename(rac4 = QRACE) %>% mutate(n_white = rac4 != "White")

non_white$ec_support3 <- non_white$Q23

des <- svydesign(id=~1, weights = ~nweight, data=non_white)

hh.non_white <- svyby( ~ ec_support3, ~ n_white, design=des, svymean)

# White suburb economy Support
white_suburban <- hh %>% rename(area = D107, rac3 = QRACE) %>% mutate(w_suburban = rac3 == "White" & area == "Suburban")

white_suburban$ec_support3 <- white_suburban$Q23

des <- svydesign(id=~1, weights = ~nweight, data=white_suburban)

hh.white_suburban <- svyby( ~ ec_support3, ~ w_suburban, design=des, svymean)

# White urban economy Support
white_urban <- hh %>% rename(area = D107, rac3 = QRACE) %>% mutate(w_urban = rac3 == "White" & area == "Urban")

white_urban$ec_support3 <- white_urban$Q23
table(white_urban$ec_support3)

des <- svydesign(id=~1, weights = ~nweight, data=white_urban)

hh.white_urban <- svyby( ~ ec_support3, ~ w_urban, design=des, svymean)
