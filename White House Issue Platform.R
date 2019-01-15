install.packages("tidyverse")
install.packages("survey")
install.packages("ggthemes")

library(haven)
library(dplyr)
library(tidyverse)
library(ggplot2)
library(survey)
library(ggthemes)

hh <- read_sav("Documents - Harvard University/2018-2019/September-December/American Public Opinion/Final Memo/2018-11_White-House.sav")

hh <- as_factor(hh)
hh_design <- svydesign(data = hh, ids = ~1, weights = ~nweight)

#White Immigration Approval

hh$immigration_approval <- hh$Q8
table(hh$immigration_approval)

hh$race <- hh$QRACE
hh$area <- hh$D107
xtabs(~ race + area, data = hh)

white_immigration <- svytable(~immigration_approval + race, design = hh_design)
prop.table(white_immigration, margin = 2)

#White Job Approval

hh$job_approval <- hh$Q6
table(hh$job_approval)
hh_design$race <- hh_design$QRACE
table(hh_design$race)

white_job <- svytable(~job_approval + racejob_approval, design = hh_design)
prop.table(white_job, margin=2)

#White Wall Support


hh_design$race <- hh_design$QRACE
table(hh_design$race)

white_wall <- svytable(~wall_support + race, design = hh_design)
prop.table(white_wall, margin=2)

#White Limit Immigration Support

hh$lim_support <- hh$Q10r6
table(hh$lim_support)

hh_design$race <- hh_design$QRACE

white_lim <- svytable(~lim_support + race, design = hh_design)
prop.table(white_lim, margin=2)

#White Iran  Support

hh$iran_support <- hh$Q15
table(hh$iran_support)

hh_design$race <- hh_design$QRACE

white_iran <- svytable(~iran_support + race, design = hh_design)
prop.table(white_iran, margin=2)

#White Tax  Support

hh_design$tax_support <- hh_design$Q26

hh_design$race <- hh_design$QRACE

white_tax <- svytable(~tax_support + race, design = hh_design)
prop.table(white_tax, margin=2)

#White Tariff  Support

hh$tar_support <- hh$Q16r1
table(hh$tar_support)

hh_design$race <- hh_design$QRACE

white_tar <- svytable(~tar_support + race, design = hh_design)
prop.table(white_tar, margin=2)

#White char  Support

hh$char_support <- hh$Q23
table(hh$char_support)

hh_design$race <- hh_design$QRACE

white_char <- svytable(~char_support + race, design = hh_design)
prop.table(white_char, margin=2)


#Republican Immigration Approval

hh_design$immigration_approval <- hh_design$Q8
table(hh_design$immigration_approval)

hh_design$party <- hh_design$H4
table(hh_design$party)

party_immigration <- svytable(~immigration_approval + party, ddesign = hh_design)
prop.table(party_immigration, margin=2)

#Republican Job Approval

hh_design$job_approval <- hh_design$Q6
table(hh_design$job_approval)

hh_design$party <- hh_design$H4
table(hh_design$party)

party_job <- svytable(~job_approval + party, design = hh_design)
prop.table(party_job, margin=2)

#Republican Wall Support

hh$wall_support <- hh$Q10r1
table(hh$wall_support)

hh$partystrength <-  hh$D110
tab <- xtabs(~wall_support + partystrength, data = hh)

prop.table(tab, margin = 2)  

hh_design$party <- hh_design$H4
table(hh_design$party)

party_wall <- svytable(~wall_support + party, design = hh_design)
prop.table(party_wall, margin=2)

#Republican lim Support

hh_design$lim_support <- hh_design$Q10r6

hh_design$party <- hh_design$H4

party_lim <- svytable(~lim_support + party, design = hh_design)
prop.table(party_lim, margin=2)

#Republican Iran Support

hh_design$iran_support <- hh_design$Q15

hh_design$party <- hh_design$H4

party_iran <- svytable(~iran_support + party, design = hh_design)
prop.table(party_iran, margin=2)

#Republican Tax Support

hh$tax_support <- hh$Q26

hh$party <- hh$H4

party_tax <- xtabs(~tax_support + party, data = hh)
prop.table(party_tax, margin=2)

#Republican Tar Support

hh_design$tar_support <- hh_design$Q16r1

hh_design$party <- hh_design$H4

party_tar <- svytable(~tar_support + party, design = hh_design)
prop.table(party_tar, margin=2)

#Republican char Support

hh_design$char_support <- hh_design$Q23

hh_design$party <- hh_design$H4

party_char <- svytable(~char_support + party, design = hh_design)
prop.table(party_char, margin=2)

#White College Educated Immigration Approval
hh$education <- hh$H5
table(hh$education)

white_education <- hh %>% rename(edu = H5, rac = QRACE) %>% mutate(w_coll = rac == "White" & edu == "4-year college degree or more")

white_education$immigration_approval <- white_education$Q8

des <- svydesign(id=~1, weights = ~nweight, data=white_education)

hh.white_educated <- svyby(~ w_coll, ~ immigration_approval, design=des, svymean)

white_educated <- xtabs(~immigration_approval + w_coll, data = white_education)
prop.table(white_educated, margin = 2)

#White College Educated Job Approval

white_education <- hh %>% rename(edu = H5, rac = QRACE) %>% mutate(w_coll = rac == "White" & edu == "4-year college degree or more")

white_education$job_approval <- white_education$Q6

des <- svydesign(id=~1, weights = ~nweight, data=white_education)

hh.white_educated <- svyby(~ w_coll, ~ job_approval, design=des, svymean)

white_educated <- xtabs(~ job_approval + w_coll, data = white_education)
prop.table(white_educated, margin = 2)

#White College Educated Wall Support

white_education <- hh %>% rename(edu = H5, rac = QRACE) %>% mutate(w_coll = rac == "White" & edu == "4-year college degree or more")

white_education$wall_support <- white_education$Q10r1

des <- svydesign(id=~1, weights = ~nweight, data=white_education)

hh.white_educated <- svyby( ~ w_coll,~ wall_support, design=des, svymean)

white_educated <- xtabs(~ wall_support + w_coll, data = white_education)
prop.table(white_educated, margin = 2)

#White College Educated lim Support

white_education <- hh %>% rename(edu = H5, rac = QRACE) %>% mutate(w_coll = rac == "White" & edu == "4-year college degree or more")

white_education$lim_support <- white_education$Q10r6

des <- svydesign(id=~1, weights = ~nweight, data=white_education)

hh.white_educated <- svyby(  ~ lim_support,~ w_coll, design=des, svymean)

white_educated <- xtabs(~ lim_support + w_coll, data = white_education)
prop.table(white_educated, margin = 2)


#White College Educated iran Support

white_education <- hh %>% rename(edu = H5, rac = QRACE) %>% mutate(w_coll = rac == "White" & edu == "4-year college degree or more")

white_education$iran_support <- white_education$Q15

des <- svydesign(id=~1, weights = ~nweight, data=white_education)

hh.white_educated <- svyby(~ w_coll, ~ iran_support, design=des, svymean)

white_educated <- xtabs(~ iran_support + w_coll, data = white_education)
prop.table(white_educated, margin = 2)

#White College Educated tax Support

white_education <- hh %>% rename(edu = H5, rac = QRACE) %>% mutate(w_coll = rac == "White" & edu == "4-year college degree or more")

white_education$tax_support <- white_education$Q26

des <- svydesign(id=~1, weights = ~nweight, data=white_education)

hh.white_educated <- svyby(~ w_coll, ~ tax_support, design=des, svymean)

white_educated <- xtabs(~ tax_support + w_coll, data = white_education)
prop.table(white_educated, margin = 2)

#White College Educated tar Support

white_education <- hh %>% rename(edu = H5, rac = QRACE) %>% mutate(w_coll = rac == "White" & edu == "4-year college degree or more")

white_education$tar_support <- white_education$Q16r1

des <- svydesign(id=~1, weights = ~nweight, data=white_education)

hh.white_educated <- svyby(~ w_coll, ~ tar_support, design=des, svymean)

white_educated <- xtabs(~ tar_support + w_coll, data = white_education)
prop.table(white_educated, margin = 2)

#White College Educated char Support

white_education <- hh %>% rename(edu = H5, rac = QRACE) %>% mutate(w_coll = rac == "White" & edu == "4-year college degree or more")

white_education$char_support <- white_education$Q23

des <- svydesign(id=~1, weights = ~nweight, data=white_education)

hh.white_educated <- svyby(~ w_coll, ~ char_support, design=des, svymean)

white_educated <- xtabs(~ char_support + w_coll, data = white_education)
prop.table(white_educated, margin = 2)

# White Non College Educated Immigration Approval
white_education2 <- hh %>% rename(edu2 = H5, rac2 = QRACE) %>% mutate(w_noncol = rac2 == "White" & (edu2 == "High school degree to less than 4-year" |  edu2 == "Less than high school degree"))

white_education2$immigration_approval2 <- white_education2$Q8

des <- svydesign(id=~1, weights = ~nweight, data=white_education2)

hh.white_educated2 <- svyby( ~ immigration_approval2, ~ w_noncol, design=des, svymean)

white_educated2 <- xtabs(~immigration_approval2 + w_noncol, data = white_education2)
prop.table(white_educated2, margin = 2)

# White Non College Educated Job Approval
white_education2 <- hh %>% rename(edu2 = H5, rac2 = QRACE) %>% mutate(w_noncol = rac2 == "White" & (edu2 == "High school degree to less than 4-year" |  edu2 == "Less than high school degree"))

white_education2$job_approval2 <- white_education2$Q6

des <- svydesign(id=~1, weights = ~nweight, data=white_education2)

hh.white_educated2 <- svyby( ~ job_approval2,~ w_noncol, design=des, svymean)

white_educated2 <- xtabs(~job_approval2 + w_noncol, data = white_education2)
prop.table(white_educated2, margin = 2)

# White Non College Educated Wall Support
white_education2 <- hh %>% rename(edu2 = H5, rac2 = QRACE) %>% mutate(w_noncol = rac2 == "White" & edu2 != "4-year college degree or more")

white_education2$wall_support2 <- white_education2$Q10r1

des <- svydesign(id=~1, weights = ~nweight, data=white_education2)

hh.white_educated2 <- svyby( ~ wall_support2, ~ w_noncol, design=des, svymean)


white_educated2 <- xtabs(~wall_support2 + w_noncol, data = white_education2)
prop.table(white_educated2, margin = 2)

# White Non College Educated Limit Support
white_education2 <- hh %>% rename(edu2 = H5, rac2 = QRACE) %>% mutate(w_noncol = rac2 == "White" & edu2 != "4-year college degree or more")

white_education2$lim_support2 <- white_education2$Q10r6

des <- svydesign(id=~1, weights = ~nweight, data=white_education2)

hh.white_educated2 <- svyby( ~ lim_support2, ~ w_noncol, design=des, svymean)

white_educated2 <- xtabs(~lim_support2 + w_noncol, data = white_education2)
prop.table(white_educated2, margin = 2)

# White Non College Educated Iran Support
white_education2 <- hh %>% rename(edu2 = H5, rac2 = QRACE) %>% mutate(w_noncol = rac2 == "White" & edu2 != "4-year college degree or more")

white_education2$iran_support2 <- white_education2$Q15

des <- svydesign(id=~1, weights = ~nweight, data=white_education2)

hh.white_educated2 <- svyby( ~ iran_support2,~ w_noncol, design=des, svymean)

white_educated2 <- xtabs(~iran_support2 + w_noncol, data = white_education2)
prop.table(white_educated2, margin = 2)

# White Non College Educated Tax Support
white_education2 <- hh %>% rename(edu2 = H5, rac2 = QRACE) %>% mutate(w_noncol = rac2 == "White" & edu2 != "4-year college degree or more")

white_education2$tax_support2 <- white_education2$Q26

des <- svydesign(id=~1, weights = ~nweight, data=white_education2)

hh.white_educated2 <- svyby( ~ tax_support2, ~ w_noncol, design=des, svymean)

white_educated2 <- xtabs(~tax_support2 + w_noncol, data = white_education2)
prop.table(white_educated2, margin = 2)

# White Non College Educated tar Support
white_education2 <- hh %>% rename(edu2 = H5, rac2 = QRACE) %>% mutate(w_noncol = rac2 == "White" & edu2 != "4-year college degree or more")

white_education2$tar_support2 <- white_education2$Q16r1

des <- svydesign(id=~1, weights = ~nweight, data=white_education2)

hh.white_educated2 <- svyby( ~ tar_support2, ~ w_noncol, design=des, svymean)

white_educated2 <- xtabs(~tar_support2 + w_noncol, data = white_education2)
prop.table(white_educated2, margin = 2)

# White Non College Educated tar Support
white_education2 <- hh %>% rename(edu2 = H5, rac2 = QRACE) %>% mutate(w_noncol = rac2 == "White" & edu2 != "4-year college degree or more")

white_education2$char_support2 <- white_education2$Q23

des <- svydesign(id=~1, weights = ~nweight, data=white_education2)

hh.white_educated2 <- svyby( ~ char_support2,~ w_noncol, design=des, svymean)

white_educated2 <- xtabs(~char_support2 + w_noncol, data = white_education2)
prop.table(white_educated2, margin = 2)

# White Rural Immigration Approval
white_rural <- hh %>% rename(area = D107, rac3 = QRACE) %>% mutate(w_rural = rac3 == "White" & area == "Rural")

white_rural$immigration_approval3 <- white_rural$Q8

des <- svydesign(id=~1, weights = ~nweight, data=white_rural)

hh.white_rural <- svyby(~ immigration_approval3, ~ w_rural, design=des, svymean)

white_rural_imm <- xtabs(~immigration_approval3 + w_rural, data = white_rural)
prop.table(white_rural_imm, margin = 2)


# White Rural Job Approval
white_rural <- hh %>% rename(area = D107, rac3 = QRACE) %>% mutate(w_rural = rac3 == "White" & area == "Rural")

white_rural$job_approval3 <- white_rural$Q6

des <- svydesign(id=~1, weights = ~nweight, data=white_rural)

hh.white_rural <- svyby( ~ job_approval3, ~ w_rural, design=des, svymean)

white_rural_job <- xtabs(~job_approval3 + w_rural, data = white_rural)
prop.table(white_rural_job, margin = 2)


# White Rural Wall Support
white_rural <- hh %>% rename(area = D107, rac3 = QRACE) %>% mutate(w_rural = rac3 == "White" & area == "Rural")

white_rural$wall_support3 <- white_rural$Q10r1

des <- svydesign(id=~1, weights = ~nweight, data=white_rural)

hh.white_rural <- svyby( ~ wall_support3, ~ w_rural, design=des, svymean)

hh.white_rural

ggplot(hh.white_rural, aes(x= wall_support3 *100, y=w_rural)) + 
  geom_point() +
  geom_errorbarh(aes(xmin=(wall_support3 - 1.96*se)*100, 
                     xmax=(wall_support3 + 1.96*se)*100), 
                 height =.3, position=position_dodge(.6))

white_rural_wall <- xtabs(~wall_support3 + w_rural, data = white_rural)
prop.table(white_rural_wall, margin = 2)

# White Rural Lim Support
white_rural <- hh %>% rename(area = D107, rac3 = QRACE) %>% mutate(w_rural = rac3 == "White" & area == "Rural")

white_rural$lim_support3 <- white_rural$Q10r6

des <- svydesign(id=~1, weights = ~nweight, data=white_rural)

hh.white_rural <- svyby( ~ lim_support3, ~ w_rural, design=des, svymean)

white_rural_lim <- xtabs(~lim_support3 + w_rural, data = white_rural)
prop.table(white_rural_lim, margin = 2)

# White Rural iran Support
white_rural <- hh %>% rename(area = D107, rac3 = QRACE) %>% mutate(w_rural = rac3 == "White" & area == "Rural")

white_rural$iran_support3 <- white_rural$Q15

des <- svydesign(id=~1, weights = ~nweight, data=white_rural)

hh.white_rural <- svyby( ~ iran_support3,  ~ w_rural, design=des, svymean)

white_rural_lim <- xtabs(~iran_support3 + w_rural, data = white_rural)
prop.table(white_rural_lim, margin = 2)

# White Rural tax Support
white_rural <- hh %>% rename(area = D107, rac3 = QRACE) %>% mutate(w_rural = rac3 == "White" & area == "Rural")

white_rural$tax_support3 <- white_rural$Q26

des <- svydesign(id=~1, weights = ~nweight, data=white_rural)

hh.white_rural <- svyby(~ w_rural, ~ tax_support3, design=des, svymean)

white_rural_lim <- xtabs(~tax_support3 + w_rural, data = white_rural)
prop.table(white_rural_lim, margin = 2)

# White Rural tar Support
white_rural <- hh %>% rename(area = D107, rac3 = QRACE) %>% mutate(w_rural = rac3 == "White" & area == "Rural")

white_rural$tar_support3 <- white_rural$Q16r1

des <- svydesign(id=~1, weights = ~nweight, data=white_rural)

hh.white_rural <- svyby( ~ tar_support3, ~ w_rural, design=des, svymean)

white_rural_lim <- xtabs(~tar_support3 + w_rural, data = white_rural)
prop.table(white_rural_lim, margin = 2)

# White Rural Lim Support
white_rural <- hh %>% rename(area = D107, rac3 = QRACE) %>% mutate(w_rural = rac3 == "White" & area == "Rural")

white_rural$char_support3 <- white_rural$Q23

des <- svydesign(id=~1, weights = ~nweight, data=white_rural)

hh.white_rural <- svyby( ~ char_support3, ~ w_rural, design=des, svymean)

white_rural_lim <- xtabs(~char_support3 + w_rural, data = white_rural)
prop.table(white_rural_lim, margin = 2)


# White Suburban Immigration Approval
white_suburban <- hh %>% rename(area = D107, rac4 = QRACE) %>% mutate(w_suburban = rac4 == "White" & area == "Suburban")

white_suburban$immigration_approval4 <- white_suburban$Q8

des <- svydesign(id=~1, weights = ~nweight, data=white_suburban)

hh.white_suburban <- svyby( ~ immigration_approval4, ~ w_suburban, design=des, svymean)

white_suburban_imm <- xtabs(~immigration_approval4 + w_suburban, data = white_suburban)
prop.table(white_suburban_imm, margin = 2)

# White Suburban Job Approval
white_suburban <- hh %>% rename(area = D107, rac4 = QRACE) %>% mutate(w_suburban = rac4 == "White" & area == "Suburban")

white_suburban$job_approval4 <- white_suburban$Q6

des <- svydesign(id=~1, weights = ~nweight, data=white_suburban)

hh.white_suburban <- svyby( ~ job_approval4,~ w_suburban, design=des, svymean)

white_suburban_job <- xtabs(~job_approval4 + w_suburban, data = white_suburban)
prop.table(white_suburban_job, margin = 2)

# White Suburban Wall Support
white_suburban <- hh %>% rename(area = D107, rac4 = QRACE) %>% mutate(w_suburban = rac4 == "White" & area == "Suburban")

white_suburban$wall_support4 <- white_suburban$Q10r1

des <- svydesign(id=~1, weights = ~nweight, data=white_suburban)

hh.white_suburban <- svyby( ~ wall_support4, ~ w_suburban, design=des, svymean)

white_suburban_wall <- xtabs(~wall_support4 + w_suburban, data = white_suburban)
prop.table(white_suburban_wall, margin = 2)

# White Suburban Lim Support
white_suburban <- hh %>% rename(area = D107, rac4 = QRACE) %>% mutate(w_suburban = rac4 == "White" & area == "Suburban")

white_suburban$lim_support4 <- white_suburban$Q10r6

des <- svydesign(id=~1, weights = ~nweight, data=white_suburban)

hh.white_suburban <- svyby(~ lim_support4, ~ w_suburban,  design=des, svymean)

white_suburban_lim <- xtabs(~lim_support4 + w_suburban, data = white_suburban)
prop.table(white_suburban_lim, margin = 2)

# White Suburban Lim Support
white_suburban <- hh %>% rename(area = D107, rac4 = QRACE) %>% mutate(w_suburban = rac4 == "White" & area == "Suburban")

white_suburban$iran_support4 <- white_suburban$Q15

des <- svydesign(id=~1, weights = ~nweight, data=white_suburban)

hh.white_suburban <- svyby( ~ iran_support4, ~ w_suburban, design=des, svymean)

white_suburban_lim <- xtabs(~iran_support4 + w_suburban, data = white_suburban)
prop.table(white_suburban_lim, margin = 2)

# White Suburban tax Support
white_suburban <- hh %>% rename(area = D107, rac4 = QRACE) %>% mutate(w_suburban = rac4 == "White" & area == "Suburban")

white_suburban$tax_support4 <- white_suburban$Q26

des <- svydesign(id=~1, weights = ~nweight, data=white_suburban)

# Weighted by using design = des, look at svyby for weighted
hh.white_suburban <- svyby(~ tax_support4, ~w_suburban, design=des, svymean)

white_suburban_lim <- xtabs(~tax_support4 + w_suburban, data = white_suburban)
prop.table(white_suburban_lim, margin = 2)

# White Suburban tar Support
white_suburban <- hh %>% rename(area = D107, rac4 = QRACE) %>% mutate(w_suburban = rac4 == "White" & area == "Suburban")

white_suburban$tar_support4 <- white_suburban$Q16r1

des <- svydesign(id=~1, weights = ~nweight, data=white_suburban)

hh.white_suburban <- svyby(~ tar_support4, ~  w_suburban, design=des, svymean)

white_suburban_lim <- xtabs(~tar_support4 + w_suburban, data = white_suburban)
prop.table(white_suburban_lim, margin = 2)



# White Suburban char Support
white_suburban <- hh %>% rename(area = D107, rac4 = QRACE) %>% mutate(w_suburban = rac4 == "White" & area == "Suburban")

white_suburban$char_support4 <- white_suburban$Q23

des <- svydesign(id=~1, weights = ~nweight, data=white_suburban)

hh.white_suburban <- svyby( ~ char_support4, ~ w_suburban, design=des, svymean)

white_suburban_lim <- xtabs(~char_support4 + w_suburban, data = white_suburban)
prop.table(white_suburban_lim, margin = 2)

#White Working Immigration Approval

white_working <- hh %>% rename(inc = D109, rac = QRACE) %>% mutate(w_work = rac == "White" & (inc == "$35,000 to $49,999" | inc == "$50,000 to $74,999"))

white_working$job_approval <- white_working$Q6

des <- svydesign(id=~1, weights = ~nweight, data=white_working)

hh.white_educated <- svyby( ~ job_approval,~ w_work, design=des, svymean)

white_work <- xtabs(~job_approval + w_work, data = white_working)
prop.table(white_work, margin = 2)

#White Working Job Approval

white_working <- hh %>% rename(inc = D109, rac = QRACE) %>% mutate(w_work = rac == "White" & (inc == "$35,000 to $49,999" | inc == "$50,000 to $74,999"))

white_working$immigration_approval <- white_working$Q8

des <- svydesign(id=~1, weights = ~nweight, data=white_working)

hh.white_educated <- svyby(~ immigration_approval, ~ w_work, design=des, svymean)

white_work <- xtabs(~immigration_approval + w_work, data = white_working)
prop.table(white_work, margin = 2)

#White Working Wall Support

white_working <- hh %>% rename(inc = D109, rac = QRACE) %>% mutate(w_work = rac == "White" & (inc == "$35,000 to $49,999" | inc == "$50,000 to $74,999"))

white_working$wall_support <- white_working$Q10r1

des <- svydesign(id=~1, weights = ~nweight, data=white_working)

hh.white_educated <- svyby(~ wall_support, ~ w_work, design=des, svymean)

white_work <- xtabs(~wall_support + w_work, data = white_working)
prop.table(white_work, margin = 2)

#White Working Lim Support

white_working <- hh %>% rename(inc = D109, rac = QRACE) %>% mutate(w_work = rac == "White" & (inc == "$35,000 to $49,999" | inc == "$50,000 to $74,999"))

white_working$lim_support <- white_working$Q10r6

des <- svydesign(id=~1, weights = ~nweight, data=white_working)

hh.white_educated <- svyby( ~ lim_support,~ w_work, design=des, svymean)

white_work <- xtabs(~lim_support + w_work, data = white_working)
prop.table(white_work, margin = 2)

#White Working Lim Support

white_working <- hh %>% rename(inc = D109, rac = QRACE) %>% mutate(w_work = rac == "White" & (inc == "$35,000 to $49,999" | inc == "$50,000 to $74,999"))

white_working$iran_support <- white_working$Q15

des <- svydesign(id=~1, weights = ~nweight, data=white_working)

hh.white_educated <- svyby(~ iran_support,~ w_work, design=des, svymean)

white_work <- xtabs(~iran_support + w_work, data = white_working)
prop.table(white_work, margin = 2)

#White Working Lim Support

white_working <- hh %>% rename(inc = D109, rac = QRACE) %>% mutate(w_work = rac == "White" & (inc == "$35,000 to $49,999" | inc == "$50,000 to $74,999"))

white_working$tax_support <- white_working$Q26

des <- svydesign(id=~1, weights = ~nweight, data=white_working)

hh.white_educated <- svyby( ~ tax_support,~ w_work, design=des, svymean)

white_work <- xtabs(~tax_support + w_work, data = white_working)
prop.table(white_work, margin = 2)

#White Working tar Support

white_working <- hh %>% rename(inc = D109, rac = QRACE) %>% mutate(w_work = rac == "White" & (inc == "$35,000 to $49,999" | inc == "$50,000 to $74,999"))

white_working$tar_support <- white_working$Q16r1

des <- svydesign(id=~1, weights = ~nweight, data=white_working)

hh.white_educated <- svyby( ~ tar_support, ~ w_work, design=des, svymean)

white_work <- xtabs(~tar_support + w_work, data = white_working)
prop.table(white_work, margin = 2)

#White Working tar Support

white_working <- hh %>% rename(inc = D109, rac = QRACE) %>% mutate(w_work = rac == "White" & (inc == "$35,000 to $49,999" | inc == "$50,000 to $74,999"))

white_working$char_support <- white_working$Q23

des <- svydesign(id=~1, weights = ~nweight, data=white_working)

hh.white_educated <- svyby(~ char_support,~ w_work, design=des, svymean)

white_work <- xtabs(~char_support + w_work, data = white_working)
prop.table(white_work, margin = 2)


#STRONG REPUBLICAN BREAKDOWN

hh$republican_break <- hh$D110
table(hh_design$republican_break)

hh$education <- hh$EDU2
table(hh$education)

republican_edu <- xtabs(~education + republican_break , data = hh)
prop.table(republican_edu, margin=1)

#STRONG REPUBLICAN BREAKDOWN Area

hh$republican_break <- hh$D110
table(hh_design$republican_break)

hh$area <- hh$D107
table(hh$area)

republican_edu <- xtabs(~republican_break + area, data = hh)
prop.table(republican_edu, margin=2)

#STRONG REPUBLICAN BREAKDOWN Income

hh$republican_break <- hh$D110
table(hh_design$republican_break)

hh$inc <- hh$H5A
table(hh$inc)

republican_edu <- xtabs(~republican_break + inc, data = hh)
prop.table(republican_edu, margin=1)

# White Urban Wall Support
white_urban <- hh %>% rename(area1 = D107, rac4 = QRACE) %>% mutate(w_urban = rac4 == "White" & area1 == "Urban")

white_urban$wall_support5 <- white_urban$Q10r1

des <- svydesign(id=~1, weights = ~nweight, data=white_urban)

hh.white_urban <- svyby( ~ wall_support5, ~ w_urban, design=des, svymean)

white_urban_wall <- xtabs(~wall_support5 + w_urban, data = white_urban)
prop.table(white_urban_wall, margin = 2)

# White urban Immigration Approval
white_urban <- hh %>% rename(area = D107, rac3 = QRACE) %>% mutate(w_urban = rac3 == "White" & area == "Urban")

white_urban$immigration_approval3 <- white_urban$Q8

des <- svydesign(id=~1, weights = ~nweight, data=white_urban)

hh.white_urban <- svyby( ~ immigration_approval3, ~ w_urban, design=des, svymean)

white_urban_imm <- xtabs(~immigration_approval3 + w_urban, data = white_urban)
prop.table(white_urban_imm, margin = 2)

# White urban Job Approval
white_urban <- hh %>% rename(area = D107, rac3 = QRACE) %>% mutate(w_urban = rac3 == "White" & area == "Urban")

white_urban$job_approval3 <- white_urban$Q6

des <- svydesign(id=~1, weights = ~nweight, data=white_urban)

hh.white_urban <- svyby( ~ job_approval3, ~ w_urban, design=des, svymean)

white_urban_job <- xtabs(~job_approval3 + w_urban, data = white_urban)
prop.table(white_urban_job, margin = 2)

# White urban tar Support
white_urban <- hh %>% rename(area = D107, rac4 = QRACE) %>% mutate(w_urban = rac4 == "White" & area == "Urban")

white_urban$tar_support4 <- white_urban$Q16r1

des <- svydesign(id=~1, weights = ~nweight, data=white_urban)

hh.white_urban <- svyby(~ tar_support4, ~  w_urban, design=des, svymean)

white_urban_lim <- xtabs(~tar_support4 + w_suburban, data = white_urban)
prop.table(white_urban_lim, margin = 2)



# White urban Lim Support
white_urban <- hh %>% rename(area = D107, rac3 = QRACE) %>% mutate(w_urban = rac3 == "White" & area == "Urban")

white_urban$lim_support3 <- white_urban$Q10r6

des <- svydesign(id=~1, weights = ~nweight, data=white_urban)

hh.white_urban <- svyby( ~ lim_support3, ~ w_urban, design=des, svymean)


# White urban iran Support
white_urban <- hh %>% rename(area = D107, rac3 = QRACE) %>% mutate(w_urban = rac3 == "White" & area == "Urban")

white_urban$iran_support3 <- white_urban$Q15

des <- svydesign(id=~1, weights = ~nweight, data=white_urban)

hh.white_urban <- svyby( ~ iran_support3,  ~ w_urban, design=des, svymean)


# White urban char Support
white_urban <- hh %>% rename(area = D107, rac3 = QRACE) %>% mutate(w_urban = rac3 == "White" & area == "Urban")

white_urban$char_support3 <- white_urban$Q23

des <- svydesign(id=~1, weights = ~nweight, data=white_urban)

hh.white_urban <- svyby( ~ char_support3, ~ w_urban, design=des, svymean)


# White non-rural Wall Support
white_non_rural <- hh %>% rename(area1 = D107, rac4 = QRACE) %>% mutate(w_non_rural = rac4 == "White" & area1 != "Rural")

white_non_rural$wall_support5 <- white_non_rural$Q10r1

des <- svydesign(id=~1, weights = ~nweight, data=white_non_rural)

hh.white_non_rural <- svyby( ~ wall_support5, ~ w_non_rural, design=des, svymean)


# White nonrural Immigration Approval
white_non_rural<- hh %>% rename(area3 = D107, rac3 = QRACE) %>% mutate(w_non_rural = rac3 == "White" & area3 != "Rural")

white_non_rural$immigration_approval3 <- white_non_rural$Q8

des <- svydesign(id=~1, weights = ~nweight, data=white_non_rural)

hh.white_non_rural <- svyby( ~ immigration_approval3, ~ w_non_rural, design=des, svymean)

# White nonrural Job Approval
white_non_rural <- hh %>% rename(area = D107, rac3 = QRACE) %>% mutate(w_non_rural = rac3 == "White" & area != "Rural")

white_non_rural$job_approval3 <- white_non_rural$Q6

des <- svydesign(id=~1, weights = ~nweight, data=white_non_rural)

hh.white_non_rural <- svyby( ~ job_approval3, ~ w_non_rural, design=des, svymean)


# White nonrural tar Support
white_non_rural <- hh %>% rename(area = D107, rac4 = QRACE) %>% mutate(w_non_rural = rac4 == "White" & area != "Rural")

white_non_rural$tar_support4 <- white_non_rural$Q16r1

des <- svydesign(id=~1, weights = ~nweight, data=white_non_rural)

hh.white_non_rural <- svyby(~ tar_support4, ~  w_non_rural, design=des, svymean)




# White nonrural Lim Support
white_non_rural <- hh %>% rename(area = D107, rac3 = QRACE) %>% mutate(w_non_rural = rac3 == "White" & area != "Rural")

white_non_rural$lim_support3 <- white_non_rural$Q10r6

des <- svydesign(id=~1, weights = ~nweight, data=white_non_rural)

hh.white_non_rural <- svyby( ~ lim_support3, ~ w_non_rural, design=des, svymean)


# White nonrural iran Support
white_non_rural <- hh %>% rename(area = D107, rac3 = QRACE) %>% mutate(w_non_rural = rac3 == "White" & area != "Rural")

white_non_rural$iran_support3 <- white_non_rural$Q15

des <- svydesign(id=~1, weights = ~nweight, data=white_non_rural)

hh.white_non_rural <- svyby( ~ iran_support3,  ~ w_non_rural, design=des, svymean)


# White nonrural char Support
white_non_rural <- hh %>% rename(area = D107, rac3 = QRACE) %>% mutate(w_non_rural = rac3 == "White" & area != "Rural")

white_non_rural$char_support3 <- white_non_rural$Q23

des <- svydesign(id=~1, weights = ~nweight, data=white_non_rural)

hh.white_non_rural <- svyby( ~ char_support3, ~ w_non_rural, design=des, svymean)

# White nonrural fp Support
white_non_rural <- hh %>% rename(area = D107, rac3 = QRACE) %>% mutate(w_non_rural = rac3 == "White" & area != "Rural")

white_non_rural$fp_support3 <- white_non_rural$Q7

des <- svydesign(id=~1, weights = ~nweight, data=white_non_rural)

hh.white_non_rural <- svyby( ~ fp_support3, ~ w_non_rural, design=des, svymean)

# White non-white Wall Support
non_white <- hh %>% rename(rac4 = QRACE) %>% mutate(n_white = rac4 != "White")

non_white$wall_support5 <- non_white$Q10r1

des <- svydesign(id=~1, weights = ~nweight, data=non_white)

hh.non_white <- svyby( ~ wall_support5, ~ n_white, design=des, svymean)


# White non_white Immigration Approval
non_white <- hh %>% rename(rac4 = QRACE) %>% mutate(n_white = rac4 != "White")

non_white$immigration_approval3 <- non_white$Q8

des <- svydesign(id=~1, weights = ~nweight, data=non_white)

hh.non_white <- svyby( ~ immigration_approval3, ~ n_white, design=des, svymean)

# White non_white Job Approval
non_white <- hh %>% rename(rac4 = QRACE) %>% mutate(n_white = rac4 != "White")

non_white$job_approval3 <- non_white$Q6

des <- svydesign(id=~1, weights = ~nweight, data=non_white)

hh.non_white <- svyby( ~ job_approval3, ~ n_white, design=des, svymean)


# White non_white tar Support
non_white <- hh %>% rename(rac4 = QRACE) %>% mutate(n_white = rac4 != "White")

non_white$tar_support4 <- non_white$Q16r1

des <- svydesign(id=~1, weights = ~nweight, data=non_white)

hh.non_white <- svyby(~ tar_support4, ~  n_white, design=des, svymean)




# White non_white Lim Support
non_white <- hh %>% rename(rac4 = QRACE) %>% mutate(n_white = rac4 != "White")

non_white$lim_support3 <- non_white$Q10r6

des <- svydesign(id=~1, weights = ~nweight, data=non_white)

hh.non_white <- svyby( ~ lim_support3, ~ n_white, design=des, svymean)


# White non_white iran Support
non_white <- hh %>% rename(rac4 = QRACE) %>% mutate(n_white = rac4 != "White")

non_white$iran_support3 <- non_white$Q15

des <- svydesign(id=~1, weights = ~nweight, data=non_white)

hh.non_white <- svyby( ~ iran_support3,  ~ n_white, design=des, svymean)


# White non_white char Support
non_white <- hh %>% rename(rac4 = QRACE) %>% mutate(n_white = rac4 != "White")

non_white$char_support3 <- non_white$Q23

des <- svydesign(id=~1, weights = ~nweight, data=non_white)

hh.non_white <- svyby( ~ char_support3, ~ n_white, design=des, svymean)

# White non_white fp Support
non_white <- hh %>% rename(rac4 = QRACE) %>% mutate(n_white = rac4 != "White")

non_white$foreign_support3 <- non_white$Q7

des <- svydesign(id=~1, weights = ~nweight, data=non_white)

hh.non_white <- svyby( ~ ec_support3, ~ n_white, design=des, svymean)

# White non_white fp Support
non_white <- hh %>% rename(rac4 = QRACE) %>% mutate(n_white = rac4 != "White")

non_white$daca_support3 <- non_white$Q10r3

des <- svydesign(id=~1, weights = ~nweight, data=non_white)

hh.non_white <- svyby( ~ daca_support3, ~ n_white, design=des, svymean)

# White non_white merit Support
non_white <- hh %>% rename(rac4 = QRACE) %>% mutate(n_white = rac4 != "White")

non_white$merit_support3 <- non_white$Q10r4

des <- svydesign(id=~1, weights = ~nweight, data=non_white)

hh.non_white <- svyby( ~ merit_support3, ~ n_white, design=des, svymean)

# White non_white visa Support
non_white <- hh %>% rename(rac4 = QRACE) %>% mutate(n_white = rac4 != "White")

non_white$visa_support3 <- non_white$Q10r7

des <- svydesign(id=~1, weights = ~nweight, data=non_white)

hh.non_white <- svyby( ~ visa_support3, ~ n_white, design=des, svymean)

# White non_white mueller Support
non_white <- hh %>% rename(rac4 = QRACE) %>% mutate(n_white = rac4 != "White")

non_white$mueller_support3 <- non_white$Q13

des <- svydesign(id=~1, weights = ~nweight, data=non_white)

hh.non_white <- svyby( ~ mueller_support3, ~ n_white, design=des, svymean)

# White non_white vehicle Support
non_white <- hh %>% rename(rac4 = QRACE) %>% mutate(n_white = rac4 != "White")

non_white$car_support3 <- non_white$Q13

des <- svydesign(id=~1, weights = ~nweight, data=non_white)

hh.non_white <- svyby( ~ car_support3, ~ n_white, design=des, svymean)

# White non_white health tax Support
non_white <- hh %>% rename(rac4 = QRACE) %>% mutate(n_white = rac4 != "White")

non_white$ht_support3 <- non_white$Q18r1

des <- svydesign(id=~1, weights = ~nweight, data=non_white)

hh.non_white <- svyby( ~ ht_support3, ~ n_white, design=des, svymean)

# White non_white health sub Support
non_white <- hh %>% rename(rac4 = QRACE) %>% mutate(n_white = rac4 != "White")

non_white$hs_support3 <- non_white$Q18r3

des <- svydesign(id=~1, weights = ~nweight, data=non_white)

hh.non_white <- svyby( ~ hs_support3, ~ n_white, design=des, svymean)

# White non_white planned parenthood Support
non_white <- hh %>% rename(rac4 = QRACE) %>% mutate(n_white = rac4 != "White")

non_white$pp_support3 <- non_white$Q18r7

des <- svydesign(id=~1, weights = ~nweight, data=non_white)

hh.non_white <- svyby( ~ pp_support3, ~ n_white, design=des, svymean)

# White non_white water inf Support
non_white <- hh %>% rename(rac4 = QRACE) %>% mutate(n_white = rac4 != "White")

non_white$inf_support3 <- non_white$Q19

des <- svydesign(id=~1, weights = ~nweight, data=non_white)

hh.non_white <- svyby( ~ inf_support3, ~ n_white, design=des, svymean)

# White non_white religious charter Support
non_white <- hh %>% rename(rac4 = QRACE) %>% mutate(n_white = rac4 != "White")

non_white$rc_support3 <- non_white$Q24

des <- svydesign(id=~1, weights = ~nweight, data=non_white)

hh.non_white <- svyby( ~ rc_support3, ~ n_white, design=des, svymean)

# White non_white oil Support
non_white <- hh %>% rename(rac4 = QRACE) %>% mutate(n_white = rac4 != "White")

non_white$oil_support3 <- non_white$Q25

des <- svydesign(id=~1, weights = ~nweight, data=non_white)

hh.non_white <- svyby( ~ oil_support3, ~ n_white, design=des, svymean)

# White non_white tax Support
non_white <- hh %>% rename(rac4 = QRACE) %>% mutate(n_white = rac4 != "White")

non_white$tax_support3 <- non_white$Q26

des <- svydesign(id=~1, weights = ~nweight, data=non_white)

hh.non_white <- svyby( ~ tax_support3, ~ n_white, design=des, svymean)

# White Rural fp Support
white_rural <- hh %>% rename(area = D107, rac3 = QRACE) %>% mutate(w_rural = rac3 == "White" & area == "Rural")

white_rural$fp_support3 <- white_rural$Q7

des <- svydesign(id=~1, weights = ~nweight, data=white_rural)

hh.white_rural <- svyby( ~ fp_support3, ~ w_rural, design=des, svymean)

# White Rural daca Support
white_rural <- hh %>% rename(area = D107, rac3 = QRACE) %>% mutate(w_rural = rac3 == "White" & area == "Rural")

white_rural$daca_support3 <- white_rural$Q10r3

des <- svydesign(id=~1, weights = ~nweight, data=white_rural)

hh.white_rural <- svyby( ~ daca_support3, ~ w_rural, design=des, svymean)

# White Rural daca Support
white_rural <- hh %>% rename(area = D107, rac3 = QRACE) %>% mutate(w_rural = rac3 == "White" & area == "Rural")

white_rural$merit_support3 <- white_rural$Q10r4

des <- svydesign(id=~1, weights = ~nweight, data=white_rural)

hh.white_rural <- svyby( ~ merit_support3, ~ w_rural, design=des, svymean)

# White Rural visa Support
white_rural <- hh %>% rename(area = D107, rac3 = QRACE) %>% mutate(w_rural = rac3 == "White" & area == "Rural")

white_rural$visa_support3 <- white_rural$Q10r7

des <- svydesign(id=~1, weights = ~nweight, data=white_rural)

hh.white_rural <- svyby( ~ visa_support3, ~ w_rural, design=des, svymean)

# White Rural mueller Support
white_rural <- hh %>% rename(area = D107, rac3 = QRACE) %>% mutate(w_rural = rac3 == "White" & area == "Rural")

white_rural$mueller_support3 <- white_rural$Q13

des <- svydesign(id=~1, weights = ~nweight, data=white_rural)

hh.white_rural <- svyby( ~ mueller_support3, ~ w_rural, design=des, svymean)

# White Rural vehicle Support
white_rural <- hh %>% rename(area = D107, rac3 = QRACE) %>% mutate(w_rural = rac3 == "White" & area == "Rural")

white_rural$car_support3 <- white_rural$Q16r4

des <- svydesign(id=~1, weights = ~nweight, data=white_rural)

hh.white_rural <- svyby( ~ car_support3, ~ w_rural, design=des, svymean)

# White Rural health tax Support
white_rural <- hh %>% rename(area = D107, rac3 = QRACE) %>% mutate(w_rural = rac3 == "White" & area == "Rural")

white_rural$ht_support3 <- white_rural$Q18r1

des <- svydesign(id=~1, weights = ~nweight, data=white_rural)

hh.white_rural <- svyby( ~ ht_support3, ~ w_rural, design=des, svymean)

# White Rural health sub Support
white_rural <- hh %>% rename(area = D107, rac3 = QRACE) %>% mutate(w_rural = rac3 == "White" & area == "Rural")

white_rural$hs_support3 <- white_rural$Q18r3

des <- svydesign(id=~1, weights = ~nweight, data=white_rural)

hh.white_rural <- svyby( ~ hs_support3, ~ w_rural, design=des, svymean)

# White Rural planned parenthood Support
white_rural <- hh %>% rename(area = D107, rac3 = QRACE) %>% mutate(w_rural = rac3 == "White" & area == "Rural")

white_rural$pp_support3 <- white_rural$Q18r7

des <- svydesign(id=~1, weights = ~nweight, data=white_rural)

hh.white_rural <- svyby( ~ pp_support3, ~ w_rural, design=des, svymean)

# White Rural water infra Support
white_rural <- hh %>% rename(area = D107, rac3 = QRACE) %>% mutate(w_rural = rac3 == "White" & area == "Rural")

white_rural$inf_support3 <- white_rural$Q19

des <- svydesign(id=~1, weights = ~nweight, data=white_rural)

hh.white_rural <- svyby( ~ inf_support3, ~ w_rural, design=des, svymean)

# White Rural reliogious charter Support
white_rural <- hh %>% rename(area = D107, rac3 = QRACE) %>% mutate(w_rural = rac3 == "White" & area == "Rural")

white_rural$rc_support3 <- white_rural$Q24

des <- svydesign(id=~1, weights = ~nweight, data=white_rural)

hh.white_rural <- svyby( ~ rc_support3, ~ w_rural, design=des, svymean)

# White Rural oil Support
white_rural <- hh %>% rename(area = D107, rac3 = QRACE) %>% mutate(w_rural = rac3 == "White" & area == "Rural")

white_rural$oil_support3 <- white_rural$Q25

des <- svydesign(id=~1, weights = ~nweight, data=white_rural)

hh.white_rural <- svyby( ~ oil_support3, ~ w_rural, design=des, svymean)

# White Rural tax Support
white_rural <- hh %>% rename(area = D107, rac3 = QRACE) %>% mutate(w_rural = rac3 == "White" & area == "Rural")

white_rural$tax_support3 <- white_rural$Q26

des <- svydesign(id=~1, weights = ~nweight, data=white_rural)

hh.white_rural <- svyby( ~ tax_support3, ~ w_rural, design=des, svymean)

# White nonrural daca Support
white_non_rural <- hh %>% rename(area = D107, rac3 = QRACE) %>% mutate(w_non_rural = rac3 == "White" & area != "Rural")

white_non_rural$daca_support3 <- white_non_rural$Q10r3
table(white_non_rural$daca_support3)

des <- svydesign(id=~1, weights = ~nweight, data=white_non_rural)

hh.white_non_rural <- svyby( ~ daca_support3, ~ w_non_rural, design=des, svymean)

# White nonrural merit Support
white_non_rural <- hh %>% rename(area = D107, rac3 = QRACE) %>% mutate(w_non_rural = rac3 == "White" & area != "Rural")

white_non_rural$merit_support3 <- white_non_rural$Q10r4
table(white_non_rural$merit_support3)

des <- svydesign(id=~1, weights = ~nweight, data=white_non_rural)

hh.white_non_rural <- svyby( ~ merit_support3, ~ w_non_rural, design=des, svymean)

# White nonrural visas Support
white_non_rural <- hh %>% rename(area = D107, rac3 = QRACE) %>% mutate(w_non_rural = rac3 == "White" & area != "Rural")

white_non_rural$visa_support3 <- white_non_rural$Q10r7
table(white_non_rural$visa_support3)

des <- svydesign(id=~1, weights = ~nweight, data=white_non_rural)

hh.white_non_rural <- svyby( ~ visa_support3, ~ w_non_rural, design=des, svymean)

# White nonrural mueller Support
white_non_rural <- hh %>% rename(area = D107, rac3 = QRACE) %>% mutate(w_non_rural = rac3 == "White" & area != "Rural")

white_non_rural$mueller_support3 <- white_non_rural$Q13
table(white_non_rural$mueller_support3)

des <- svydesign(id=~1, weights = ~nweight, data=white_non_rural)

hh.white_non_rural <- svyby( ~ mueller_support3, ~ w_non_rural, design=des, svymean)

# White nonrural vehicle Support
white_non_rural <- hh %>% rename(area = D107, rac3 = QRACE) %>% mutate(w_non_rural = rac3 == "White" & area != "Rural")

white_non_rural$car_support3 <- white_non_rural$Q16r4
table(white_non_rural$car_support3)

des <- svydesign(id=~1, weights = ~nweight, data=white_non_rural)

hh.white_non_rural <- svyby( ~ car_support3, ~ w_non_rural, design=des, svymean)

# White nonrural health tax Support
white_non_rural <- hh %>% rename(area = D107, rac3 = QRACE) %>% mutate(w_non_rural = rac3 == "White" & area != "Rural")

white_non_rural$ht_support3 <- white_non_rural$Q18r1
table(white_non_rural$ht_support3) 

des <- svydesign(id=~1, weights = ~nweight, data=white_non_rural)

hh.white_non_rural <- svyby( ~ ht_support3, ~ w_non_rural, design=des, svymean)

# White nonrural health sub Support
white_non_rural <- hh %>% rename(area = D107, rac3 = QRACE) %>% mutate(w_non_rural = rac3 == "White" & area != "Rural")

white_non_rural$hs_support3 <- white_non_rural$Q18r3
table(white_non_rural$hs_support3)

des <- svydesign(id=~1, weights = ~nweight, data=white_non_rural)

hh.white_non_rural <- svyby( ~ hs_support3, ~ w_non_rural, design=des, svymean)

# White nonrural planned parenthood Support
white_non_rural <- hh %>% rename(area = D107, rac3 = QRACE) %>% mutate(w_non_rural = rac3 == "White" & area != "Rural")

white_non_rural$pp_support3 <- white_non_rural$Q18r7
table(white_non_rural$pp_support3)

des <- svydesign(id=~1, weights = ~nweight, data=white_non_rural)

hh.white_non_rural <- svyby( ~ pp_support3, ~ w_non_rural, design=des, svymean)

# White nonrural water infrastructure Support
white_non_rural <- hh %>% rename(area = D107, rac3 = QRACE) %>% mutate(w_non_rural = rac3 == "White" & area != "Rural")

white_non_rural$inf_support3 <- white_non_rural$Q19
table(white_non_rural$inf_support3)

des <- svydesign(id=~1, weights = ~nweight, data=white_non_rural)

hh.white_non_rural <- svyby( ~ inf_support3, ~ w_non_rural, design=des, svymean)

# White nonrural religious charter Support
white_non_rural <- hh %>% rename(area = D107, rac3 = QRACE) %>% mutate(w_non_rural = rac3 == "White" & area != "Rural")

white_non_rural$rc_support3 <- white_non_rural$Q24
table(white_non_rural$rc_support3)

des <- svydesign(id=~1, weights = ~nweight, data=white_non_rural)

hh.white_non_rural <- svyby( ~ rc_support3, ~ w_non_rural, design=des, svymean)

# White nonrural oil Support
white_non_rural <- hh %>% rename(area = D107, rac3 = QRACE) %>% mutate(w_non_rural = rac3 == "White" & area != "Rural")

white_non_rural$oil_support3 <- white_non_rural$Q25
table(white_non_rural$oil_support3)

des <- svydesign(id=~1, weights = ~nweight, data=white_non_rural)

hh.white_non_rural <- svyby( ~ oil_support3, ~ w_non_rural, design=des, svymean)

# White nonrural tax Support
white_non_rural <- hh %>% rename(area = D107, rac3 = QRACE) %>% mutate(w_non_rural = rac3 == "White" & area != "Rural")

white_non_rural$tax_support3 <- white_non_rural$Q26
table(white_non_rural$tax_support3)

des <- svydesign(id=~1, weights = ~nweight, data=white_non_rural)

hh.white_non_rural <- svyby( ~ tax_support3, ~ w_non_rural, design=des, svymean)

# White suburb foreign Support
white_suburban <- hh %>% rename(area = D107, rac3 = QRACE) %>% mutate(w_suburban = rac3 == "White" & area == "Suburban")

white_suburban$fp_support3 <- white_suburban$Q7

des <- svydesign(id=~1, weights = ~nweight, data=white_suburban)

hh.white_suburban <- svyby( ~ fp_support3, ~ w_suburban, design=des, svymean)

# White urban foreign Support
white_urban <- hh %>% rename(area = D107, rac3 = QRACE) %>% mutate(w_urban = rac3 == "White" & area == "Urban")

white_urban$ec_support3 <- white_urban$Q7

des <- svydesign(id=~1, weights = ~nweight, data=white_urban)

hh.white_urban <- svyby( ~ ec_support3, ~ w_urban, design=des, svymean)

# White suburb foreign Support
white_suburban <- hh %>% rename(area = D107, rac3 = QRACE) %>% mutate(w_suburban = rac3 == "White" & area == "Suburban")

white_suburban$fp_support3 <- white_suburban$Q10r3

des <- svydesign(id=~1, weights = ~nweight, data=white_suburban)

hh.white_suburban <- svyby( ~ fp_support3, ~ w_suburban, design=des, svymean)

# White urban foreign Support
white_urban <- hh %>% rename(area = D107, rac3 = QRACE) %>% mutate(w_urban = rac3 == "White" & area == "Urban")

white_urban$ec_support3 <- white_urban$Q10r3

des <- svydesign(id=~1, weights = ~nweight, data=white_urban)

hh.white_urban <- svyby( ~ ec_support3, ~ w_urban, design=des, svymean)

# White suburb foreign Support
white_suburban <- hh %>% rename(area = D107, rac3 = QRACE) %>% mutate(w_suburban = rac3 == "White" & area == "Suburban")

white_suburban$fp_support3 <- white_suburban$Q10r4

des <- svydesign(id=~1, weights = ~nweight, data=white_suburban)

hh.white_suburban <- svyby( ~ fp_support3, ~ w_suburban, design=des, svymean)

# White urban foreign Support
white_urban <- hh %>% rename(area = D107, rac3 = QRACE) %>% mutate(w_urban = rac3 == "White" & area == "Urban")

white_urban$ec_support3 <- white_urban$Q10r4

des <- svydesign(id=~1, weights = ~nweight, data=white_urban)

hh.white_urban <- svyby( ~ ec_support3, ~ w_urban, design=des, svymean)

# White suburb foreign Support
white_suburban <- hh %>% rename(area = D107, rac3 = QRACE) %>% mutate(w_suburban = rac3 == "White" & area == "Suburban")

white_suburban$fp_support3 <- white_suburban$Q10r7

des <- svydesign(id=~1, weights = ~nweight, data=white_suburban)

hh.white_suburban <- svyby( ~ fp_support3, ~ w_suburban, design=des, svymean)

# White urban foreign Support
white_urban <- hh %>% rename(area = D107, rac3 = QRACE) %>% mutate(w_urban = rac3 == "White" & area == "Urban")

white_urban$ec_support3 <- white_urban$Q10r7

des <- svydesign(id=~1, weights = ~nweight, data=white_urban)

hh.white_urban <- svyby( ~ ec_support3, ~ w_urban, design=des, svymean)


# White suburb foreign Support
white_suburban <- hh %>% rename(area = D107, rac3 = QRACE) %>% mutate(w_suburban = rac3 == "White" & area == "Suburban")

white_suburban$fp_support3 <- white_suburban$Q13

des <- svydesign(id=~1, weights = ~nweight, data=white_suburban)

hh.white_suburban <- svyby( ~ fp_support3, ~ w_suburban, design=des, svymean)

# White urban foreign Support
white_urban <- hh %>% rename(area = D107, rac3 = QRACE) %>% mutate(w_urban = rac3 == "White" & area == "Urban")

white_urban$ec_support3 <- white_urban$Q13

des <- svydesign(id=~1, weights = ~nweight, data=white_urban)

hh.white_urban <- svyby( ~ ec_support3, ~ w_urban, design=des, svymean)


# White suburb foreign Support
white_suburban <- hh %>% rename(area = D107, rac3 = QRACE) %>% mutate(w_suburban = rac3 == "White" & area == "Suburban")

white_suburban$fp_support3 <- white_suburban$Q16r4

des <- svydesign(id=~1, weights = ~nweight, data=white_suburban)

hh.white_suburban <- svyby( ~ fp_support3, ~ w_suburban, design=des, svymean)

# White urban foreign Support
white_urban <- hh %>% rename(area = D107, rac3 = QRACE) %>% mutate(w_urban = rac3 == "White" & area == "Urban")

white_urban$ec_support3 <- white_urban$Q16r4

des <- svydesign(id=~1, weights = ~nweight, data=white_urban)

hh.white_urban <- svyby( ~ ec_support3, ~ w_urban, design=des, svymean)


# White suburb foreign Support
white_suburban <- hh %>% rename(area = D107, rac3 = QRACE) %>% mutate(w_suburban = rac3 == "White" & area == "Suburban")

white_suburban$fp_support3 <- white_suburban$Q18r1

des <- svydesign(id=~1, weights = ~nweight, data=white_suburban)

hh.white_suburban <- svyby( ~ fp_support3, ~ w_suburban, design=des, svymean)

# White urban foreign Support
white_urban <- hh %>% rename(area = D107, rac3 = QRACE) %>% mutate(w_urban = rac3 == "White" & area == "Urban")

white_urban$ec_support3 <- white_urban$Q18r1

des <- svydesign(id=~1, weights = ~nweight, data=white_urban)

hh.white_urban <- svyby( ~ ec_support3, ~ w_urban, design=des, svymean)

# White suburb foreign Support
white_suburban <- hh %>% rename(area = D107, rac3 = QRACE) %>% mutate(w_suburban = rac3 == "White" & area == "Suburban")

white_suburban$fp_support3 <- white_suburban$Q18r3

des <- svydesign(id=~1, weights = ~nweight, data=white_suburban)

hh.white_suburban <- svyby( ~ fp_support3, ~ w_suburban, design=des, svymean)

# White urban foreign Support
white_urban <- hh %>% rename(area = D107, rac3 = QRACE) %>% mutate(w_urban = rac3 == "White" & area == "Urban")

white_urban$ec_support3 <- white_urban$Q18r3

des <- svydesign(id=~1, weights = ~nweight, data=white_urban)

hh.white_urban <- svyby( ~ ec_support3, ~ w_urban, design=des, svymean)

# White suburb foreign Support
white_suburban <- hh %>% rename(area = D107, rac3 = QRACE) %>% mutate(w_suburban = rac3 == "White" & area == "Suburban")

white_suburban$fp_support3 <- white_suburban$Q18r7

des <- svydesign(id=~1, weights = ~nweight, data=white_suburban)

hh.white_suburban <- svyby( ~ fp_support3, ~ w_suburban, design=des, svymean)

# White urban foreign Support
white_urban <- hh %>% rename(area = D107, rac3 = QRACE) %>% mutate(w_urban = rac3 == "White" & area == "Urban")

white_urban$ec_support3 <- white_urban$Q18r7

des <- svydesign(id=~1, weights = ~nweight, data=white_urban)

hh.white_urban <- svyby( ~ ec_support3, ~ w_urban, design=des, svymean)

# White suburb foreign Support
white_suburban <- hh %>% rename(area = D107, rac3 = QRACE) %>% mutate(w_suburban = rac3 == "White" & area == "Suburban")

white_suburban$fp_support3 <- white_suburban$Q19

des <- svydesign(id=~1, weights = ~nweight, data=white_suburban)

hh.white_suburban <- svyby( ~ fp_support3, ~ w_suburban, design=des, svymean)

# White urban foreign Support
white_urban <- hh %>% rename(area = D107, rac3 = QRACE) %>% mutate(w_urban = rac3 == "White" & area == "Urban")

white_urban$ec_support3 <- white_urban$Q19

des <- svydesign(id=~1, weights = ~nweight, data=white_urban)

hh.white_urban <- svyby( ~ ec_support3, ~ w_urban, design=des, svymean)

# White suburb foreign Support
white_suburban <- hh %>% rename(area = D107, rac3 = QRACE) %>% mutate(w_suburban = rac3 == "White" & area == "Suburban")

white_suburban$fp_support3 <- white_suburban$Q24

des <- svydesign(id=~1, weights = ~nweight, data=white_suburban)

hh.white_suburban <- svyby( ~ fp_support3, ~ w_suburban, design=des, svymean)

# White urban foreign Support
white_urban <- hh %>% rename(area = D107, rac3 = QRACE) %>% mutate(w_urban = rac3 == "White" & area == "Urban")

white_urban$ec_support3 <- white_urban$Q24

des <- svydesign(id=~1, weights = ~nweight, data=white_urban)

hh.white_urban <- svyby( ~ ec_support3, ~ w_urban, design=des, svymean)

# White suburb foreign Support
white_suburban <- hh %>% rename(area = D107, rac3 = QRACE) %>% mutate(w_suburban = rac3 == "White" & area == "Suburban")

white_suburban$fp_support3 <- white_suburban$Q25

des <- svydesign(id=~1, weights = ~nweight, data=white_suburban)

hh.white_suburban <- svyby( ~ fp_support3, ~ w_suburban, design=des, svymean)

# White urban foreign Support
white_urban <- hh %>% rename(area = D107, rac3 = QRACE) %>% mutate(w_urban = rac3 == "White" & area == "Urban")

white_urban$ec_support3 <- white_urban$Q25

des <- svydesign(id=~1, weights = ~nweight, data=white_urban)

hh.white_urban <- svyby( ~ ec_support3, ~ w_urban, design=des, svymean)


# White suburb foreign Support
white_suburban <- hh %>% rename(area = D107, rac3 = QRACE) %>% mutate(w_suburban = rac3 == "White" & area == "Suburban")

white_suburban$fp_support3 <- white_suburban$Q26

des <- svydesign(id=~1, weights = ~nweight, data=white_suburban)

hh.white_suburban <- svyby( ~ fp_support3, ~ w_suburban, design=des, svymean)

# White urban foreign Support
white_urban <- hh %>% rename(area = D107, rac3 = QRACE) %>% mutate(w_urban = rac3 == "White" & area == "Urban")

white_urban$ec_support3 <- white_urban$Q26

des <- svydesign(id=~1, weights = ~nweight, data=white_urban)

hh.white_urban <- svyby( ~ ec_support3, ~ w_urban, design=des, svymean)