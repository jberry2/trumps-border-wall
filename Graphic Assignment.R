install.packages("tidyverse")
install.packages("survey")
install.packages("ggthemes")

library(haven)
library(tidyverse)
library(ggplot2)
library(survey)
library(ggthemes)

hh <- read_sav("Documents - Harvard University/2018-2019/September-December/American Public Opinion/Final Memo/2018-11_White-House.sav")

hh <- as_factor(hh)





des <- svydesign(id=~1, weights = ~nweight, data=hh)

hh.border <- svyby(~ border, ~ party, design=des, svymean)

hh.border


# dot chart
ggplot(hh.border, aes(x= border *100, y=party)) + 
  geom_point() +
  geom_errorbarh(aes(xmin=(border - 1.96*se)*100, 
                     xmax=(border + 1.96*se)*100), 
             height =.3, position=position_dodge(.6)) + 
  xlab("Percent") + ylab("") +
  labs(caption="Data from Harvard-Harris poll, Nov 2018-White House \n (demographic weights applied)") +
  ggtitle("Strong Support for US-Mexico Border Wall, by Party ID") +
  xlim(0,100) +
  theme_minimal() +
  geom_vline(xintercept=50, lty=2, col="grey")

