library(haven)
library(tidyverse)
library(ggplot2)
library(survey)
library(ggthemes)

WhiteCharter <- read_csv("Documents - Harvard University/2018-2019/September-December/American Public Opinion/Final Memo/White Area Charter School Support  - Sheet1.csv")

names(WhiteCharter) <- c("Demographic", "Percent", "n")
WhiteCharter$Percent <- as.numeric(as.character(WhiteCharter$Percent))
WhiteCharter <- WhiteCharter[order(-WhiteCharter$Percent),]
WhiteCharter$Demographic <- factor(WhiteCharter$Demographic, levels= WhiteCharter$Demographic[order(WhiteCharter$Percent)]) 

# create new column for standard errors
WhiteCharter$se <- sqrt(WhiteCharter$Percent*(1-WhiteCharter$Percent)/WhiteCharter$n)

ggplot(WhiteCharter, aes(x=Demographic, y=Percent*100)) + geom_point() + 
  geom_errorbar(aes(ymin = (Percent-1.96*se)*100, ymax = (Percent+1.96*se)*100, width =.3)) + 
  coord_flip() + theme_minimal() +
  xlab("") + ylab("Percent") +
  labs(caption="Data from Harvard-Harris poll, Nov 2018-White House \n (demographic weights applied)") +
  ggtitle("Strong Support for Opening Charter Schools, by Demographic") +
  geom_hline(yintercept=25.2, linetype="dashed", 
             color = "grey", size=1)