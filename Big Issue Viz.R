library(haven)
library(tidyverse)
library(ggplot2)
library(survey)
library(ggthemes)

DTissues <- read_csv("Documents - Harvard University/2018-2019/September-December/American Public Opinion/Final Memo/DT 2020 Issue Campaign Edges - Sheet1 (1).csv")

hh <- as_factor(hh)
names(DTissues) <- c("Issue", "Score", "Category")
DTissues$Score <- as.numeric(as.character(DTissues$Score))
DTissues <- DTissues[order(-DTissues$Percent),]
DTissues$Issue <- factor(DTissues$Issue, levels= DTissues$Issue[order(DTissues$Score)]) 

ggplot(DTissues) + geom_point(aes(x=Issue, y=Score, color=Category)) + coord_flip() + scale_color_brewer(palette="Dark2") + theme_minimal() +
  labs(caption="Data from Harvard-Harris poll Nov 2018 series \n (demographic weights applied)") +
  ggtitle("Some Issues Score Better with White Voters") 

