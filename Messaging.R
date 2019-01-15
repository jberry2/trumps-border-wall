library(haven)
library(tidyverse)
library(ggplot2)
library(survey)
library(ggthemes)

DTmessage <- read_csv("Documents - Harvard University/2018-2019/September-December/American Public Opinion/Final Memo/DT 2020 Messaging Edges - Sheet1.csv")


names(DTmessage) <- c("Disagree", "Score", "Category")
DTmessage$Score <- as.numeric(as.character(DTmessage$Score))
DTmessage <- DTmessage[order(-DTmessage$Score),]
DTmessage$Disagree <- factor(DTmessage$Disagree, levels= DTmessage$Disagree[order(DTmessage$Score)]) 

ggplot(DTmessage) + geom_point(aes(x=Disagree, y=Score, color=Category)) + coord_flip() + scale_color_brewer(palette="Dark2") + theme_minimal() +
  xlab("Disagree that Dems") + ylab("Percent") +
  labs(caption="Data from Harvard-Harris poll Nov 2018, Democratic Congressional \n (demographic weights applied)") +
  ggtitle("Economic Messaging Scores Best") 

