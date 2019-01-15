library(haven)
library(tidyverse)
library(ggplot2)
library(survey)
library(ggthemes)

WhitePop <- read_csv("Documents - Harvard University/2018-2019/September-December/American Public Opinion/Final Memo/White Population 2016 - Sheet1.csv")

names(WhitePop) <- c("Region", "Percent", "Category")
WhitePop$Score <- as.numeric(as.character(WhitePop$Percent))
WhitePop <- WhitePop[order(-WhitePop$Percent),]
WhitePop$Region <- factor(WhitePop$Region, levels= WhitePop$Region[order(WhitePop$Percent)]) 

ggplot(WhitePop) + geom_col(aes(x=Region, y=Percent, color = Category, width = .5)) + theme_minimal() +
  labs(caption="State Data from 2016 CCES, USA Data from 2016 ACS \n (demographic weights applied)") +
  ggtitle("White Population of Swing States compared to USA at-large") +
  geom_hline(yintercept=61.3, linetype="dashed", 
             color = "red", size=1)

