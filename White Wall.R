install.packages("rmarkdown")


library(haven)
library(tidyverse)
library(ggplot2)
library(survey)
library(ggthemes)

WhiteWall <- read_csv("Documents - Harvard University/2018-2019/September-December/American Public Opinion/Final Memo/White Area Wall Support - Sheet1.csv")

names(WhiteWall) <- c("Demographic", "Percent", "n")
WhiteWall$Percent <- as.numeric(as.character(WhiteWall$Percent))
WhiteWall <- WhiteWall[order(-WhiteWall$Percent),]
WhiteWall$Demographic <- factor(WhiteWall$Demographic, levels= WhiteWall$Demographic[order(WhiteWall$Percent)]) 

# create new column for standard errors
WhiteWall$se <- sqrt(WhiteWall$Percent*(1-WhiteWall$Percent)/WhiteWall$n)

ggplot(WhiteWall, aes(x=Demographic, y=Percent*100)) + geom_point() + 
  geom_errorbar(aes(ymin = (Percent-1.96*se)*100, ymax = (Percent+1.96*se)*100, width =.3)) + 
  coord_flip() + theme_minimal() +
  xlab("") + ylab("Percent") +
  labs(caption="Data from Harvard-Harris poll, Nov 2018-White House \n (demographic weights applied)") +
  ggtitle("Strong Support for US-Mexico Border Wall, by Demographic") +
  geom_hline(yintercept=17.7, linetype="dashed", 
             color = "grey", size=.5)