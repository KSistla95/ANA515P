#install.packages("tidyverse",repos = "http://cran.us.r-project.org")
#install.packages("knitr",repos = "http://cran.us.r-project.org")
#install.packages("bslib",repos = "http://cran.us.r-project.org")

library(tidyverse)
library(knitr)
library(bslib)
library(stringr)
library(openxlsx)
library(dplyr, warn = FALSE)
library(lubridate)
library(ggplot2)

# Set working directory and load data
setwd("C:/Users/DELL/Desktop/Masters/04P. ANA 515P Exp Practicum Fundamentals")
data_sheet1 <- read_csv("nfl_elo_latest_sheet1.csv")
data_sheet2 <- read_csv("nfl_elo_latest_sheet2.csv")


# Combine both datasets to ease the data cleaning
data <- rbind(data_sheet1, data_sheet2)

# convert date into yyyy-mm-dd format 
data$date <- mdy(c(data$date))


data$season <- year(data$date)

# some of the columns contain a $ prefix for numbers. Remove the prefix
data$elo_prob1 <- str_replace_all(data$elo_prob1, "\\$", "")

# use the row data from dataframe for which there is valid data. 4 th column is "playoff", removed from this filter. 
datatrial <- data[complete.cases(data[,1:3]), ]
datatrial <- datatrial[complete.cases(datatrial[,5:30]), ]

# Probabilites should be less than 1. Filter out those which are mistakenly entered more than 1.
datatrial <- datatrial[which(datatrial$elo_prob1<1&datatrial$elo_prob2<1&datatrial$qbelo_prob1<1&datatrial$qbelo_prob2<1),]


# Graph 1 is related to histogram of mean pre match team ELO distributions 
graph1 <- aggregate(datatrial$elo1_pre, list(datatrial$team1), FUN=mean)
colnames(graph1) <- c('NFL_Team','Mean_ELO_Prematch') 

ggplot(data=graph1, aes(x=NFL_Team, y=Mean_ELO_Prematch)) + geom_bar(stat="identity")+
  labs(y="Mean ELO Prematch", x = "NFL Team")


# Graph 2 is related to scatter plot of Mean pre match team elo against mean prematch Quarter back ELO.
graph2 <- aggregate(datatrial$qbelo1_pre, list(datatrial$team1), FUN=mean)
colnames(graph2) <- c('NFL_Team','Mean_ELO_QB_Prematch') 

graph2 <- merge(graph1,graph2,by="NFL_Team", all = TRUE)

ggplot(graph2, aes(x=Mean_ELO_Prematch, y=Mean_ELO_QB_Prematch)) + geom_point() + geom_smooth(method=lm) + 
  labs(y="Mean QB ELO Prematch", x = "Mean Team ELO Prematch")

# Export cleaned dataset into csv file
write.csv(datatrial, file = "nfl_data_cleaned.csv")
