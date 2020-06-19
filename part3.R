# Replication file for: "Beating the Bookies"
# RPubs-link: https://rpubs.com/mstefan-rpubs
# (c) Martin Stefan, June 2020

# PART II: ALL LEAGUES AND COMPOUND INTEREST

# clear workspace
rm(list = ls())

# load packages
library(tidyverse)

# import first .csv file
df <- read.csv("data/D1.csv")

# loop through and rbind the other .csv files
for(i in c("D2","E0","E1","E2","E3","SP1","SP2","I1")){
  
  # load csv
  path <- paste("data/",i,".csv",sep="")
  df_new <- read.csv(path)
  # drop "Referee" columns (not part of all datasets)
  if(ncol(df_new)==62) df_new <- df_new[,-which(names(df_new)=="Referee")]
  df <- rbind(df,df_new)
}
rm(i,path,df_new)
df <- df %>% select(Date,HomeTeam,AwayTeam,FTR,BbMxH,BbMxD,BbMxA)
df$Date <- as.Date(as.character(df$Date),"%d/%m/%Y")

# sort data by date
df <- df[order(df$Date),]

# compute implied probability
df$ImplProb <- 1/df$BbMxH + 1/df$BbMxD + 1/df$BbMxA
df$Margin <- 1 - df$ImplProb
df$ShouldBet <- ifelse(df$Margin > 0, 1, 0)

# remove unprofitable games
df <- df %>% filter(ShouldBet == 1) 
nrow(df)