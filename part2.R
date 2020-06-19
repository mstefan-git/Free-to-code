# Replication file for: "Beating the Bookies"
# RPubs-link: https://rpubs.com/mstefan-rpubs
# (c) Martin Stefan, June 2020

# PART II: PREMIER LEAGUE ONLY, BUT WITH COMPOUND INTEREST

# clear workspace
rm(list = ls())

# load packages
library(tidyverse)

# import data
df <- read.csv("data/E0.csv")
df <- df %>% select(Date,HomeTeam,AwayTeam,FTR,BbMxH,BbMxD,BbMxA)
df$Date <- as.Date(as.character(df$Date),"%d/%m/%Y")

# compute implied probability
df$ImplProb <- 1/df$BbMxH + 1/df$BbMxD + 1/df$BbMxA
df$Margin <- 1 - df$ImplProb
df$ShouldBet <- ifelse(df$Margin > 0, 1, 0)

# remove unprofitable games
df <- df %>% filter(ShouldBet == 1) 

# initial amount of money
initial = 10
money = c(initial)

# loop through each day in the season
dates <- unique(df$Date)
for(day in dates){
  
  # how much money available by now?
  bet <- money[length(money)]
  
  # select most profitable game of the day
  df_day <- df %>% 
    filter(Date == day) %>%
    filter(ImplProb == min(ImplProb))
  
  # place bet
  df_day$BetH <- round(bet * 1/df_day$BbMxH * (1+df_day$Margin), 2)
  df_day$BetD <- round(bet * 1/df_day$BbMxD * (1+df_day$Margin), 2)
  df_day$BetA <- round(bet * 1/df_day$BbMxA * (1+df_day$Margin), 2)
  
  # calculate earnings
  df_day$WinH <- round(ifelse(df_day$FTR == "H",1,0) * df_day$BetH * df_day$BbMxH,2)
  df_day$WinD <- round(ifelse(df_day$FTR == "D",1,0) * df_day$BetD * df_day$BbMxD,2) 
  df_day$WinA <- round(ifelse(df_day$FTR == "A",1,0) * df_day$BetA * df_day$BbMxA,2)
  df_day$WinSum <- df_day$WinH + df_day$WinD + df_day$WinA
  
  # update money
  money <- c(money,df_day$WinSum)
  
  
} # end loop

# plot money over time
plot(money[-1] ~ unique(df$Date), type="l", ylab="Money", xlab="Time")

