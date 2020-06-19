# Replication file for: "Beating the Bookies"
# RPubs-link: https://rpubs.com/mstefan-rpubs
# (c) Martin Stefan, June 2020

# PART I: PREMIER LEAGUE ONLY, NO COMPOUND INTEREST

# clear workspace
rm(list = ls())

# load packages
library(tidyverse)

# import data
df <- read.csv("data/E0.csv")
df <- df %>% select(Date,HomeTeam,AwayTeam,FTR,BbMxH,BbMxD,BbMxA)
head(df)

# compute implied probability
df$ImplProb <- 1/df$BbMxH + 1/df$BbMxD + 1/df$BbMxA
df$Margin <- 1 - df$ImplProb
df$ShouldBet <- ifelse(df$Margin > 0, 1, 0)
head(df %>% select(-BbMxH,-BbMxD,-BbMxA))

# visualize implied probabilities
plot(df$ImplProb ~ c(1:nrow(df)), 
     ylab = "Implied probability", 
     xlab = "Season games")
abline(h = 1, col = "red") # break even line

# how many profitable games?
sum(df$ShouldBet)

# place bets (ignoring minor rounding errors)
bet <- 10
df$BetH <- round(ifelse(df$ShouldBet == 1, bet * 1/df$BbMxH * (1+df$Margin), 0), 2)
df$BetD <- round(ifelse(df$ShouldBet == 1, bet * 1/df$BbMxD * (1+df$Margin), 0), 2)
df$BetA <- round(ifelse(df$ShouldBet == 1, bet * 1/df$BbMxA * (1+df$Margin), 0), 2)
head(df %>% select(-BbMxH,-BbMxD,-BbMxA,-Margin,-ShouldBet))

# calculate earnings
df$WinH <- round(ifelse(df$FTR == "H",1,0) * df$BetH * df$BbMxH,2)
df$WinD <- round(ifelse(df$FTR == "D",1,0) * df$BetD * df$BbMxD,2) 
df$WinA <- round(ifelse(df$FTR == "A",1,0) * df$BetA * df$BbMxA,2)
df$WinSum <- df$WinH + df$WinD + df$WinA

# calculate profits
df$Profits <- df$WinSum - df$BetH - df$BetD - df$BetA

# how much money made in total?
sum(df$Profits)