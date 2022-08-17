library(dplyr)
library(tidyr)
library(tsibble)
library(lubridate)
library(ggplot2)
library(scales)

install.packages("scales")

## Add new cases/deaths/recovered

addNewColumn <- function(df){
  NewConfirmed <- 0
  NewDeaths <- 0
  NewRecoveries <- 0
  
  x <- length(df$Date)
  
  for(i in 1:x){
    if(i == 1){
      NewConfirmed[i] = 0
      NewDeaths[i] = 0
      NewRecoveries[i] = 0
    }
    if(i > 1){
      NewConfirmed[i] = df$Confirmed[i] - df$Confirmed[(i - 1)]
      NewDeaths[i] = df$Deaths[i] - df$Deaths[(i - 1)]
      NewRecoveries[i] = df$Recovered[i] - df$Recovered[(i - 1)]
    }
  }
  df <- df %>% mutate(NewConfirmed = NewConfirmed, NewDeaths = NewDeaths, NewRecovered = NewRecoveries)
  return(df)
}

COVIDRaw <- read.csv("countries-aggregated.csv")
COVID <- COVIDRaw %>% group_by(Country) %>% mutate(Date = as.Date(Date))

COVIDCountry <- COVID %>% filter(Country == "Afghanistan")
COVIDCountry <- addNewColumn(COVIDCountry)

SevenDayAverage <- function(df){
Confirmed7 <- 0
Deaths7 <- 0
Recovered7 <- 0
  
NewConfirmed7 <- 0
NewDeaths7 <- 0
NewRecovered7 <- 0

x <- length(df$Date)

for(i in 1:x){
Confirmed7[i] <- mean(df$Confirmed[max(1, i - 3):min(x, i + 3)])
Deaths7[i] <- mean(df$Deaths[max(1, i - 7):min(x, i + 3)])
Recovered7[i] <- mean(df$Recovered[max(1, i - 7):min(x, i + 3)])  
  
NewConfirmed7[i] <- mean(df$NewConfirmed[max(1, i - 3):min(x, i + 3)])
NewDeaths7[i] <- mean(df$NewDeaths[max(1, i - 7):min(x, i + 3)])
NewRecovered7[i] <- mean(df$NewRecovered[max(1, i - 7):min(x, i + 3)])
}

df <- df %>% mutate(Confirmed7 = Confirmed7, Deaths7 = Deaths7, Recovered7 = Recovered7, 
                    NewConfirmed7 = NewConfirmed7, NewDeaths7 = NewDeaths7, NewRecovered7 = NewRecovered7)
}
