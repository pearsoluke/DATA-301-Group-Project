library(dplyr)
library(tidyr)
library(tsibble)
library(lubridate)
library(ggplot2)

COVIDRaw <- read.csv("countries-aggregated.csv")

COVID <- COVIDRaw %>% group_by(Country) %>% mutate(Date = as.Date(Date))

## Add new cases/deaths/recovered

addNewColumn <- function(df){
  NewConfirmed <- 0
  NewDeaths <- 0
  NewRecoveries <- 0
  
  x <- length(df$Date)
  
  for(i in 1:x){
    if(i == 1){
      NewConfirmed[i] = 0
      NewDeaths[i] = df$Deaths[i]
      NewRecoveries[i] = df$Confirmed[i]
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
