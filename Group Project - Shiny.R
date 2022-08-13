library(shiny)
library(dplyr)
library(tidyr)
library(tsibble)
library(lubridate)
library(ggplot2)

ui <- fluidPage(
  sidebarPanel(
    uiOutput("Country"),
    dateRangeInput("dateRange", "Select Date Range"),
    radioButtons("dateInterval", "Select Date Interval", choiceNames = c("Weekly", "Monthly"), choiceValues = c("week", "month"), selected = "month"),
    uiOutput("Go")
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Total",
               plotOutput("TotalConfirmed"),
               plotOutput("TotalDeaths")
      ),
      tabPanel("Daily",
               plotOutput("DailyConfirmed"),
               plotOutput("DailyDeaths")
      )
    )))


server <- function(input, output, session){
  
  COVIDRaw <- read.csv("countries-aggregated.csv")
  COVID <- COVIDRaw %>% group_by(Country) %>% mutate(Date = as.Date(Date))
  countriesList <- unique(COVID$Country) 
  
  output$Country <- renderUI({
    selectInput("Country", "Select A Country", choices = countriesList)
  })
  output$Go <- renderUI({
    actionButton("Go", "Go")
  })
  
  updateDateRangeInput(session, "dateRange", min = min(COVID$Date), max = max(COVID$Date), start = min(COVID$Date), end = max(COVID$Date))
  
  observeEvent(input$Go,{
    COVIDCountry <- COVID %>% filter(Country == input$Country)
    COVIDCountry <- unique(COVIDCountry)
    COVIDCountry <- addNewColumn(COVIDCountry)
    COVIDCountry <- COVIDCountry %>% filter(Date %in% input$dateRange[1]:input$dateRange[2])
    
    # Set scale for x-axis
    dateIntervals <- data.frame(xDates = seq(as.Date(min(COVIDCountry$Date)), as.Date(max(COVIDCountry$Date)), by = input$dateInterval))
    if(input$dateInterval == 'week'){
      dateIntervals <- dateIntervals %>% mutate(xLabels = format(xDates, '%d %b'))
    }
    if(input$dateInterval == 'month'){
      dateIntervals <- dateIntervals %>% mutate(xLabels = format(xDates, "%b '%y"))
    }
    
    # Set scale for y-axis
    yRangeTC <- pretty(COVIDCountry$Confirmed)
    yRangeTD <- pretty(COVIDCountry$Deaths)
    yRangeDC <- pretty(COVIDCountry$NewConfirmed)
    yRangeDD <- pretty(COVIDCountry$NewDeaths)
    
    yLabelTC <- format(yRangeTC, big.mark = ",", scientific = FALSE)
    yLabelTD <- format(yRangeTD, big.mark = ",", scientific = FALSE)
    yLabelDC <- format(yRangeDC, big.mark = ",", scientific = FALSE)
    yLabelDD <- format(yRangeDD, big.mark = ",", scientific = FALSE)
    
    # Set labels
    TotalConfirmedTitle <- paste("Total confirmed cases in", input$Country)
    TotalConfirmedY <- "Total Cases"
    
    TotalDeathsTitle <- paste("Total deaths in", input$Country)
    TotalDeathsY <- "Total Deaths"
    
    DailyConfirmedTitle <- paste("Daily confirmed cases in", input$Country)
    DailyConfirmedY <- "Daily Cases"
    
    DailyDeathsTitle <- paste("Daily deaths in", input$Country)
    DailyDeathsY <- "Daily Deaths"
    
    xText <- paste("Date by", input$dateInterval)
    
    # Plotting
    output$TotalConfirmed <- renderPlot({
      #Total Confirmed Cases
      plot(x = COVIDCountry$Date, COVIDCountry$Confirmed, type = 'l', xlab = "", ylab = "", axes = FALSE)
      axis.Date(side = 1, at = dateIntervals$xDates, labels = dateIntervals$xLabels, las = 2)
      axis(side = 2, at = yRangeTC, labels = yLabelTC, las = 3)
      title(main = TotalConfirmedTitle)
      mtext(text = xText, side = 1, line = -1)
      mtext(text = TotalConfirmedY, side = 2, line = 2)
    })
    
    output$TotalDeaths <- renderPlot({
      # Total Deaths
      plot(x = COVIDCountry$Date, COVIDCountry$Deaths, type = 'l', xlab = "", ylab = "", axes = FALSE)
      axis.Date(side = 1, at = dateIntervals$xDates, labels = dateIntervals$xLabels, las = 2)
      axis(side = 2, at = yRangeTD, labels = yLabelTD, las = 3)
      title(main = TotalDeathsTitle)
      mtext(text = xText, side = 1, line = -1)
      mtext(text = TotalDeathsY, side = 2, line = 2)
    })
    
    output$DailyConfirmed <- renderPlot({
      # Daily Confirmed Cases
      plot(x = COVIDCountry$Date, COVIDCountry$NewConfirmed, type = 'l', xlab = "", ylab = "", axes = FALSE)
      axis.Date(side = 1, at = dateIntervals$xDates, labels = dateIntervals$xLabels, las = 2)
      axis(side = 2, at = yRangeDC, labels = yLabelDC, las = 3)
      title(main = DailyConfirmedTitle)
      mtext(text = xText, side = 1, line = -1)
      mtext(text = DailyConfirmedY, side = 2, line = 2)
    })
    
    output$DailyDeaths <- renderPlot({
      # Daily Deaths
      plot(x = COVIDCountry$Date, COVIDCountry$NewDeaths, type = 'l', xlab = "", ylab = "", axes = FALSE)
      axis.Date(side = 1, at = dateIntervals$xDates, labels = dateIntervals$xLabels, las = 2)
      axis(side = 2, at = yRangeDD, labels = yLabelDD, las = 3)
      title(main = DailyDeathsTitle)
      mtext(text = xText, side = 1, line = -1)
      mtext(text = DailyDeathsY, side = 2, line = 2)
    })
  })
}

shinyApp(ui =  ui, server = server)
