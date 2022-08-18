library(shiny)
library(dplyr)
library(tidyr)
library(tsibble)
library(lubridate)
library(ggplot2)
library(scales)
library(seasonal)
library(ggfortify)
library(forecast)

ui <- fluidPage(
  sidebarPanel(
    uiOutput("Country"),
    dateRangeInput("dateRange", "Select Date Range"),
    radioButtons("dateInterval", "Select Date Interval", choiceNames = c("Weekly", "Monthly", "Quarterly"), choiceValues = c("week", "month", "quarter"), selected = "month"),
    checkboxInput("Seven", "Tick to add 7 day moving averages"),
    uiOutput("Go")
  ),
  mainPanel(
    tabsetPanel(
      tabPanel("Total Confirmed",
               plotOutput("TCLine"),
               plotOutput("TCSeasonal"),
               plotOutput("TCSpider")
      ),
      tabPanel("Daily Confirmed",
               plotOutput("DCLine"),
               plotOutput("DCSeasonal"),
               plotOutput("DCSpider")
      ),
      tabPanel("Total Deaths",
               plotOutput("TDLine"),
               plotOutput("TDSeasonal"),
               plotOutput("TDSpider")
      ),
      tabPanel("Daily Deaths",
               plotOutput("DDLine"),
               plotOutput("DDSeasonal"),
               plotOutput("DDSpider")
      )
    ))
)


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
    if(input$dateInterval == 'quarter'){
      dateIntervals <- dateIntervals %>% mutate(xLabels = paste0(year(dateIntervals[,]), " Q", quarter(dateIntervals[,])))
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
    
    # 7 Day average
    x7 <- c()
    yTC7 <- c()
    yTD7 <- c()
    yDC7 <- c()
    yDD7 <- c()
    
    if(input$Seven){
      x7 <- COVIDCountry$Date
      yTC7 <- COVIDCountry$Confirmed7
      yTD7 <- COVIDCountry$Deaths7
      yDC7 <- COVIDCountry$NewConfirmed7
      yDD7 <- COVIDCountry$NewDeaths7
    }
    # Time Series objects
    tsTC <- ts(COVIDCountry$Confirmed, start = c(2020, 1), frequency = 365)
    tsTD <- ts(COVIDCountry$Deaths, start = c(2020, 1), frequency = 365)
    tsDC <- ts(COVIDCountry$NewConfirmed, start = c(2020, 1), frequency = 365)
    tsDD <- ts(COVIDCountry$NewDeaths, start = c(2020, 1), frequency = 365)
    
    # Plotting
    #Total Confirmed Cases
    output$TCLine <- renderPlot({
        # Main Plot
        plot(x = COVIDCountry$Date, COVIDCountry$Confirmed, type = 'l', xlab = "", ylab = "", axes = FALSE, col = alpha('black', 0.6))
        # 7 Day average
        lines(x7, yTC7, type = 'l', col = 4, lwd = 2)
        # Axis scales
        axis.Date(side = 1, at = dateIntervals$xDates, labels = dateIntervals$xLabels, las = 2)
        axis(side = 2, at = yRangeTC, labels = yLabelTC, las = 3)
        # Labels
        title(main = TotalConfirmedTitle)
        mtext(text = xText, side = 1, line = -1)
        mtext(text = TotalConfirmedY, side = 2, line = 2)
        # Grid
        grid(nx = length(dateIntervals[,1]), ny = length(yRangeTC), lty = 2, col = "gray", lwd = 1)
        # Legend
        if(input$Seven){
        legend('topleft', '7 Day average', lty = 1, col = 4, lwd = 2)
        }
    })
    
    output$TCSeasonal <- renderPlot({
      ggseasonplot(tsTC)
    })
    
    output$TCSpider <- renderPlot({
      ggseasonplot(tsTC, polar = TRUE)
    })
    
    # Total Deaths    
    output$TDLine <- renderPlot({
        # Main Plot
        plot(x = COVIDCountry$Date, COVIDCountry$Deaths, type = 'l', xlab = "", ylab = "", axes = FALSE, col = alpha('black', 0.6))
        # 7 Day average
        lines(x7, yTD7, type = 'l', col = 4, lwd = 2)
        # Axis scales
        axis.Date(side = 1, at = dateIntervals$xDates, labels = dateIntervals$xLabels, las = 2)
        axis(side = 2, at = yRangeTD, labels = yLabelTD, las = 3)
        # Labels
        title(main = TotalDeathsTitle)
        mtext(text = xText, side = 1, line = -1)
        mtext(text = TotalDeathsY, side = 2, line = 2)
        # Grid
        grid(nx = length(dateIntervals[,1]), ny = length(yRangeTD), lty = 2, col = "gray", lwd = 1)
        # Legend
        if(input$Seven){
          legend('topleft', '7 Day average', lty = 1, col = 4, lwd = 2)
        }
        
    })
    
    output$TDSeasonal <- renderPlot({
      ggseasonplot(tsTD)
    })
    
    output$TDSpider <- renderPlot({
      ggseasonplot(tsTD, polar = TRUE)
    })
    
    # Daily Confirmed Cases    
    output$DCLine <- renderPlot({
        # Main Plot
        plot(x = COVIDCountry$Date, COVIDCountry$NewConfirmed, type = 'l', xlab = "", ylab = "", axes = FALSE, col = alpha('black', 0.6))
        # 7 day average
        lines(x7, yDC7, type = 'l', col = 4, lwd = 2)
        # Axis scales
        axis.Date(side = 1, at = dateIntervals$xDates, labels = dateIntervals$xLabels, las = 2)
        axis(side = 2, at = yRangeDC, labels = yLabelDC, las = 3)
        # Labels
        title(main = DailyConfirmedTitle)
        mtext(text = xText, side = 1, line = -1)
        mtext(text = DailyConfirmedY, side = 2, line = 2)
        # Grid
        grid(nx = length(dateIntervals[,1]), ny = length(yRangeDC), lty = 2, col = "gray", lwd = 1)
        # Legend
        if(input$Seven){
          legend('topleft', '7 Day average', lty = 1, col = 4, lwd = 2)
        }
    })
    
    output$DCSeasonal <- renderPlot({
      ggseasonplot(tsDC)
    })
    
    output$DCSpider <- renderPlot({
      ggseasonplot(tsDC, polar = TRUE)
    })
    
    # Daily Deaths   
    output$DDLine <- renderPlot({
        # Main plot
        plot(x = COVIDCountry$Date, COVIDCountry$NewDeaths, type = 'l', xlab = "", ylab = "", axes = FALSE, col = alpha('black', 0.6))
        # 7 Day average
        lines(x7, yDD7, type = 'l', col = 4, lwd = 2)
        # Axis scales
        axis.Date(side = 1, at = dateIntervals$xDates, labels = dateIntervals$xLabels, las = 1)
        axis(side = 2, at = yRangeDD, labels = yLabelDD, las = 3)
        # Labels
        title(main = DailyDeathsTitle)
        mtext(text = xText, side = 1, line = -1)
        mtext(text = DailyDeathsY, side = 2, line = 2)
        # Grid
        grid(nx = length(dateIntervals[,1]), ny = length(yRangeDD), lty = 2, col = "gray", lwd = 1)
        # Legend
        if(input$Seven){
          legend('topleft', '7 Day average', lty = 1, col = 4, lwd = 2)
        }
    })
    
    output$DDSeasonal <- renderPlot({
      ggseasonplot(tsDD)
    })
    
    output$DDSpider <- renderPlot({
      ggseasonplot(tsDD, polar = TRUE)
    })
  })
}

shinyApp(ui =  ui, server = server)
