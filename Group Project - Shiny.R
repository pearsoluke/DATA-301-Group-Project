library(shiny)
library(dplyr)

ui <- fluidPage(
  sidebarPanel(
    uiOutput("Country"),
    actionButton("Go", "Go")
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


server <- function(input, output){
  
  COVIDRaw <- read.csv("countries-aggregated.csv")
  COVID <- COVIDRaw %>% group_by(Country) %>% mutate(Date = as.Date(Date))
  countriesList <- unique(COVID$Country) 
  
  output$Country <- renderUI({
    selectInput("Country", "Select A Country", choices = countriesList)
  })
  
  observeEvent(input$Go,{
    COVIDCountry <- COVID %>% filter(Country == input$Country)
    COVIDCountry <- unique(COVIDCountry)
    COVIDCountry <- addNewColumn(COVIDCountry)
    
    TotalConfirmedTitle <- paste("Daily confirmed cases in", input$Country)
    TotalConfirmedY <- "Total Cases"
    
    TotalDeathsTitle <- paste("Daily deaths in", input$Country)
    TotalDeathsY <- "Total Deaths"
    
    DailyConfirmedTitle <- paste("Daily confirmed cases in", input$Country)
    DailyConfirmedY <- "Daily Cases"
    
    DailyDeathsTitle <- paste("Daily deaths in", input$Country)
    DailyDeathsY <- "Daily Deaths"
    
    xText <- "Date"
    
    output$TotalConfirmed <- renderPlot({
      plot(COVIDCountry$Date, COVIDCountry$Confirmed, type = 'l', main = TotalConfirmedTitle, xlab = xText, ylab = TotalConfirmedY)
    })
    
    output$TotalDeaths <- renderPlot({
      plot(COVIDCountry$Date, COVIDCountry$Deaths, type = 'l', main = TotalDeathsTitle, xlab = xText, ylab = TotalDeathsY)
    })
    
    output$DailyConfirmed <- renderPlot({
      plot(COVIDCountry$Date, COVIDCountry$NewConfirmed, type = 'l', main = DailyConfirmedTitle, xlab = xText, ylab = DailyConfirmedY)
    })
    
    output$DailyDeaths <- renderPlot({
      plot(COVIDCountry$Date, COVIDCountry$NewDeaths, type = 'l', main = DailyDeathsTitle, xlab = xText, ylab = DailyDeathsY)
    })
  })
}

shinyApp(ui =  ui, server = server)

