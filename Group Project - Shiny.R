library(shiny)
library(dplyr)

ui <- fluidPage(
  sidebarPanel(
    uiOutput("Country"),
    actionButton("Go", "Go")
  ),
  mainPanel(
    plotOutput("confirmed"),
    plotOutput("deaths"),
    plotOutput("recovered"),
  )
)


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
    
    confirmedTitle <- paste("Daily confirmed cases in", input$Country)
    confirmedY <- "Cases"
    deathsTitle <- paste("Daily deaths in", input$Country)
    deathsY <- "Deaths"
    recoveredTitle <- paste("Daily recovered in", input$Country)
    recoveredY <- "Recovered"
    
    xText <- "Date"
    
    
    output$confirmed <- renderPlot({
      plot(COVIDCountry$Date, COVIDCountry$NewConfirmed, type = 'l', main = confirmedTitle, xlab = xText, ylab = confirmedY)
    })
    
    output$deaths <- renderPlot({
      plot(COVIDCountry$Date, COVIDCountry$NewDeaths, type = 'l', main = deathsTitle, xlab = xText, ylab = deathsY)
    })
    
    #output$recovered <- renderPlot({
    #  plot(COVIDCountry$Date, COVIDCountry$NewRecovered, type = 'l', main = recoveredTitle, xlab = xText, ylab = recoveredY)
    #})
    
  })
}

shinyApp(ui =  ui, server = server)

