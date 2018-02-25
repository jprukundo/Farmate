#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#

library(shiny)
library(shinydashboard)
library(DT)
library(forecast)

#Load and clean data
murundi_data = na.omit(read.csv("Murundi04.csv", skip = 2))
ndego_data = na.omit(read.csv("Ndego03.csv", skip = 2))
rwinkwavu_data = na.omit(read.csv("Rwinkwavu01.csv", skip = 2))
test_data = na.omit(read.csv("Test terminal.csv", skip = 2))

#convert to time series
murundi_ts = as.ts(murundi_data)  
ndego_ts = as.ts(ndego_data)
rwinkwavu_ts = as.ts(rwinkwavu_data)
test_ts = as.ts(test_data)


# Define UI for application 
ui <- dashboardPage(
  dashboardHeader(title = "Climate App Demo"),
  dashboardSidebar(
    sidebarMenu(
      menuItem("Murundi", tabName = "murundi", icon = icon("arrow-right")),
      menuItem("Ndego", tabName = "ndego", icon = icon("arrow-right")),
      menuItem("Rwinkwavu", tabName = "rwinkwavu", icon = icon("arrow-right")),
      menuItem("Test terminal", tabName = "test", icon = icon("arrow-right"))
    )
    
  ),
  dashboardBody(
    tabItems(
      # Murundi tab content
      tabItem(tabName = "murundi",
              fluidRow(
                box(sliderInput("slider_murundi_temp_ts", "Number of observations:", 1, length(murundi_ts[,2]), length(murundi_ts[,2]))),
                box(plotOutput("plot_murundi_temp_ts", height = 250)),
                box(sliderInput("slider_murundi_humi_ts", "Number of observations:", 1, length(murundi_ts[,3]), length(murundi_ts[,3]))),
                box(plotOutput("plot_murundi_humi_ts", height = 250)),
                numericInput("murundi_forcast_temp", "Forcast Temperature", 10, min = 1, max = length(murundi_ts[,2])), 
                numericInput("murundi_forcast_humi", "Forcast Humidity:", 10, min = 1, max = length(murundi_ts[,3])), 
                tabsetPanel(
                  id = 'table_murundi',
                  tabPanel("Temperature Forcast", DT::dataTableOutput("table_murundi_temp")),
                  tabPanel("Humidity Forcast", DT::dataTableOutput("table_murundi_humi"))
                )
                
              )
      ),
      
      # Ndego tab content
      tabItem(tabName = "ndego",
              fluidRow(
                box(sliderInput("slider_ndego_temp_ts", "Number of observations:", 1, length(ndego_ts[,2]), length(ndego_ts[,2]))),
                box(plotOutput("plot_ndego_temp_ts", height = 250)),
                box(sliderInput("slider_ndego_humi_ts", "Number of observations:", 1, length(ndego_ts[,3]), length(ndego_ts[,3]))),
                box(plotOutput("plot_ndego_humi_ts", height = 250))
                
              )
      ),
      
      # Rwinkwavu tab content
      tabItem(tabName = "rwinkwavu",
              fluidRow(
                box(sliderInput("slider_rwinkwavu_temp_ts", "Number of observations:", 1, length(rwinkwavu_ts[,2]), length(rwinkwavu_ts[,2]))),
                box(plotOutput("plot_rwinkwavu_temp_ts", height = 250)),
                box(sliderInput("slider_rwinkwavu_humi_ts", "Number of observations:", 1, length(rwinkwavu_ts[,3]), length(rwinkwavu_ts[,3]))),
                box(plotOutput("plot_rwinkwavu_humi_ts", height = 250))
                
              )
      ),
      
      # Test tab content
      tabItem(tabName = "test",
              fluidRow(
                box(sliderInput("slider_test_temp_ts", "Number of observations:", 1, length(test_ts[,2]), length(test_ts[,2]))),
                box(plotOutput("plot_test_temp_ts", height = 250)),
                box(sliderInput("slider_test_humi_ts", "Number of observations:", 1, length(test_ts[,3]), length(test_ts[,3]))),
                box(plotOutput("plot_test_humi_ts", height = 250))
                
              )
      )
    )
  )
)

server <- function(input, output) {
  set.seed(122)
 
  #Times series plot
  output$plot_murundi_temp_ts <- renderPlot({
    data <- murundi_ts[seq_len(input$slider_murundi_temp_ts), 2]
    plot(data, main="Murundi Temperature Data",col="red")
  })
  output$plot_murundi_humi_ts <- renderPlot({
    data <- murundi_ts[seq_len(input$slider_murundi_humi_ts), 3]
    plot(data, main="Murundi Humidity Data", col="blue")
  })
  
  output$plot_ndego_temp_ts <- renderPlot({
    data <- ndego_ts[seq_len(input$slider_ndego_temp_ts), 2]
    plot(data, main="Ndego Temperature Data",col="red")
  })
  output$plot_ndego_humi_ts <- renderPlot({
    data <- ndego_ts[seq_len(input$slider_ndego_humi_ts), 3]
    plot(data, main="Ndego Humidity Data", col="blue")
  })
  
  output$plot_rwinkwavu_temp_ts <- renderPlot({
    data <- rwinkwavu_ts[seq_len(input$slider_rwinkwavu_temp_ts), 2]
    plot(data, main="Rwinkwavu Temperature Data",col="red")
  })
  output$plot_rwinkwavu_humi_ts <- renderPlot({
    data <- rwinkwavu_ts[seq_len(input$slider_rwinkwavu_humi_ts), 3]
    plot(data, main="Rwinkwavu Humidity Data", col="blue")
  })
  
  output$plot_test_temp_ts <- renderPlot({
    data <- test_ts[seq_len(input$slider_test_temp_ts), 2]
    plot(data, main="Test Temperature Data",col="red")
  })
  output$plot_test_humi_ts <- renderPlot({
    data <- test_ts[seq_len(input$slider_test_humi_ts), 3]
    plot(data, main="Test Humidity Data", col="blue")
  })
  
  #Forcast table
  output$table_murundi_temp <- DT::renderDataTable({
    as.data.frame(forecast(auto.arima(murundi_ts[,2]),input$murundi_forcast_temp))
  })
  output$table_murundi_humi <- DT::renderDataTable({
    as.data.frame(forecast(auto.arima(murundi_ts[,3]),input$murundi_forcast_humi))
  })
  
  output$plot_ndego_temp_ts <- renderPlot({
    data <- ndego_ts[seq_len(input$slider_ndego_temp_ts), 2]
    plot(data, main="Ndego Temperature Data",col="red")
  })
  output$plot_ndego_humi_ts <- renderPlot({
    data <- ndego_ts[seq_len(input$slider_ndego_humi_ts), 3]
    plot(data, main="Ndego Humidity Data", col="blue")
  })
  
  output$plot_rwinkwavu_temp_ts <- renderPlot({
    data <- rwinkwavu_ts[seq_len(input$slider_rwinkwavu_temp_ts), 2]
    plot(data, main="Rwinkwavu Temperature Data",col="red")
  })
  output$plot_rwinkwavu_humi_ts <- renderPlot({
    data <- rwinkwavu_ts[seq_len(input$slider_rwinkwavu_humi_ts), 3]
    plot(data, main="Rwinkwavu Humidity Data", col="blue")
  })
  
  output$plot_test_temp_ts <- renderPlot({
    data <- test_ts[seq_len(input$slider_test_temp_ts), 2]
    plot(data, main="Test Temperature Data",col="red")
  })
  output$plot_test_humi_ts <- renderPlot({
    data <- test_ts[seq_len(input$slider_test_humi_ts), 3]
    plot(data, main="Test Humidity Data", col="blue")
  })

}

# Run the application 
shinyApp(ui, server)

