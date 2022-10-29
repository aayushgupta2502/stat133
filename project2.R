# ===============================================
# Fill in the following fields
# ===============================================
# Title: Retirement Withdrawal Simulator
# Description: Creates an app which can assist financial planners in understanding the behavior of their clients'
# portfolios after retirement, taking into account initial savings, variable return, and inflation rates.
# Author: Aayush Gupta
# Date: 11/12/2021


# ===============================================
# Required packages
# ===============================================
library(tidyverse)
library(shiny)
library(dplyr)
library(rsconnect)

rsconnect::setAccountInfo(name='aayushguptaberkeley',
                          token='8806BA27A93F689331F391F850EE74AC',
                          secret='Ls4iVuoXxlz8vt5kFHQn7aajXJ4wyA74rFIkZL6b')
# ...



# ===============================================
# Define UserInterface "ui" for application
# ===============================================

ui <- fluidPage(
  
  titlePanel("Retirement Withdrawal Simulator"),
  fluidRow(
    # Inputs for initial portfolio, retirement age, and withdrawal rate
    column(3,
           numericInput(inputId = 'savings',
                       label = 'Initial Portfolio:',
                       value = 1000000),
           sliderInput(inputId = 'age',
                       label = 'Retirement Age:',
                       value = 60, min = 18, max = 100),
           numericInput(inputId = 'wrate',
                       label = 'Withdrawal Rate:',
                       value = 0.04, min = 0)
           # delete this line and replace it with your widgets!!!
    ),
    
    # Inputs for mean and standard deviation of annual return rates
    column(3,
           numericInput(inputId = 'rateOfReturn',
                       label = 'Average Annual Rate of Return:',
                       value = 0.10),
           numericInput(inputId = 'rateStdDev',
                       label = 'Rate of Return Standard Deviation',
                       value = 0.18)
           # delete this line and replace it with your widgets!!!
    ),
    
    # Inputs for mean and standard deviation of annual inflation rates
    column(3,
           numericInput(inputId = 'infRate',
                       label = 'Average Inflation Rate:',
                       value = 0.03),
           numericInput(inputId = 'avgInfVol',
                       label = "Average Inflation Volatility",
                       value = 0.035)
           
           # delete this line and replace it with your widgets!!!
    ),
    
    # Inputs for number of simulations, and random seed
    column(3,
           radioButtons(inputId = 'simNum',
                       label = 'Number of Simulations:',
                       choices = c(25, 50, 75, 100),
                       selected = 50),
                       
           numericInput(inputId = 'setSeed',
                        label = 'Set Seed Number:',
                        value = 123)# delete this line and replace it with your widgets!!!
    )
  ),
  
  hr(),
  h4('Portfolio Balance Over Time'),
  plotOutput('plot'),
  
  hr(),
  h4('Portfolio Balance Summary'),
  dataTableOutput('table')
  
)



# ===============================================
# Define Server "server" logic
# ===============================================

server <- function(input, output) {

  # you may need to create reactive objects
  # (e.g. data frame to be used for graphing purposes)
  set.seed(123)
  df <- reactive({
    # replace the code below with your code!!!
    matrix = c()
    years = 100 - input$age
    
    for (i in 1:input$simNum) {
      balance = input$savings
      vec = c(balance)
      
      for (j in 2:years) {
        interest_rate = rnorm(1, input$rateOfReturn, input$rateStdDev)
        inflation_rate = rnorm(1, input$infRate, input$avgInfVol)
        
        current = (balance) * (1 + interest_rate) - ((input$savings * input$wrate) * (1 + inflation_rate))
        vec = c(vec, current)
        balance = current
      }
      
      matrix = cbind(matrix, vec)
      
      
    }
    
    year = 0:years
    colnames(matrix) = paste0('simulation', 1:input$simNum)
    matrix = cbind(matrix, year)
    
    dat = as.data.frame(matrix)
    
    sim_dat = pivot_longer(
      dat,
      cols = starts_with('simulation'),
      names_to = 'simulation',
      values_to = 'amount'
    )
    
    sim_dat
    
  })
  
  
  # code for graph
  # (e.g. reactive data frame used for graphing purposes)
  output$plot <- renderPlot({
    # replace the code below with your code!!!
    ggplot(data = df(), aes(x = year, y = amount, group = simulation)) + geom_point() + geom_line(aes(color = simulation)) + xlab('years after retirement') + ylab('portfolio balance in millions')
  })
  
  
  # code for statistics
  output$table <- renderDataTable({
    # replace the code below with your code!!!
    #df()
    
    groupedDf = group_by(df(), year)
    
    summarise(groupedDf, 'Minimum' = round(min(amount), 2),
              'Maximum' = round(max(amount), 2),
              'Median' = round(median(amount), 2),
              'Mean' = round(mean(amount), 2),
              'Std Dev' = round(sd(amount), 2),
              '25th Percentile' = round(quantile(amount, probs = 0.25), 2),
              '50th Percentile' = round(quantile(amount, probs = 0.5), 2),
              '75th Percentile' = round(quantile(amount, probs = 0.75), 2),
              '90th Percentile' = round(quantile(amount, probs = 0.90), 2))
  
  })

}



# ===============================================
# Run the application
# ===============================================

shinyApp(ui = ui, server = server)

