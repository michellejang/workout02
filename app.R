#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(ggplot2)
library(dplyr)
library(tidyr)

# Define UI for application that draws a histogram
ui <- fluidPage(
  fluidRow(
    
    # Application title
    titlePanel("Saving/Investment Scenarios"),
    
    # Sidebar with a slider input for number of bins \
column(4,
        sliderInput(inputId = "initial", 
                    label = "Initial Investment", 
                    value = 1000, min = 1, max = 100000, step = 500),
        sliderInput(inputId = "annual",
                    label = "Annual Contribution",
                    value = 2000, min = 0, max = 50000, step = 500)
      ),

column(4,
        sliderInput(inputId = "return", 
                    label = "Return Rate (in %)", 
                    value = 5, min = 0, max = 20, step = 0.1),
        
        sliderInput(inputId = "growth",
                    label = "Growth Rate (in %)",
                    value = 2, min = 0, max = 20, step = 0.1)
      ),

column(4,
      sliderInput(inputId = "years",
                  label = "Years",
                  value = 20, min = 0, max = 50, step = 1),
      
        selectInput(inputId = "facet", 
                    label = "Facet?",
                    choices = c("No", "Yes"))
      )

  ),
fluidRow(
  titlePanel("Timelines"),
  plotOutput("returns")
),

fluidRow(
  titlePanel("Balances"),
    tableOutput("balances")
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  dat <- reactive({
   
   future_value <- function(amount = 0, rate = 0, years = 0){
     invest <- amount*(1+rate)^years
     return(invest)
   }
   
   annuity <- function(contrib = 0, rate = 0, years= 0){
     balance <- contrib*(((1 + rate)^years -1)/rate)
     return(balance)
   }
   
   growing_annuity <- function(contrib = 0, rate = 0, growth = 0, years = 0){
     balance_1 <- contrib*(((1 + rate)^years - (1 + growth)^years)/(rate - growth))
     return(balance_1)
   }
   
   future_values <- rep(0, input$years)
   for (i in 0:input$years){
     future_values[i+1]=future_value(amount = input$initial, rate = input$return, years = i)
   }
   
   annuities <- rep(0, input$years)
   for (i in 0:input$years){
     annuities[i+1]=future_values[i+1] + annuity(contrib = input$annual, rate = input$return, years = i)
   }
   
   growing_annuities <- rep(0, input$years)
   for (i in 0:input$years){
     growing_annuities[i+1]=future_values[i+1] + growing_annuity(contrib = input$annual, rate = input$return, growth = input$growth, years = i)
   }
  
   
   dat <- data.frame(year = 0:input$years,
                     no_contrib = future_values,
                     fixed_contrib = annuities,
                     growing_contrib = growing_annuities)
   return(dat)
  })
  
  dat_long <- reactive({
    dat_long <- gather(dat(), key = "measure", value = "value", c("no_contrib", "fixed_contrib", "growing_contrib"))
  })
  
  plot1 <- reactive({
    ggplot(data = dat()) + geom_line(aes(x = year, y = no_contrib, color = "no_contrib")) + geom_line(aes(x = year, y = fixed_contrib, color = "fixed_contrib")) + geom_line(aes(x = year, y = growing_contrib,color = "growing_contrib")) + xlab("Years") + ylab("value") + scale_color_manual("Variable", breaks = c("no_contrib", "fixed_contrib", "growing_contrib"), values = c("green", "blue", "red")) + ggtitle("Three modes of investing")
  })
  
  plot2 <- reactive({
    ggplot(data = dat_long(), aes(x = year, y = value, color = measure)) + geom_line() + geom_point(size = 1) + geom_area() + facet_wrap(~measure) + scale_color_manual("Variable", breaks = c("no_contrib", "fixed_contrib", "growing_contrib"), values = c("green", "blue", "red")) + ggtitle("Three modes of investing")
  })
  
  graphInput <- reactive({
    switch(input$facet, 
           "No" = plot1(),
           "Yes" = plot2())
  })
  
  output$returns <- renderPlot({
    graphInput()
  })
  output$balances <- renderTable({
    dat()
  })
}



# Run the application 
shinyApp(ui = ui, server = server)

