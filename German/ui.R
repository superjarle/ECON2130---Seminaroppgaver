library(shiny)
library(ggplot2)


# Define UI for application that draws a histogram
ui <- fluidPage(
  
  # Application title
  titlePanel("German Tank Problem"),
  
  # Sidebar with a slider input for number of bins 
  sidebarLayout(
    sidebarPanel(
      
      selectInput("estimator",
                  "choice of estimator",
                  choices = c("max", "min + max","mean + 2sd", "max + max/n -1")),
      
      numericInput("sample_size", 
                   "Number of tanks captured:", 
                   min=1, 
                   max="population_size", 
                   value = 5),
      
      numericInput("population_size",
                   "Actual Number of Tanks (hypothetical):",
                   min="sample_size",
                   max=1000,
                   value=342),
      
      numericInput("iterations", 
                   "Number of iterations:",
                   min=100,
                   max=9999, 
                   value = 1000),
      
      sliderInput("bins",
                  "Number of bins:",
                  min = 1,
                  max = 50,
                  value = 30),
      
    ),
    
    # Show a plot of the generated distribution
    mainPanel(
      plotOutput("distPlot"),
      "Bias of estimate:", textOutput("bias"),
      
      "Standard deviation of estimate", textOutput("se")
    )
  )
)