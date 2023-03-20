#
# Dette er en "Shiny" app, som fungerer slik at dere ??pner et nytt vindu i R-Studio
# Dere kjorer ved aa kjorre den siste linjen i appen
#
# det kan hende dere maa installere "shiny" sjekk evnt feilmelding.  :) 

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

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  output$distPlot <- renderPlot({
    
    
    samp_dist <- rep(NA,input$iterations)
    
    for (i in seq(input$iterations)) {
      sample_data <- sample(x=seq(input$population_size), 
                            size=input$sample_size, 
                            replace = FALSE)
      
      samp_dist[i] <- if(input$estimator == "min + max"){
        min(sample_data)+max(sample_data)
        
      } else if(input$estimator == "mean + 2sd"){
        mean(sample_data)+2*sd(sample_data)
        
      } else if (input$estimator == "max + max/n -1"){
        max(sample_data)+max(sample_data)/input$sample_size-1
      }
      else if (input$estimator == "max"){
        max(sample_data)
      }
    }
    
    estimate <- mean(samp_dist)
    se <- sd(samp_dist)
    output$bias <- renderText(estimate-input$population_size)
    output$se <-renderText(se) 
    samp_dist <- as.data.frame(samp_dist)
    
    
    
    
    # draw the histogram with the specified number of bins
    
    
    ggplot(samp_dist)+
      geom_histogram(aes(x=samp_dist, y = ..density..), bins = input$bins, fill="gray")+
      geom_vline(aes(color="Actual", xintercept = input$population_size), lwd=1, lty="dashed")+
      geom_vline(aes(color="Estimate", xintercept = estimate), lwd=2, lty="dotted")+
      theme_light()+
      xlab("Number of Tanks")+
      ylab("Frequency")+
      scale_color_manual(name= "KEY", values=c(Actual="blue", Estimate="pink"))
  })
  
  
  
}

# Denne linjen starter applikasjonen
shinyApp(ui = ui, server = server)
