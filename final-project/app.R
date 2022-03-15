#########################################################################################
# Final Project Web App
# This is a completed application that is ready for hosting.
# This product is the type of thing your data workforce will deliver.
# This goal of seeing this process is to tie together all
# of the components we have seen over the course
# 1. data pipelines provide data that we can then store for later use
# 2. we can use that data for insight and model development
# 3. those models can then be used for decision support 
# In this case it is helping with the question should the
# winemaker harvest?
# don't worry about the details behind each component of this tool
# instead see how a fully developed concept can be deployed and available
# for an end user to leverage.
########################################################################################

# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

########################################################################################
# To host a Shiny App, we need to use the shiny packages
########################################################################################

library(shiny)
library(shinydashboard)
library(plyr)
library(tidyverse)
library(ggplot2)

########################################################################################
# Reading in our prediction data to the application
########################################################################################

prediction <- readRDS("data/prediction.RData")

########################################################################################
# Shiny applications have 2 main components, the UI and the logic
# Here we define our UI and use the package Shiny daschboards to help create everything
########################################################################################

ui <- dashboardPage(skin = "green",
                    dashboardHeader(title = "Decision Support Tool Winemakers Dilemma"),
                    dashboardSidebar(
                      collapsed = TRUE,
                      sidebarMenu(
                        menuItem("Decision App", tabName = "decision", icon = icon("dashboard"))
                      )
                    ),
                    dashboardBody(
                      tabItem(tabName = "decision",
                              # Here is an example viz with the controls of interest
                              # you can change this as much as you like
                              fluidRow(
                                tabBox(width=4,
                                       #title = "Inputs", #status = "success", #solidHeader = TRUE,
                                       tabPanel("Probabilities", 
                                                sliderInput("PRCP", "Precipitation last week", 0,2,0.5, step = 0.1),
                                                sliderInput("TMAX", "Max temperature last week", 0,100,65, step = 5),
                                                sliderInput("mold", "Probability of mold given Storm", 0,1,0.4, step = 0.1)
                                       ),
                                       tabPanel("Other",
                                                sliderInput("cost25", "Retail Price per bottle with 25% sugar", 0,50,35, step = 5),
                                                sliderInput("cost20", "Retail Price per bottle with 20% sugar", 0,50,30, step = 5),
                                                sliderInput("cost19", "Retail Price per bottle with 19% sugar", 0,50,25, step = 5),
                                                sliderInput("costbot", "Retail Price per bottle Late Harvest", 0,100,80, step = 10),
                                                sliderInput("rep_plus", "Reputation Increase from Late Harvest Wine", 0,500000,150000, step = 50000)
                                                #sliderInput("prob_19", "Probability Obtaining less than 19% sugar without a Storm", 0,1,0.2, step = 0.1),
                                                #sliderInput("cases", "Number of cases normally made", 500,1500,1000, step = 100)
                                       )
                                ),
                                box(width=8,
                                    title = "Results", status = "success", solidHeader = TRUE,
                                    plotOutput("distPlot"),
                                    HTML(paste("<center><font size = 6>",textOutput('text'), "</font></center>")))
                              )
                      )
                    )
                    
)

########################################################################################
# Shiny applications have 2 main components, the UI and the logic
# Here we define our server logic, we do all calculations and plots in this section
########################################################################################

server <- function(input, output) {
  
output$distPlot <- renderPlot({
   
    #a simple variable for the best alternative 
    winner <- plot_dataframe() %>% 
                filter(Best==TRUE)
    
    #Create a nice plot for the application
    ggplot(data = plot_dataframe(), aes(x=Name, y=Value, fill = Best)) +
      geom_hline(yintercept = 0, colour = "black") +
      geom_col() + 
      scale_fill_manual(values = c('red','#00a65a')) +
      ylab("Expected Utility") + 
      xlab("") +
      labs(title = paste0("The best alternative is to ",winner$Name[1])) +
      geom_text(aes(label = paste0(round(plot_dataframe()$Value,2),"K")), 
                size = 5, 
                position = position_stack(vjust = 0.5), 
                colour = "white") +
      theme_minimal() +
      theme(plot.title = element_text(size=22))
      
    
  })

plot_dataframe <- reactive({
  df <- data.frame(Value = c(NA,NA), 
                   Name = c("Harvest Now", "Harvest Later"))
  bot_case <- 12
  price_harvest_now <- 28.5
  bot_red <- 0.7
  thin_aug <- 1.075
  thin_price <- 10
  prob_25 <- 0.4
  prob_20 <- 0.4
  prob_19 <- 0.2
  cases <- 1000
  
  week40 <- data.frame("PRCPt1" = input$PRCP,
                       "TMAXt1" = input$TMAX)
  
  prob <-predict(prediction, type = 'response', newdata = week40)
  p_rain <- as.numeric(prob)
  
  df[1,1] <- (cases*bot_case*price_harvest_now)/1000
  
  lateh_prospect <- ((input$costbot*cases*bot_case*bot_red) + input$rep_plus)/1000
  thin_prospect <- (thin_aug*thin_price*bot_case*cases)/1000
  prospect_25 <- input$cost25*bot_case*cases/1000
  prospect_20 <- input$cost20*bot_case*cases/1000
  prospect_19 <- input$cost19*bot_case*cases/1000
  
  df[2,1] <- (((prospect_25*prob_25) + 
                 (prospect_20*prob_20) + 
                 (prospect_19*prob_19))*(1-(p_rain))) +
                 ((lateh_prospect*input$mold) + 
                 (thin_prospect*(1-input$mold)))*p_rain
  
  df <- df %>% 
    mutate(Best = ifelse(Value == max(Value), TRUE, FALSE))
  
  return(df)
})

}


# Run the application 
shinyApp(ui = ui, server = server)

