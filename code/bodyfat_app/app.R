#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
source("./model.R")

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Body Fat % Predictor"),

    # Sidebar with numeric input for parameters 
    sidebarLayout(
        sidebarPanel(
            numericInput("weight", 
                         "Weight:", 
                         150, 
                         min = 50, 
                         max = 500),
            numericInput("abdomen", 
                         "Abdomen (in):", 
                         30, 
                         min = 10, 
                         max = 75)
                    ),

        # Show a plot of the generated distribution
        mainPanel(
           textOutput("bodyfat_prediction"),
           textOutput("prediction_interval")
        )
    )
)
model <- create_model()

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$bodyfat_prediction <- renderText({
        # Generate Bodyfat estimate based on values
        input_df <- data.frame("ABDOMEN"=input$abdomen,"WEIGHT" = input$weight)
        fat_prediction <- predict(model, input_df)
        paste("Predicted Body Fat: ", round(fat_prediction,3),"%", sep="")
    })
    output$prediction_interval <- renderText({
      # Generate Bodyfat estimate based on values
      input_df <- data.frame("ABDOMEN"=input$abdomen,"WEIGHT" = input$weight)
      fat_prediction <- predict(model, input_df, interval = "prediction")
      paste("95% predictive interval: [",
            round(fat_prediction[2],3),",",
            round(fat_prediction[3],3),
            "]",
            sep="")
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)