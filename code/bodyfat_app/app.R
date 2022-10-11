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
source("./model.R")

# Helper functions
valid_input <- function(abdomen, weight) {
    # Verify positive numeric values
    if(abdomen <=0 || weight <= 0) {
        return(FALSE)
    }
    
    return(is.numeric(abdomen) && is.numeric(weight))
}

model <- create_model()

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Body Fat % Predictor"),

    # Sidebar with numeric input for parameters 
    sidebarLayout(
        sidebarPanel(
            numericInput("weight", 
                         "Weight (lbs):", 
                         150, 
                         min = 50, 
                         max = 500),
            numericInput("abdomen", 
                         "Abdomen (in):", 
                         30, 
                         min = 10, 
                         max = 75),
            submitButton()
                    ),
        
        # Show a plot of the generated distribution
        mainPanel(
            span(textOutput("warning_input"), style="color:red"),
            span(textOutput("bodyfat_prediction"), style="font-size:200%"),
            span(textOutput("prediction_interval"), style="font-size:150%"),
            plotOutput("comparison_hist")
        )
    )
)


# Define server logic required to draw a histogram
server <- function(input, output) {
    
    
    output$warning_input <- renderText({
        validate(need(valid_input(input$abdomen, input$weight), ""))
        
        # Provide warning for extreme values that are outside of dataset
        if(input$abdomen < 20 || input$abdomen > 65){
            paste("Input values outside of dataset range, potentially inaccurate results")
        } else if(input$weight < 100 || input$weight > 275){
            paste("Input values outside of dataset range, potentially inaccurate results")
        } else{
            paste("")
        }
    })
    
    output$bodyfat_prediction <- renderText({
        validate(
          need(valid_input(input$abdomen, input$weight), 
               "Abdomen and Weight need to be postive numeric values."))
        # Generate Bodyfat estimate based on values
        input_df <- data.frame("ABDOMEN"=input$abdomen,"WEIGHT" = input$weight)
        fat_prediction <- predict(model, input_df)
        
        paste("Predicted Body Fat: ", round(fat_prediction,3),"%", sep="")
    })
    
    output$prediction_interval <- renderText({
        validate(need(valid_input(input$abdomen, input$weight),""))
        # Generate Bodyfat estimate based on values
        input_df <- data.frame("ABDOMEN"=input$abdomen,"WEIGHT" = input$weight)
        fat_prediction <- predict(model, input_df, interval = "prediction")
        paste("95% predictive interval: [",
              round(fat_prediction[2],3),",",
              round(fat_prediction[3],3),
              "]",
              sep="")
    })
    
    output$comparison_hist <- renderPlot({
        validate(need(valid_input(input$abdomen, input$weight),""))
        bodyfat_ranges <- data.frame(Ranges= c("Obsese", "Average", "Fit","Athletic","Essential Fat"),
                                     Proportion=c(10,7,4,7,6),
                                     dummy_x = c(0,0,0,0,0))
        bodyfat_ranges$Ranges <- factor(bodyfat_ranges$Ranges, levels=c("Obsese", "Average", "Fit","Athletic","Essential Fat"))
        bodyfat_ranges$Labels <- c("24%+", "17-24%", "13-17%","6-13%","2-6%")
        input_df <- data.frame("ABDOMEN"=input$abdomen,"WEIGHT" = input$weight)
        fat_prediction <- predict(model, input_df)
        
        ggplot(bodyfat_ranges, aes(x=dummy_x, y = Proportion, fill=Ranges)) + 
            geom_col()+
            geom_text(aes(label =Labels),
                                position = position_stack(vjust = 0.5)) +
            scale_fill_brewer(palette = "Set2") +
            theme_minimal(base_size = 16) +
            ylab("Body Fat %") +
            xlab(NULL) +
            geom_hline(yintercept=fat_prediction, 
                       linetype="dashed", 
                       color = "red") +
            annotate(geom="text",
                     label=paste(round(fat_prediction,3),"%",sep=""),
                     x=-0.4,
                     y=fat_prediction,
                     vjust=-1) +
            theme(axis.text.x = element_blank())
    })
}

# Run the application 
shinyApp(ui = ui, server = server)