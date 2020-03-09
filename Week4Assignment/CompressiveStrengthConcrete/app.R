#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)

# Define UI for application 
ui <- fluidPage(

    # Application title
    titlePanel("Compressive Strength of Concrete from Yeh (1998)"),
    
    h5("Yeh (1998) describes a collection of data sets from different sources that can be used for modeling the compressive strength of concrete formulations as a functions of their ingredients and age."),
    h5("The data are from Yeh (1998) and taken from the UCI ML website http://archive.ics.uci.edu/ml/datasets/Concrete+Compressive+Strength."),

    h5("In this application, you can modify these variables:"),
    h5("- Percentage of data in the train set: percentage of data we put in the train set. The rest of the data are put in the test set"),
    h5("- Use colors in the plot: you can choose if you want or not colors in the plot"),
    h5("- Type of x variable against which plot:"),
    h5("    * fraction: plots agains the fraction of the L1 norm of the coefficient vector."),
    h5("    * penalty: plots against the 1-norm penalty parameter."),
    h5("    * L1norm: plots against the L1 norm of the coefficient vector."),
    h5("    * step: plots against the LARS-EN step number."),
    
    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            sliderInput("pValue",
                        "Percentage of data in the train set",
                        min = 1,
                        max = 100,
                        value = 75),
            checkboxInput("colorUse","Use colors in the plot",value=TRUE),
            selectInput("xVariable", "Type of x variable against which plot:",
                        c("fraction" = "fraction",
                          "penalty" = "penalty",
                          "L1norm" = "L1norm",
                          "step" = "step"))
        ),

        # Show a plot of the generated distribution
        mainPanel(
           plotOutput("concretePlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
    
    output$concretePlot <- renderPlot({
     
        # Import needed libraries
        library(caret)
        library(AppliedPredictiveModeling)
        library(elasticnet)
        
        #Set the seed for the reproductible
        set.seed(3523)
        
        # Use sample data concrete
        data(concrete)
        
        # Predictive model
        inTrain = createDataPartition(concrete$CompressiveStrength, p = input$pValue/100)[[1]]
        training = concrete[ inTrain,]
        testing = concrete[-inTrain,]
        fitLs <- train(CompressiveStrength~., data=training, method = "lasso")
        
        # Plot
        plot.enet(fitLs$finalModel, xvar = input$xVariable, use.color = input$colorUse)
        
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
