#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(rStrava)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("End to End in 2021 progress tracker"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            textInput("athlid", label = "Strava Athlete ID", value = 32838265)
            textInput("targetmiles", label = "Yearly target miles", value = 874)
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
           textOutput("textOutput"),
           plotOutput("distPlot")
        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$textOutput <- renderText({
        athleteID <- input$athlid
        targetmiles <- input$targetmiles
        # generate bins based on input$bins from ui.R
        mydat <- athl_fun(athleteID, trace = FALSE)[[1]]
        x    <- mydat$monthly$miles
        
        ## It's 874 miles end to end.
        startDate <- as.Date.character("2021-01-01")
        
        milesIn2021 <- sum(mydat$monthly$miles[mydat$monthly$month>=startDate])
        daysSoFar <- as.integer(startDate - Sys.Date() + 1)
        # draw the histogram with the specified number of bins
        sprintf("You have run %0.1f miles in 2021.\n 
                %d days elapsed so far (including today).\n
                To meet the target you need to run an average of %0.1f miles per day.\n
                You have run %0.1f miles per day so far.\n
                You have %0.2f miles left to run in %d days.\n
                You need to run $0.2f miles per day to meet your target.",
                milesIn2021)
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
