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
library(shinythemes)
library(ggplot2)
library(patchwork)


# Define UI for application that draws a histogram
ui <- fluidPage(theme=shinytheme("superhero"),

    # Application title
    titlePanel("End to End in 2021 progress tracker"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            textInput("athlid", label = "Strava Athlete ID", value = 32838265),
            textInput("targetmiles", label = "Yearly target miles (shortest end to end is 813 miles)", value = 813)
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
           htmlOutput("textOutput"),
           plotOutput("distPlot")
        )
    )
)



# Define server logic required to draw a histogram
server <- function(input, output) {

    mydat2 <- reactive({
        athleteID <- input$athlid
        mydat3 <- athl_fun(athleteID, trace = FALSE)[[1]]
        mydat3$targetmiles <- as.integer(input$targetmiles)
        mydat3
    })
    output$distPlot <- renderPlot({
        mydat <- mydat2()
        startDate <- as.Date.character("2021-01-01")
        
        daysSoFar <- as.integer(1+Sys.Date()-startDate)
        x    <- mydat$monthly$miles
        milesIn2021 <- sum(x[mydat$monthly$month>=startDate])
        if(mydat$units[1]=="km") x <- x*0.621371
        df1 <- data.frame(x=c("Total so far", "Left to go"), y=c(milesIn2021, mydat$targetmiles-milesIn2021))
        df2 <- data.frame(x=c("Current average\n(miles per day)","Required average\n(miles per day)"),
                          y=c((milesIn2021)/(daysSoFar),(mydat$targetmiles - milesIn2021)/(365 - daysSoFar)))
        g1 <- ggplot(df1, aes(x,y)) + geom_col(fill="darkblue") + coord_flip() + theme_bw() + labs(x=NULL, y="Miles")
        g2 <- ggplot(df2, aes(x,y)) + geom_col(fill="darkblue") + coord_flip() + theme_bw() + labs(x=NULL, y="Miles")
        g1 + g2
        #plot(df1)
    })
    
    output$textOutput <- renderUI({
        
        #stoken <- httr::config(token = readRDS('.httr-oauth')[[1]])
        #acts <- compile_activities(get_activity_list(stoken = stoken, after = as.Date.character("2020-12-30")))
        #acts <- acts[,c("name", "distance", "elapsed_time", "workout_type", "start_date")]
        
        mydat <- mydat2()
        
        # generate bins based on input$bins from ui.R
        if(is.na(mydat)) return(HTML("Something went wrong scraping your data from Strava, sorry!"))
        x    <- mydat$monthly$miles
        if(mydat$units[1]=="km") x <- x*0.621371
        ## It's 874 miles end to end.
        startDate <- as.Date.character("2021-01-01")
        
        milesIn2021 <- sum(x[mydat$monthly$month>=startDate])
        daysSoFar <- as.integer(1+Sys.Date()-startDate)
        # draw the histogram with the specified number of bins
        HTML(sprintf("<h3>Hi, %s</h3>You have run %0.1f miles in 2021.<br/> 
                %d days elapsed so far (including today).<br/>
                You have run %0.1f miles per day so far.<br/>
                You have %0.1f miles left to run in %d days.<br/>
                From today you need to run %0.1f miles per day to meet your target.<br/>&nbsp;",
                mydat$name, milesIn2021, 
                #dim(acts)[1],
                daysSoFar, (milesIn2021/daysSoFar), 
                mydat$targetmiles - milesIn2021,
                365 - daysSoFar,
                (mydat$targetmiles - milesIn2021)/(365 - daysSoFar)
                ))
    })
}

# Run the application 
shinyApp(ui = ui, server = server)

#app_name <- 'gWorkoutAnalysis' # chosen by user
#app_client_id  <- '44619' # an integer, assigned by Strava
#app_secret <- '9fec30a606c02bed58395f90d03efb265a2624d8' # an alphanumeric secret, assigned by Strava

# create the authentication token
#stoken <- httr::config(token = strava_oauth(app_name, app_client_id, app_secret, app_scope="activity:read_all", cache=TRUE))

# 
# 
# myinfo <- get_athlete(stoken, id = '32838265')
# myinfo
# my_acts <- get_activity_list(stoken)
# 
# act_data <- subset(compile_activities(my_acts))
