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
library(ggmap)
library(raster)
library(XML)
library(OpenStreetMap)
library(sp)
library(mapview)
library(leaflet)
library(leafpop)
library(stringr)

shift.vec <- function (vec, shift) {
    if(length(vec) <= abs(shift)) {
        rep(NA ,length(vec))
    }else{
        if (shift >= 0) {
            c(rep(NA, shift), vec[1:(length(vec)-shift)]) }
        else {
            c(vec[(abs(shift)+1):length(vec)], rep(NA, abs(shift))) } } }


getLEJOG <- function(){
# Parse the GPX file

}

getLEJOG()

# Define UI for application that draws a histogram
ui <- function(request) {fluidPage(theme=shinytheme("superhero"),

    # Application title
    titlePanel("End to End progress tracker"),

    # Sidebar with a slider input for number of bins 
    sidebarLayout(
        sidebarPanel(
            HTML("<p>Enter your own Strava ID, target date and distance then if you want to save hit 'bookmark' to generate a link to this page that keeps your info.</p>"),
            textInput("athlid", label = "Strava Athlete ID", value = 32838265),
            textInput("targetmiles", label = "Yearly target miles (shortest end to end is 813 miles)", value = 813),
            dateInput("targetDate", label="Target date", value="2021-12-31", format="dd M yyyy"),
            bookmarkButton(title = "Generates a bookmark to this page with the current data stored."),
            plotOutput("distPlot")
            
            
        ),

        # Show a plot of the generated distribution
        mainPanel(
           htmlOutput("textOutput"),
           HTML("<h4>End to End progress:</h4>"),
           leafletOutput("mymap")
           
        )
    )
)}



# Define server logic required to draw a histogram
server <- function(input, output) {

    
    
    mydat2 <- reactive({
        athleteID <- input$athlid
        mydat3 <- athl_fun(athleteID, trace = FALSE)[[1]]
        mydat3$targetmiles <- as.integer(input$targetmiles)
        mydat3
    })
    
    pointdist <- function(lat1, lat2, lon1, lon2){
    R = 6371e3
    F1 = lat1 * pi/180
    F2 = lat2 * pi/180;
    DF = (lat2-lat1) * pi/180
    DL = (lon2-lon1) * pi/180
    
    a = sin(DF/2) * sin(DF/2) +
        cos(F1) * cos(F2) *
        sin(DL/2) * sin(DL/2)
    c = 2 * atan2(sqrt(a), sqrt(1-a))
    
    d = R * c
    d
    }
    
    
    
    output$mymap <- renderLeaflet({
        # pfile <- htmlTreeParse(file = "lejog.gpx", error = function(...) {
        # }, useInternalNodes = T)
        # # Get all elevations, times and coordinates via the respective xpath
        # coords <- xpathSApply(pfile, path = "//trkpt", xmlAttrs)
        # lats <- as.numeric(coords["lat",])
        # lons <- as.numeric(coords["lon",])
        # geodf<-data.frame(lat = lats, lon = lons)
        # geodf2 <- geodf[((1:length(geodf[,1])%%20)==1) | ((1:length(geodf[,1]))==length(geodf[,1])),]
        # geodf2 <- geodf2[length(geodf2[,1]):1,]
        # geodf2$lat.p1 <- shift.vec(geodf2$lat, -1)
        # geodf2$lon.p1 <- shift.vec(geodf2$lon, -1)
        # geodf2$dist.to.prev <- c(0,
        #                          pointdist(geodf2$lat, geodf2$lat.p1, geodf2$lon, geodf2$lon.p1)[1:(length(geodf2[,1])-1)]
        #                          )
        # geodf2$cumsum <- cumsum(geodf2$dist.to.prev)
        
        #saveRDS(geodf2, "geodf2.rds")
        geodf2 <- readRDS("geodf2.rds")
        
         
         spdf_geo <- geodf2
         coordinates(spdf_geo) <- ~ lon + lat
         sp::proj4string(spdf_geo) <- "+init=epsg:4326"
        
        
        
        mydat <- mydat2()
        x    <- mydat$monthly$miles
        startDate <- as.Date.character("2021-01-01")
        if(mydat$units[1]=="km") x <- x*0.621371
        milesIn2021 <- sum(x[mydat$monthly$month>=startDate])
        
        spdf_geo2 <- remove_missing(geodf2[cumsum(geodf2$dist.to.prev)<milesIn2021*1609,])
        coordinates(spdf_geo2) <- ~ lon + lat
        sp::proj4string(spdf_geo2) <- "+init=epsg:4326"
        
        
        m <- mapview(list(spdf_geo,spdf_geo2),col.regions=list("red","blue"),col=list("red","blue"),layer.name=c("Remaining", "Completed") )
        
        m@map
    })
    
    output$distPlot <- renderPlot({
        mydat <- mydat2()
        startDate <- as.Date.character("2021-01-01")
        targetDate <- input$targetDate
        daysSoFar <- as.integer(1+Sys.Date()-startDate)
        daysToGo <- as.integer(targetDate-Sys.Date())
        x    <- mydat$monthly$miles
        if(mydat$units[1]=="km") x <- x*0.621371
        milesIn2021 <- sum(x[mydat$monthly$month>=startDate])
        df1 <- data.frame(x=c("Total so far", "Left to go"), y=c(milesIn2021, mydat$targetmiles-milesIn2021))
        df2 <- data.frame(x=c("Current average","Required average"),
                          y=c(milesIn2021/daysSoFar,(mydat$targetmiles - milesIn2021)/(daysToGo)))
        print(df2)
        g1 <- ggplot(df1, aes(x,y,fill=x)) + geom_col() +scale_fill_manual(values=c("red", "blue")) + theme_bw() + labs(x=NULL, y="Miles") + theme(axis.text.x = element_text(angle=45, size=14, hjust=1), legend.position = "none")
        g2 <- ggplot(df2, aes(x,y,fill=x)) + geom_col() +scale_fill_manual(values=c("blue", "red"))+ theme_bw() + labs(x=NULL, y="Miles per day") + theme(axis.text.x = element_text(angle=45, size = 14, hjust=1), legend.position = "none")
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
        targetDate <- input$targetDate
        milesIn2021 <- sum(x[mydat$monthly$month>=startDate])
        daysSoFar <- as.integer(1+Sys.Date()-startDate)
        daysToGo <- as.integer(targetDate-Sys.Date())
        totalDays <- as.integer(targetDate - startDate)
        milesahead <- milesIn2021 - (mydat$targetmiles / totalDays)*daysSoFar
        # draw the histogram with the specified number of bins
        HTML(sprintf("<h3>Hi, %s</h3>You have run %0.1f miles in 2021.<br/> 
                %d days elapsed so far (including today).<br/>
                You have run %0.1f miles per day so far.<br/>
                You are %0.1f miles %s schedule.<br/>
                You have %0.1f miles left to run in %d days.<br/>
                From today you need to run %0.1f miles per day to meet your target.<br/>&nbsp;",
                str_replace_all(mydat$name, "[^[A-Za-z,]]", " "), milesIn2021, 
                #dim(acts)[1],
                daysSoFar, (milesIn2021/daysSoFar), 
                abs(milesahead), ifelse(milesahead>0, "ahead of", "behind"),
                mydat$targetmiles - milesIn2021,
                daysToGo,
                (mydat$targetmiles - milesIn2021)/(daysToGo)
                ))
    })
}

# Run the application 
shinyApp(ui = ui, server = server, enableBookmarking = "url")

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
