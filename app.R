library(shiny)
library(plyr)
library(tidyverse)
library(acs)
library(stringr)
library(dplyr)
library(leaflet)
library(ggplot2)
library(tidyr)
library(ggmap)
library(xlsx)
require(magrittr)
library(geosphere)
library(googleway)
library(gmapsdistance)
library(shinythemes)
library(shinycssloaders)


mykey <- "AIzaSyDTDMOryaVf_vXtgtxFbD5UAsj-DXy7JNs" # this key is used for the google maps API 

# Define UI for application. This contains the Method of Transportation Selector and the map.
ui <- fluidPage(theme = shinytheme("slate"),
                tags$script('$(document).on("keypress", function (e) {
                                                  if(e.keyCode == 13){
                                                     $("#enter").click();
                            }});
                            '),
   # Application title
   titlePanel("Genentech Ride Finder"),
   
   # Sidebar with Method of Transportation Selector
   sidebarLayout(
      sidebarPanel(
        textInput("origin", "Enter Your Address: " , value = ""),
        selectInput("travel_mode", "Select the Mode of Transportation to Your Pickup Location", choices = c("walking","bicycling", "transit")),
        actionButton("enter","Enter")
      ),
      
      # Show a map containing the optimal route
      mainPanel(
        leafletOutput("map")
      )
   ),
   h3(textOutput("Info1")),
   h4(textOutput("Info2")), 
   h4(textOutput("Info3"))
)

server <- function(input, output) {

  ## Enter in origin
  observeEvent(input$enter,
    handlerExpr = {
    output$map <- renderLeaflet({
      leaflet()
    })
    
    # read in the bus address's lat and longs
    final <- read.xlsx('BusAddressesFinal.xlsx',1)
    set.api.key("AIzaSyDTDMOryaVf_vXtgtxFbD5UAsj-DXy7JNs")
    
    # read in the user inputted address to ensure that it is a valid address
    result <- google_geocode(input$origin, key = mykey)
    
    if(result$status == "ZERO_RESULTS"){
      output$Info1 <- renderText({ 
        print(paste("Your address could not be found. Please try again."))
      })
      output$Info3 <- renderText({ 
        print(paste(""))
      })
      output$Info3 <- renderText({ 
        print(paste(""))
      })
      return ()
    }
    
    #grabs the lat and long of the user's address
    final$ideallon <- as.numeric(result$results$geometry$location$lng) 
    final$ideallat <- as.numeric(result$results$geometry$location$lat) 
    final$originaddress <- as.character(result$results$formatted_address) 
    
    ## Does a quick search for distance utilizing Haversine Distance Calculator
    apartments <- matrix(c(final$ideallon, final$ideallat),ncol = 2)
    stops <- matrix(c(final$lon,final$lat), ncol = 2)
    final$distances <- distHaversine (apartments, stops, r = 6378137)
    final <- final[order(final$distances),] 
    final <- final[1:10,]
    
    ## Uses Google's API to find travel time and distance
    final$latlon <- paste(final$lat,final$lon, sep = '+')
    final$latlonhome <- paste(final$ideallat,final$ideallon, sep = '+')
    
    if(input$travel_mode == "driving"){ ## here 
      results <-  do.call(rbind, lapply(1:nrow(final),
                    function(i)gmapsdistance(origin = final[i,14], destination = final[i,13],  mode = "driving")))
      results <-   as.data.frame(results)
    } else {
      results <-  do.call(rbind, lapply(1:nrow(final),
                    function(i)gmapsdistance(origin = final[i,14], destination = final[i,13],  mode = input$travel_mode, key = get.api.key())))
      results <-   as.data.frame(results)
    }
    
    # Grab the data from Google's API
    final$traveltime <- as.numeric(results$Time)
    final$traveldistance <- as.numeric(results$Distance)
    
    # sort the list of pick up location by travel time
    sortedfinal <- final[order(as.numeric(final$traveltime)),] 
    
    # converts travel time from seconds to minutes
    sortedfinal$traveltime <- round(sortedfinal$traveltime/60, digits = 0)
    
    # converts distance from meters to miles
    sortedfinal$traveldistance <- round(sortedfinal$traveldistance/1600, digits = 1)
    
    ## Gets the route
    key <-  mykey
    origin <- as.numeric(c(sortedfinal[1,10:9]))
    destination <- as.numeric(c(sortedfinal[1,6:5]))
    
    res <- google_directions(origin = origin, destination = destination, key = mykey, mode = input$travel_mode)
    df_polyline <- decode_pl(res$routes$overview_polyline$points)
    head(df_polyline)
 
    ## Plots the markers and the route
    output$map <- renderLeaflet({
      leaflet() %>%
        addProviderTiles("CartoDB.Positron") %>%
        addPolylines(data = df_polyline, lat = ~lat, lng = ~lon) %>%
        addAwesomeMarkers(destination[2], destination[1], icon = awesomeIcons(icon = ' fa-bus',iconColor = '#ffffff', library = 'fa', markerColor = 'darkblue'), popup = paste("<b>",sortedfinal[1,7],"</b>"), popupOptions(color = "black")) %>%
        addAwesomeMarkers(origin[2],origin[1], icon = awesomeIcons(icon = 'fa-home',iconColor = '#ffffff', library = 'fa', markerColor = 'darkred'), popup = paste("<b>",sortedfinal[1,11], "</b>"), popupOptions(color = 'black'))
      })
    
    #outputs information about the pickup
    output$Info1 <- renderText({
      HTML(paste(("Your gRide pickup is: "), sortedfinal[1,3], sep = ''))
    })
    output$Info2 <- renderText({
      print(paste("The"," closest pickup location ", "is: ", sortedfinal[1,4], sep = ''))
    })
    output$Info3 <- renderText({ 
      print(paste("Travel time to the stop is: ", sortedfinal[1,15], " minutes. Travel distance is: ", sortedfinal[1,16], " miles.", sep = ''))
    })
    })
  
}

# Run the application 
shinyApp(ui = ui, server = server)

