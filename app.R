library(tidyverse)
library(leaflet)
library(jsonlite)
library(osmdata)
library(htmlwidgets)
library(tidygeocoder)
library(leaflet.extras)
library(ggplot2)
library(glue)
library(shiny)

mapbox <- Sys.getenv("MAPBOX_API_KEY")

attribution <- "© <a href='https://www.mapbox.com/about/maps/'>Mapbox</a>"
# Define a function to retrieve and process data
get_data <- function(data_source) {
  # Retrieve data from the API
  data_url <- data_source
  data <- fromJSON(data_url)
  
  # Add Nashville to the address column for geocoding coordinates
  data <- data %>%
    mutate(address = paste0(address, ' ', "Nashville, TN"))
  # Get lat_long coordinates
  lat_long <- geo(address = data$address, method = "arcgis")
  # Prcoess the data
  data <- data %>% 
    select(-address) %>% 
    cbind(lat_long) %>% 
    mutate(address = gsub("Nashville, TN", "", address),
           coord = paste(lat, long, sep = ","),  # Creates new column with the latitude and longitude coordinates
           gsv = glue("http://maps.google.com/maps?q=&layer=c&cbll={coord}"),  # Creates new column with the Google Street View URL
           gsv_links = paste0('<a target=_blank href=', gsv, '>Google Street View</a>'))  # Creates new column with the Google Street View clickable link
  
  df <- data %>% 
    select(incident_type, call_received, address, city, lat, long, gsv_links) %>% 
    mutate(call_received = format(as.POSIXct(call_received, format = "%Y-%m-%dT%H:%M:%S"), "%I:%M:%S %p"), # Converts military time
           city = as.character(city),
           color = ifelse(incident_type %in% c("SHOTS FIRED", 
                                               "SHOOTING", 
                                               "HOLD UP ROBBERY IN PROGRESS", 
                                               "ROBERRY/HOLD UP ALARM", 
                                               "HOLD UP ROBBERY IN PROGRESS JUVENILE"), "red", "orange")) # Color code incidents based on threat level
  
  return(df)
}

# Shiny app code
ui <- fluidPage(
  # Positioning code for map and overlaid barplot
  tags$style(HTML("
    #map { 
      position: absolute; 
      top: 0; 
      bottom: 0; 
      left: 0; 
      right: 0; 
    }
    #chart {
    position: absolute;
    bottom: 10px;
    left: 10px;
    z-index: 999;
    background-color: rgb(34, 34, 34); 
    opacity: 0.8;
    color: rgb(105, 255, 255); 
    }
    #chart .plot-container { 
    background-color: rgb(105, 255, 255);
    }
    ")),
  leafletOutput("map", width = "100%", height = "100%"),
  plotOutput("chart", height = 250, width = 500)
)

server <- function(input, output, session) {
  
  # Update data source every 5 minutes
  data_source <- reactivePoll(1000 * 60 * 5, session, 
                              valueFunc = function() {
                                get_data("https://data.nashville.gov/resource/qywv-8sc2.json")
                              }, 
                              checkFunc = function() {
                                Sys.time()
                              })
  
  # Leaflet map output
  output$map <- renderLeaflet({
    map <- leaflet() %>% 
      addTiles(mapbox, attribution) %>% 
      clearMarkers() %>% 
      addCircleMarkers(data = data_source(), 
                       lat = ~lat, 
                       lng = ~long, 
                       color = ~color,
                       popup = paste0("Incident Type: ", data_source()$incident_type, 
                                      "<br>", "Call Received: ", data_source()$call_received, 
                                      "<br>", "Address: ", data_source()$address, 
                                      "<br>", "City: ", data_source()$city,
                                      "<br>", data_source()$gsv_links), 
                       clusterOptions = markerClusterOptions(spiderfyDistanceMultiplier=1.5, maxClusterRadius = 1)) %>%
      setView(lat = 36.1627, lng = -86.7816, zoom = 11) %>% 
      addResetMapButton()
    map
  })
  
  # Bar chart output
  output$chart <- renderPlot({
    crime_count <- data_source() %>%
      group_by(incident_type) %>%
      summarize(count = n()) %>%
      arrange(desc(count))
    
    ggplot(crime_count, aes(x = reorder(incident_type, count), y = count)) +
      geom_bar(stat = "identity") +
      coord_flip() +
      xlab("") +
      ylab("") +
      ggtitle(paste(nrow(data_source()),"ACTIVE INCIDENTS")) +
      geom_text(aes(y = count, label = count), hjust = -0.2, size = 4) +
      theme(legend.position = "none",
            panel.background = element_rect(fill = "#222222"), 
            panel.grid.major = element_blank(), 
            panel.grid.minor = element_blank(), 
            axis.text.x = element_blank(),
            axis.ticks.x = element_blank(),
            axis.text.y = element_text(size = 12),
            plot.title = element_text(size = 13)) 
    
  })
}

shinyApp(ui = ui, server = server)