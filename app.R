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
library(bslib)
library(thematic)
library(htmltools)
thematic::thematic_shiny(font = "auto")

mapbox <- Sys.getenv("MAPBOX_API_KEY")
attribution <- "© <a href='https://www.mapbox.com/about/maps/'>Mapbox</a> © <a href='http://www.openstreetmap.org/copyright'>OpenStreetMap</a> <strong><a"
# Function to retrieve and process data
get_data <- function(data_source) {
  # Retrieve data from the API
  data_url <- data_source
  data <- fromJSON(data_url)
  
  # Add Nashville to the address column for geocoding coordinates
  data <- data %>%
    mutate(address = gsub("/", "&", address)) %>%
    mutate(address = ifelse(grepl("OLD HICKORY BLVD", address) & city == "HERMITAGE",
                            paste0(address, ' ', "Hermitage, TN"),
                            paste0(address, ' ', "Nashville, TN")))
  
  # Get lat_long coordinates
  lat_long <- geo(address = data$address, method = "arcgis")
  # Process the data
  data <- data %>% 
    select(-address) %>% 
    cbind(lat_long) %>% 
    mutate(address = gsub("Nashville, TN", "", address),
           coord = paste(lat, long, sep = ","),  # Creates new column with the latitude and longitude coordinates
           gsv = glue("http://maps.google.com/maps?q=&layer=c&cbll={coord}"),  # Creates new column with the Google Street View URL
           gsv_links = paste0('<a target=_blank href=', gsv, '>Street View</a>'),  # Creates new column with the Google Street View clickable link
           incident_type = gsub("ROBERRY/HOLD UP ALARM", "ROBBERY/HOLD UP ALARM", incident_type)) # Robbery is misspelled in the json data
  
  df <- data %>% 
    select(incident_type, call_received, address, city, lat, long, gsv_links) %>% 
    mutate(call_received = format(as.POSIXct(call_received, format = "%Y-%m-%dT%H:%M:%S"), "%I:%M:%S %p"), # Converts military time
           city = as.character(city)) 
  
  return(df)
}
theme <- bslib::bs_theme(version = 5, bootswatch = "superhero", bg = '#222222', fg = '#11F5DF' )
# Shiny app code
ui <- fluidPage(
  theme = theme,
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
  #bs_themer()
  # Define variable to hold leaflet map
  m <- leaflet() %>% 
    addTiles(mapbox, attribution) %>% 
    setView(lat = 36.1627, lng = -86.7816, zoom = 11) %>% 
    addResetMapButton()
  
  # Update data source every 15 minutes
  data_source <- reactivePoll(1000 * 60 * 15, session, 
                              valueFunc = function() {
                                get_data("https://data.nashville.gov/resource/qywv-8sc2.json")
                              }, 
                              checkFunc = function() {
                                Sys.time()
                              })
  
  # Leaflet map output
  output$map <- renderLeaflet({
    m %>% 
      clearMarkers() %>% 
      addPulseMarkers(
        data = data_source(), 
        lat = ~lat, 
        lng = ~long,
        label = ~paste0("Incident Type: ", incident_type, 
                        "<br>", "Call Received: ", call_received, 
                        "<br>", "Address: ", address, 
                        "<br>", "City/Area: ", city) %>% lapply(htmltools::HTML),
        popup = ~paste0("Incident Type: ", incident_type, 
                        "<br>", "Call Received: ", call_received, 
                        "<br>", "Address: ", address, 
                        "<br>", "City/Area: ", city,
                        "<br>", gsv_links), 
        clusterOptions = markerClusterOptions(spiderfyDistanceMultiplier=1.5, maxClusterRadius = 1),
        icon = makePulseIcon(
          heartbeat = ifelse(data_source()$incident_type %in% c("SHOTS FIRED", 
                                                                "SHOOTING", 
                                                                "HOLD UP ROBBERY IN PROGRESS", 
                                                                "ROBBERY/HOLD UP ALARM", 
                                                                "HOLD UP ROBBERY IN PROGRESS JUVENILE",
                                                                "SHOOTING IN PROGRESS JUVENILE"), 1, 2), 
          iconSize = c(10, 10), 
          color = ifelse(data_source()$incident_type %in% c("SHOTS FIRED", 
                                                            "SHOOTING", 
                                                            "HOLD UP ROBBERY IN PROGRESS", 
                                                            "ROBBERY/HOLD UP ALARM", 
                                                            "HOLD UP ROBBERY IN PROGRESS JUVENILE",
                                                            "SHOOTING IN PROGRESS JUVENILE"), "red", "orange")
        )
      )
  })
  
  
  # Update markers every 15 minutes
  observeEvent(data_source(), {
    leafletProxy("map") %>%
      clearMarkers() %>%
      addPulseMarkers(
        data = data_source(),
        lat = ~lat,
        lng = ~long,
        label = ~paste0("Incident Type: ", incident_type, 
                        "<br>", "Call Received: ", call_received, 
                        "<br>", "Address: ", address, 
                        "<br>", "City/Area: ", city) %>% lapply(htmltools::HTML),
        popup = ~paste0("Incident Type: ", incident_type, 
                        "<br>", "Call Received: ", call_received, 
                        "<br>", "Address: ", address, 
                        "<br>", "City/Area: ", city,
                        "<br>", gsv_links),
        clusterOptions = markerClusterOptions(spiderfyDistanceMultiplier = 1.5, maxClusterRadius = 1),
        icon = makePulseIcon(
          heartbeat = ifelse(data_source()$incident_type %in% c("SHOTS FIRED", 
                                                                "SHOOTING", 
                                                                "HOLD UP ROBBERY IN PROGRESS", 
                                                                "ROBBERY/HOLD UP ALARM", 
                                                                "HOLD UP ROBBERY IN PROGRESS JUVENILE",
                                                                "SHOOTING IN PROGRESS JUVENILE"), 1, 2), 
          iconSize = c(10, 10), 
          color = ifelse(data_source()$incident_type %in% c("SHOTS FIRED", 
                                                            "SHOOTING", 
                                                            "HOLD UP ROBBERY IN PROGRESS", 
                                                            "ROBBERY/HOLD UP ALARM", 
                                                            "HOLD UP ROBBERY IN PROGRESS JUVENILE",
                                                            "SHOOTING IN PROGRESS JUVENILE"), "red", "orange")
        )
      )
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
      geom_text(aes(y = count, label = count), hjust = -0.2, size = 5) +
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
