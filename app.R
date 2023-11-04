library(tidyverse)
library(leaflet)
library(jsonlite)
library(osmdata)
library(htmlwidgets)
library(tidygeocoder)
library(leaflet.extras)
library(glue)
library(shiny)
library(htmltools)
library(highcharter)

# Load Mapbox API key and map attribution
mapbox <- Sys.getenv("MAPBOX_API_KEY")
attribution <- "© <a href='https://www.mapbox.com/about/maps/'>Mapbox</a> © <a href='http://www.openstreetmap.org/copyright'>OpenStreetMap</a> <strong><a"

# Function to retrieve and process data
get_data <- function(data_source) {
  
  # Retrieve data from the API
  data_url <- data_source
  data <- fromJSON(data_url)
  
  # Replace '/' with '&' and add Nashville, Brentwood or Hermitage to the address column for better geocoding accuracy
  data <- data %>%
    mutate(
      address = gsub("/", "&", address),
      address = ifelse(
        grepl("OLD HICKORY BLVD", address) & city == "BRENTWOOD DAVIDSON COUNTY",
        paste0(address, ' ', "Brentwood, TN"),
        ifelse(
          grepl("OLD HICKORY BLVD", address) & city == "HERMITAGE",
          paste0(address, ' ', "Hermitage, TN"),
          paste0(address, ' ', "Nashville, TN")
        )
      )
    )
  
  # Get lat/long coordinates
  lat_long <- geo(address = data$address, method = "arcgis", full_results = TRUE)
  
  # Mutate the data
  data <- data %>% 
    select(-address) %>% 
    cbind(lat_long) %>% 
    mutate(
      address = arcgis_address,
      full_address = gsub(" ", "%20", arcgis_address),  # Format address for Google Maps link
      coord = paste(lat, long, sep = ","),  # Create new column with coordinates
      
      # Use conditional to distinguish between cross street coordinates and actual addresses
      gsv = ifelse(
        grepl("&", arcgis_address),
        glue("http://maps.google.com/maps?q=&layer=c&cbll={coord}"),
        glue("http://maps.google.com/maps?q={full_address}")
      ),
      
      gsv_links = paste0('<a target="_blank" href=', gsv, '>Street View</a>'), # Google Street View links
      incident_type = gsub("ROBERRY/HOLD UP ALARM", "ROBBERY/HOLD UP ALARM", incident_type)  # Correct spelling error 
    )
  
  df <- data %>% 
    select(incident_type, call_received, address, city, lat, long, gsv_links) %>% 
    mutate(
      call_received = format(as.POSIXct(call_received, format = "%Y-%m-%dT%H:%M:%S"), "%I:%M:%S %p"),  # Convert military time
      city = as.character(city)
    )
  
  return(df)
}

# UI setup
ui <- fluidPage(
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
      opacity: 0.8;
    }
    #github-link {
      position: absolute;
      top: 10px;
      right: 10px;
      z-index: 1;  
      font-size: 36px;  
      color: #11F5DF;
    }
  ")),
  leafletOutput("map", width = "100%", height = "100%"),
  highchartOutput("chart", height = 300, width = 500), 
  
  # GitHub link with icon
  tags$a(href = "https://github.com/RodNSS/Nashville_Active_Incident_Map", 
         target = "_blank", 
         icon("github", lib = "font-awesome"),
         id = "github-link")
)

server <- function(input, output, session) {
  
  # Define variable to hold Leaflet basemap
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
        clusterOptions = markerClusterOptions(
          spiderfyDistanceMultiplier = 1.5,
          maxClusterRadius = 1,
          iconCreateFunction = JS(
            "function(cluster) {
              return L.divIcon({ 
                html: '<div style=\"font-size: 16px; color: #000;\">' + cluster.getChildCount() + '</div>',
                className: 'custom-cluster-icon',
                iconSize: L.point(20, 20)
              });
            }"
          )
        ),
        icon = makePulseIcon(
          heartbeat = ifelse(data_source()$incident_type %in% c("SHOTS FIRED", 
                                                                "SHOOTING", 
                                                                "HOLD UP ROBBERY IN PROGRESS", 
                                                                "ROBBERY/HOLD UP ALARM",
                                                                "CUTTING OR STABBING IN PROGRESS",
                                                                "CUTTING OR STABBING IN PROGRESS JUVENILE",
                                                                "HOLD UP ROBBERY IN PROGRESS JUVENILE",
                                                                "SHOOTING IN PROGRESS JUVENILE"), 1, 2), 
          iconSize = c(10, 10), 
          color = ifelse(data_source()$incident_type %in% c("SHOTS FIRED", 
                                                            "SHOOTING", 
                                                            "HOLD UP ROBBERY IN PROGRESS", 
                                                            "ROBBERY/HOLD UP ALARM",
                                                            "CUTTING OR STABBING IN PROGRESS",
                                                            "CUTTING OR STABBING IN PROGRESS JUVENILE",
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
        clusterOptions = markerClusterOptions(
          spiderfyDistanceMultiplier = 1.5,
          maxClusterRadius = 1,
          iconCreateFunction = JS(
            "function(cluster) {
              return L.divIcon({ 
                html: '<div style=\"font-size: 16px; color: #000;\">' + cluster.getChildCount() + '</div>',
                className: 'custom-cluster-icon',
                iconSize: L.point(20, 20)
              });
            }"
          )
        ),
        icon = makePulseIcon(
          heartbeat = ifelse(data_source()$incident_type %in% c("SHOTS FIRED", 
                                                                "SHOOTING", 
                                                                "HOLD UP ROBBERY IN PROGRESS", 
                                                                "ROBBERY/HOLD UP ALARM",
                                                                "CUTTING OR STABBING IN PROGRESS",
                                                                "CUTTING OR STABBING IN PROGRESS JUVENILE",
                                                                "HOLD UP ROBBERY IN PROGRESS JUVENILE",
                                                                "SHOOTING IN PROGRESS JUVENILE"), 1, 2), 
          iconSize = c(10, 10), 
          color = ifelse(data_source()$incident_type %in% c("SHOTS FIRED", 
                                                            "SHOOTING", 
                                                            "HOLD UP ROBBERY IN PROGRESS", 
                                                            "ROBBERY/HOLD UP ALARM",
                                                            "CUTTING OR STABBING IN PROGRESS",
                                                            "CUTTING OR STABBING IN PROGRESS JUVENILE",
                                                            "HOLD UP ROBBERY IN PROGRESS JUVENILE",
                                                            "SHOOTING IN PROGRESS JUVENILE"), "red", "orange")
        )
      )
  })
  # Highcharter code
  output$chart <- renderHighchart({
    crime_count <- data_source() %>%
      group_by(incident_type) %>%
      summarize(count = n()) %>%
      arrange(desc(count))
    
    # Check if there's only one incident_type so chart displays properly
    if (nrow(crime_count) == 1) {
      crime_count <- tibble(incident_type = crime_count$incident_type, count = crime_count$count)
    }
    
    # Create the chart and interactive features
    hc <- highchart() %>%
      hc_chart(type = "bar") %>%
      hc_tooltip(enabled = FALSE
      ) %>%
      hc_title(
        text = paste(nrow(data_source()), "ACTIVE INCIDENTS"),
        style = list(fontSize = "15px", fontWeight = "bold", color = '#11F5DF')
      ) %>%
      hc_xAxis(
        categories = crime_count$incident_type,
        labels = list(style = list(color = '#11F5DF', fontSize = "13px")),
        lineWidth = 0,  
        lineColor = "transparent",
        tickmarkPlacement = "on",
        tickColor = '#11F5DF',
        tickWidth = 1
      ) %>%
      hc_yAxis(
        labels = list(enabled = FALSE),
        gridLineWidth = 0,
        gridLineColor = "transparent"
      ) %>% 
      hc_add_series(
        data = crime_count$count,
        name = "Incident Count",
        showInLegend = FALSE,
        cursor = "pointer",
        color = '#11F5DF',
        point = list(
          events = list(
            mouseOver = JS("
            function () {
              Shiny.setInputValue('selectedIncident', this.category);
              this.graphic.attr({
                fill: '#FF00FF'  // Changes the color when hovering over the bar
              });
            }
          "),
            mouseOut = JS("
            function () {
              Shiny.setInputValue('selectedIncident', null);
              var originalColor = this.color;  // Store the original color
              this.graphic.attr({
                fill: this.color  // Revert to original color on mouseOut
              });
            }
          ")
          )
        ) 
      ) %>%
      hc_plotOptions(
        bar = list(groupPadding = 0, pointWidth = 25, borderRadius = 9,
                   dataLabels = list(
                     enabled = TRUE,
                     color = '#11F5DF',
                     style = list(fontSize = '14px', 
                                  fontWeight = 'normal',
                                  textOutline = 'transparent'
                     )
                   ),
                   borderWidth = 0
        )
      )
    
    hc
  })
  # Observe the selected incident and add/clear markers
  observe({
    if (!is.null(input$selectedIncident)) {
      # Handle the selected incident
      selected_incident <- input$selectedIncident
      markers_to_show <- data_source() %>%
        filter(incident_type == selected_incident)
      
      # Add markers based on bar hover
      leafletProxy("map") %>%
        clearMarkers() %>%
        addCircleMarkers(
          data = markers_to_show,
          lat = ~lat,
          lng = ~long,
          color = '#FF00FF', 
          radius = 15,  
          fillOpacity = 0.1,  
          opacity = 1 
        )
    } else {
      leafletProxy("map") %>%
        clearMarkers()
    }
  })
  
}

shinyApp(ui = ui, server = server)
