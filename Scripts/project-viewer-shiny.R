library(rstudioapi)
library(arcgis)
library(sf)
library(shiny)
library(leaflet)
library(bslib)
library(stringr)

### Initialization ###

# Feature Layer url
f_url <- "https://services1.arcgis.com/acqg5b8xOcwTUl4z/arcgis/rest/services/Rice_Rivers_Center_-_Project_Registration_Public_View/FeatureServer"

# Open feature layer (this contains the metadata for the layer)
f_layer <- arc_open(f_url)

# Read the data
projects <- get_layer(f_layer, id=0)
projects_sf <- arc_select(projects)

# RRC Boundary File
aoi_url <- "Data/RRC-Boundary/RRC_ROI.shp"
aoi_sf <- st_read(aoi_url) |>
  st_transform(crs = 4326)

# RRC Bounding Box
aoi_bbox <- st_bbox(aoi_sf) |>
  as.vector()

### Shiny UI ###

ui <- page_fluid(
  
  leafletOutput("map")
  
)

server <- function(input, output) {
  
  # Create the map
  output$map <- renderLeaflet({
    leaflet() |>
      addProviderTiles(providers$Esri.WorldImagery) |>
      fitBounds(aoi_bbox[1], aoi_bbox[2], aoi_bbox[3], aoi_bbox[4]) |>
      addPolygons(data = aoi_sf, color = "#006894", opacity = 0.8, fillOpacity = 0) 
      
  })
  
  # This observer is responsible for maintaining the markers
  observe({
    leafletProxy("map", data = projects_sf) |>
      addCircleMarkers(data = projects_sf, layerId = ~globalid, color = "#FFB300", stroke = TRUE, opacity=0.9, fillOpacity = 0.3) 
  })
  
  # Functions for formatting popup
  formatProjectLead <- function(project) {
    
    lead <- project$projectLead
    
    if (lead == "other") {
      lead <- project$projectLead_other # Free text parameter
    } else {
      lead <- gsub('([[:upper:]])', ' \\1', lead) # Str split at uppercase
    }
    
    return(str_to_title(lead)) # Return name, formatted as a title
    
  }
  
  # Show a popup at a given location
  showPopup <- function(project, lat, lng) {
    selectedProject <- projects_sf[projects_sf$globalid == project,]
    content <- as.character(tagList(
      tags$strong("Project Title: "),
      p(str_to_title(as.character(selectedProject$projectTitle))), tags$br(),
      tags$strong("Principle Investigator: "),
      p(as.character(formatProjectLead(selectedProject))), tags$br()
    ))
    leafletProxy("map") |>
      addPopups(lng, lat, content, layerId = project)
  }
  
  observe({
    leafletProxy("map") |>
      clearPopups()
    event <- input$map_marker_click
    if (is.null(event))
      return()
    
    isolate({
      showPopup(event$id, event$lat, event$lng)
    })
  })
}

shinyApp(ui = ui, server = server)
