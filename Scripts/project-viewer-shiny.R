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

# Prepare UI inputs
formatTopicsList <- function(projects) {
  # Pull topics entered for existing projects
  topics <- unlist(lapply(projects$topics, function(x) { unlist(strsplit(x, ",")) }))
  topicsOther <- unlist(lapply(projects$topics_other, function(x) { unlist(strsplit(x, ",")) }))
  
  # Combine columns and format strings to user-facing 
  topicsCombined <- unique(c(topics, topicsOther))
  topicsCombined <- unlist(lapply(topicsCombined, function(x) { str_to_title(gsub('([[:upper:]])', ' \\1', x)) }))
  
  # Remove 'NA' and 'Other' from selection
  topicsCombined <- topicsCombined[!topicsCombined %in% c(NA, "Other")]
  
  return(topicsCombined)
}


### Shiny UI ###


ui <- page_sidebar(
  title = "Rice Rivers Center - Project Viewer",
  
  sidebar = sidebar(
    title = "Map Filters",
    selectInput(inputId = "selectTopics", label = "Filter by Topics: ",
                   choices = formatTopicsList(projects_sf), multiple = TRUE)
  ),
  
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
  # Format project lead name
  formatProjectLead <- function(project) {
    lead <- project$projectLead
    if (lead == "other") {
      lead <- project$projectLead_other # Free text parameter
    } else {
      lead <- gsub('([[:upper:]])', ' \\1', lead) # Str split at uppercase
    }
    return(str_to_title(lead)) # Return name, formatted as a title
  }
  
  # Format status (active vs. inactive)
  formatStatus <- function(project) {
    startYear <- as.numeric(project$yearStart) # Project start year
    endYear <- as.numeric(project$yearEnd) # Project end year
    
    currentYear <- as.numeric(format(as.Date(Sys.Date(), format = "%Y-%m-%d"), "%Y")) # Current year
    
    status <- ifelse((startYear <= currentYear) && (currentYear <= endYear), "In Progress", "Completed")
    return(status)
  }
  
  # Show a popup at a given location
  showPopup <- function(project, lat, lng) {
    selectedProject <- projects_sf[projects_sf$globalid == project,]
    content <- as.character(tagList(
      HTML(paste(tags$span(style="color:#006894;font-weight:bold", "Project Title: "), str_to_title(as.character(selectedProject$projectTitle)), sep = "")),
      tags$br(),
      HTML(paste(tags$span(style="color:#006894;font-weight:bold", "PI: "), as.character(formatProjectLead(selectedProject)))),
      tags$br(),
      HTML(paste(tags$span(style="color:#006894;font-weight:bold", "Status: "), as.character(formatStatus(selectedProject))))
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
