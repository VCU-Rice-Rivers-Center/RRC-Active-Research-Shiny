library(rstudioapi)
library(arcgis)
library(sf)
library(shiny)
library(leaflet)
library(bslib)
library(stringr)
library(htmltools)
library(rsconnect)

### Initialization ###

# Feature Layer url
f_url <- "https://services1.arcgis.com/acqg5b8xOcwTUl4z/arcgis/rest/services/Rice_Rivers_Center_-_Project_Registration_Public_View/FeatureServer"

# Open feature layer (this contains the metadata for the layer)
f_layer <- arc_open(f_url)

# Read the data
projects <- get_layer(f_layer, id=0)
projects_sf <- arc_select(projects)

# RRC Boundary File
aoi_url <- "data/RRC_ROI.shp"
aoi_sf <- st_read(aoi_url) |>
  st_transform(crs = 4326)

# RRC Bounding Box
aoi_bbox <- st_bbox(aoi_sf) |>
  as.vector()

# Functions for formatting UI inputs
formatTopicsListUI <- function(projects) {
  # Pull topics entered for existing projects
  topics <- unlist(lapply(projects$topics, function(x) { unlist(strsplit(x, ",")) }))
  topicsOther <- unlist(lapply(projects$topics_other, function(x) { unlist(strsplit(x, ",")) }))
  topicsOther <- unlist(lapply(topicsOther, function(x) { ifelse(str_count(x, " ") > 3, NA, x) }))

  # Combine columns and format strings to user-facing 
  topicsCombinedVals <- unique(c(topics, topicsOther))
  topicsCombined <- unlist(lapply(topicsCombinedVals, function(x) { str_to_title(gsub('([[:upper:]])', ' \\1', x)) }))
  
  # Create names list
  names(topicsCombinedVals) <- topicsCombined
  print(class(topicsCombinedVals))
  
  # Remove 'NA' and 'Other' from selection
  topicsCombinedVals <- sort(topicsCombinedVals[!topicsCombinedVals %in% c(NA, "Other")])
  
  return(topicsCombinedVals)
}

formatPIListUI <- function(projects) {
  # Pull project leads entered for existing projects
  leads <- projects$projectLead
  leadsOther <- projects$projectLead_other
  
  # Combine columns and format strings to user-facing
  leadsCombined <- unique(c(leads, leadsOther))
  leadsCombined <- unlist(lapply(leadsCombined, function(x) { str_to_title(gsub('([[:upper:]])', ' \\1', x)) }))
  
  # Remove 'NA' and 'Other' from selection
  leadsCombined <- sort(leadsCombined[!leadsCombined %in% c(NA, "Other")])
  
  return(leadsCombined)
}

# Functions for formatting popup details
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

# Format project associate names
formatProjectAssociates <- function(project) {
  associates <- unlist(lapply(project$projectAssociates, function(x) { unlist(strsplit(x, ",")) }))
  associatesOther <- unlist(lapply(project$projectAssociates_other, function(x) { unlist(strsplit(x, ",")) }))
  
  # Combine columns and format strings to user-facing
  associatesCombined <- unique(c(associates, associatesOther))
  associatesCombined <- unlist(lapply(associatesCombined, function(x) { gsub('([[:upper:]])', ' \\1', x) }))
  
  # Remove 'NA' and 'Other' from selection
  associatesCombined <- sort(associatesCombined[!associatesCombined %in% c(NA, "Other")])
  
  # Format character string
  if (length(associatesCombined) == 0) {
    return("None")
  } else {
    return(paste(str_to_title(as.character(associatesCombined)), collapse=", "))
  }
}

formatTopics <- function(project) {
  # Pull topics entered for existing projects
  topics <- unlist(lapply(project$topics, function(x) { unlist(strsplit(x, ",")) }))
  topicsOther <- unlist(lapply(project$topics_other, function(x) { unlist(strsplit(x, ",")) }))
  topicsOther <- unlist(lapply(topicsOther, function(x) { ifelse(str_count(x, " ") > 3, NA, x) }))
  
  # Combine columns and format strings to user-facing 
  topicsCombined <- unique(c(topics, topicsOther))
  topicsCombined <- unlist(lapply(topicsCombined, function(x) { str_to_title(gsub('([[:upper:]])', ' \\1', x)) }))
  
  # Remove 'NA' and 'Other' from selection
  topicsCombined <- sort(topicsCombined[!topicsCombined %in% c(NA, "Other")])
  
  # Format character string
  if (length(topicsCombined) == 0) {
    return("NA")
  } else {
    return(paste(str_to_title(as.character(topicsCombined)), collapse=", "))
  }
  
  return(topicsCombined)
}

# Format status (active vs. inactive)
formatStatus <- function(project) {
  startYear <- as.numeric(project$yearStart) # Project start year
  endYear <- as.numeric(project$yearEnd) # Project end year
  
  currentYear <- as.numeric(format(as.Date(Sys.Date(), format = "%Y-%m-%d"), "%Y")) # Current year
  
  status <- ifelse((startYear <= currentYear) && (currentYear <= endYear), "In Progress", "Complete")
  return(status)
}


### Shiny UI ###


ui <- page_sidebar(
  title = "Rice Rivers Center - Project Viewer",
  
  sidebar = sidebar(
    title = "Map Filters",
    selectInput(inputId = "selectTopics", label = "Filter by Topics: ",
                   choices = formatTopicsListUI(projects_sf), multiple = TRUE),
    selectInput(inputId = "selectPI", label = "Filter by PI: ", 
                    choices = formatPIListUI(projects_sf), multiple = TRUE),
    selectInput(inputId = "selectStatus", label = "Filter by Status: ",
                    choices = c("In Progress", "Complete", "Any Status"),
                    selected = "Any Status")
  ),
  
  div(
      
      tags$head(
        # Include our custom CSS
        includeCSS("styles.css")
      ),
      
  tags$style(type = "text/css", "#mymap {height: calc(100vh - 80px) !important;}"),
  leafletOutput("mymap"), 
  absolutePanel(id = "controls", class = "panel panel-default", fixed = TRUE,
                draggable = TRUE, top = 120, left = "auto", right = 70, bottom = "auto",
                width = 400, height = "80%",
                
                h2("Project Details"),
                
                uiOutput(outputId= "projectDetails")
  )
  )
)

server <- function(input, output) {
  
  # Offset for map
  bbox_offset = 0.005
  
  # Create the map
  output$mymap <- renderLeaflet({
    leaflet() |>
      addProviderTiles(providers$Esri.WorldImagery) |>
      fitBounds(aoi_bbox[1] + bbox_offset, aoi_bbox[2], aoi_bbox[3] + bbox_offset, aoi_bbox[4]) |>
      addPolygons(data = aoi_sf, color = "#006894", opacity = 0.8, fillOpacity = 0) 
      
  })
  
  # This observer is responsible for maintaining the markers + labels
  observe({
    # Pre-calculate the labels for each point
    labels <- lapply(seq_len(nrow(projects_sf)), function(i) {
      project <- projects_sf[i, ]
      HTML(paste(
        tags$span(style="color:#006894;font-weight:bold", "Project Title: "), str_to_title(as.character(project$projectTitle)), "<br/>",
        tags$span(style="color:#006894;font-weight:bold", "PI: "), as.character(formatProjectLead(project)), "<br/>",
        tags$span(style="color:#006894;font-weight:bold", "Status: "), as.character(formatStatus(project))
      ))
    })
    
    leafletProxy("mymap", data = projects_sf) |>
      clearMarkers() |> # Good practice to clear before re-adding in an observer
      addCircleMarkers(
        layerId = ~globalid, 
        color = "#FFB300", 
        stroke = TRUE, 
        opacity = 0.9, 
        fillOpacity = 0.3,
        label = labels, # Add the labels here
        labelOptions = labelOptions(
          style = list(
            "font-weight" = "normal", 
            "padding" = "8px",
            "width" = "320px",      # Limits the width of the label
            "white-space" = "normal",   # Allows text to wrap to the next line
            "word-wrap" = "break-word"  # Ensures long words don't overflow
          ),
          textsize = "13px",
          direction = "auto"
        )
      )
  })

  # UI output for project details
  output$projectDetails <- renderUI({
    selectedProject <- projects_sf[projects_sf$globalid == input$mymap_marker_click$id,]
    
    if (nrow(selectedProject) == 0) {
      return(p("Click a marker to see details."))
    }
    
    content <- tagList(
      tags$h4(str_to_title(as.character(selectedProject$projectTitle))),
      tags$hr(),
      HTML(paste(tags$span(style="color:#006894;font-weight:bold", "Topics: "), as.character(formatTopics(selectedProject)))),
      tags$br(),
      tags$br(),
      HTML(paste(tags$span(style="color:#006894;font-weight:bold", "PI: "), as.character(formatProjectLead(selectedProject)))),
      tags$br(),
      tags$br(),
      HTML(paste(tags$span(style="color:#006894;font-weight:bold", "Project Associates: "), as.character(formatProjectAssociates(selectedProject)))),
      tags$br(),
      tags$br(),
      HTML(paste(tags$span(style="color:#006894;font-weight:bold", "Project Objectives: "), selectedProject$projectObjectives)),
      tags$br(),
      tags$br(),
      HTML(paste(tags$span(style="color:#006894;font-weight:bold", "Project Methods: "), selectedProject$projectMethods)),
      tags$br(),
      tags$br(),
      HTML(paste(tags$span(style="color:#006894;font-weight:bold", "Status: "), as.character(formatStatus(selectedProject))))
    )
    
    p(content)
  })

}

shinyApp(ui = ui, server = server)
