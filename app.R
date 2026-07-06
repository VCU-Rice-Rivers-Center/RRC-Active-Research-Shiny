library(rstudioapi)
library(arcgis)
library(sf)
library(shiny)
library(leaflet)
library(bslib)
library(stringr)
library(htmltools)
library(rsconnect)
library(DT)

### Initialization ###

# Feature Layer url
f_url <- "https://services1.arcgis.com/acqg5b8xOcwTUl4z/arcgis/rest/services/RiceRiversCenter_ProjectRegistration_V2_Public_View/FeatureServer"

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

## Map Center point
point <- c(median(c(aoi_bbox[1], aoi_bbox[3])), median(c(aoi_bbox[2], aoi_bbox[4])))

# Functions for formatting UI inputs
formatTopicsListUI <- function(projects) {
  # Pull topics entered for existing projects
  topics <- unlist(lapply(projects$topics, function(x) { unlist(strsplit(x, ",")) }))
  topicsOther <- unlist(lapply(projects$topics_other, function(x) { unlist(strsplit(x, ",")) }))
  topicsOther <- unlist(lapply(topicsOther, function(x) { ifelse(str_count(x, " ") > 3, NA, x) }))

  # Combine columns and format strings to user-facing 
  topicsCombinedVals <- unique(c(topics, topicsOther))
  topicsCombined <- unlist(lapply(topicsCombinedVals, function(x) { str_to_title(gsub('([[:upper:]])', ' \\1', x)) }))
  
  # Create named list
  names(topicsCombinedVals) <- topicsCombined
  
  # Remove 'NA' and 'Other' from selection
  topicsCombinedVals <- sort(topicsCombinedVals[!topicsCombinedVals %in% c(NA, "other")])
  
  return(topicsCombinedVals)
}

formatPIListUI <- function(projects) {
  # Pull project leads entered for existing projects
  leads <- projects$projectLead
  leadsOther <- projects$projectLead_other
  
  # Combine columns and format strings to user-facing
  leadsCombinedVals <- unique(c(leads, leadsOther))
  leadsCombined <- unlist(lapply(leadsCombinedVals, function(x) { str_to_title(gsub('([[:upper:]])', ' \\1', x)) }))
  
  # Create named list
  names(leadsCombinedVals) <- leadsCombined
  
  # Remove 'NA' and 'Other' from selection
  leadsCombinedVals <- sort(leadsCombinedVals[!leadsCombinedVals %in% c(NA, "other")])
  
  return(leadsCombinedVals)
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
  associatesCombined <- sort(associatesCombined[!associatesCombined %in% c(NA, "other")])
  
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

ui <- page_navbar(
  theme = bs_theme(
    bootswatch = "yeti", 
    version = 5,
    base_font = font_google("Roboto")
  ),
  
  title = "RRC Project Viewer",
  
  # Sidebar using standard, reliable settings
  sidebar = sidebar(
    title = "Map Filters",
    open = "closed",
    # Simply listing the inputs here is standard and highly mobile-responsive.
    # On mobile, bslib natively collapses this into a top toggle strip.
    selectInput(inputId = "selectTopics", label = "Filter by Topics: ",
                choices = formatTopicsListUI(projects_sf), multiple = TRUE),
    
    selectInput(inputId = "selectPI", label = "Filter by PI: ", 
                choices = formatPIListUI(projects_sf), multiple = TRUE),
    
    selectInput(inputId = "selectStatus", label = "Filter by Status: ",
                choices = c("In Progress", "Complete", "Any Status"),
                selected = "Any Status")
  ),
  
  # Map Explorer Panel
  nav_panel(
    title = "Map Explorer", 
    
    # Grid Row: Spacing utility 'g-3' adds clean gaps between columns
    div(class = "row g-3",
        
        # COLUMN 1: The Map
        # 'col-12' means full width on mobile. 'col-lg-7' means 7/12 width on desktops.
        div(class = "col-12 col-lg-7",
            card(
              style = "height: 550px; padding: 0;", # Consistent height across devices
              leafletOutput("mymap", height = "100%")
            )
        ),
        
        # COLUMN 2: The Details Pane (No longer floating!)
        # 'col-12' means full width on mobile (below map). 'col-lg-5' means 5/12 width on desktops.
        div(class = "col-12 col-lg-5",
            card(
              style = "height: 550px; overflow-y: auto;", # Scrollable if text is long
              class = "shadow-sm",
              
              card_header(
                class = "bg-light",
                h4("Project Details", class = "mb-0", style = "font-weight: 600;")
              ),
              
              card_body(
                uiOutput(outputId = "projectDetails")
              )
            )
        )
    )
  ),
  
  # Table View Panel
  nav_panel(
    title = "Table View", 
    card(
      DT::dataTableOutput("projectDT")
    )
  )
)


server <- function(input, output) {
  
  # Trigger the modal on app startup
  showModal(modalDialog(
    title = "Welcome to the RRC Project Viewer",
    tagList(
      p("This is an interactive application for exploring research projects at the Rice Rivers
      Center. This pop-up outlines different ways to interact with the application."),
      p("The navigation bar at the top allows you to view the data from a map of Rice or from a table."),
      h5("Map View"),
      p("With the map view, hover over different points to get an overview of the project. Selecting
        a point will populate the Project Details pane with more information about the project."),
      h5("Table View"),
      p("With the table view, all projects are listed in one table. You can sort by different columns or
        search for keywords or names using the searchbar in the top right corner"),
      h5("Additional Filters"),
      p("To filter the map and table elements by project status, PI, or project topics, you can toggle open
        the sidebar located on the left side of the screen.")
      
    ),
    easyClose = TRUE,
    footer = modalButton("Dismiss")
  ))
  
  # Offset for map
  bbox_offset = 0.005
  
  # Create the map
  output$mymap <- renderLeaflet({
    leaflet() |>
      addProviderTiles(providers$Esri.WorldImagery) |>
      # fitBounds(aoi_bbox[1] + bbox_offset, aoi_bbox[2], aoi_bbox[3] + bbox_offset, aoi_bbox[4]) |>
      fitBounds(aoi_bbox[1], aoi_bbox[2], aoi_bbox[3], aoi_bbox[4]) |>
      
      addPolygons(data = aoi_sf, color = "#006894", opacity = 0.8, fillOpacity = 0) 
      
  })
  
  # This observer is responsible for maintaining the markers + labels
  observe({
    # Create copy of global df
    filtered_projects <- projects_sf
    
    # Read user inputs
    topicFilter <- input$selectTopics
    piFilter <- input$selectPI
    statusFilter <- input$selectStatus
    
    # Filter projects dataframe based on user input
    if (length(topicFilter) > 0) {
      # Filter by topic
      projectsTopicFilter <- filtered_projects[grep(paste(topicFilter, collapse="|"), filtered_projects$topics),]
      projectsOtherTopicFilter <- filtered_projects[grep(paste(topicFilter, collapse="|"), filtered_projects$topics_other),]
      filtered_projects <- rbind(projectsTopicFilter, projectsOtherTopicFilter)
    }
    
    if (length(piFilter) > 0) {
      # Filter by PI
      projectsPIFilter <- filtered_projects[grep(paste(piFilter, collapse="|"), filtered_projects$projectLead),]
      projectsOtherPIFilter <- filtered_projects[grep(paste(piFilter, collapse="|"), filtered_projects$projectLead_other),]
      filtered_projects <- rbind(projectsPIFilter, projectsOtherPIFilter)
    }
    
    if (statusFilter == "In Progress") {
      # Filter by status
      currentYear <- as.numeric(format(as.Date(Sys.Date(), format = "%Y-%m-%d"), "%Y")) # Current year
      filtered_projects <- filtered_projects[(as.numeric(filtered_projects$yearStart) <= currentYear & as.numeric(filtered_projects$yearEnd) >= currentYear),]
    } else if (statusFilter == "Complete") {
      currentYear <- as.numeric(format(as.Date(Sys.Date(), format = "%Y-%m-%d"), "%Y")) # Current year
      filtered_projects <- filtered_projects[(as.numeric(filtered_projects$yearEnd) < currentYear),]
    }
    
    if (nrow(filtered_projects) > 0) {
      # Pre-calculate the labels for each point
      labels <- lapply(seq_len(nrow(filtered_projects)), function(i) {
        project <- filtered_projects[i, ]
        HTML(paste(
          tags$span(style="color:#006894;font-weight:bold", "Project Title: "), str_to_title(as.character(project$projectTitle)), "<br/>",
          tags$span(style="color:#006894;font-weight:bold", "PI: "), as.character(formatProjectLead(project)), "<br/>",
          tags$span(style="color:#006894;font-weight:bold", "Status: "), as.character(formatStatus(project))
        ))
      })
      
      leafletProxy("mymap", data = filtered_projects) |>
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
    } else {
      leafletProxy("mymap", data=NULL) |>
        clearMarkers()
    }
    
  
  })

  # UI output for project details
  output$projectDetails <- renderUI({
    selectedProject <- projects_sf[projects_sf$globalid == input$mymap_marker_click$id,]
    
    if (nrow(selectedProject) == 0) {
      return(p("Click a marker to see details."))
    }
    
    content <- tagList(
      tags$h4(str_to_title(as.character(selectedProject$projectTitle))),
      #tags$hr(),
      HTML(paste(tags$span(style="color:#006894;font-weight:bold", "Topics: "), as.character(formatTopics(selectedProject)))),
      tags$br(),
      HTML(paste(tags$span(style="color:#006894;font-weight:bold", "PI: "), as.character(formatProjectLead(selectedProject)))),
      tags$br(),
      HTML(paste(tags$span(style="color:#006894;font-weight:bold", "Project Associates: "), as.character(formatProjectAssociates(selectedProject)))),
      tags$br(),
      HTML(paste(tags$span(style="color:#006894;font-weight:bold", "Project Objectives: "), selectedProject$projectObjectives)),
      tags$br(),
      # HTML(paste(tags$span(style="color:#006894;font-weight:bold", "Project Methods: "), selectedProject$projectMethods)),
      # tags$br(),
      # tags$br(),
      HTML(paste(tags$span(style="color:#006894;font-weight:bold", "Status: "), as.character(formatStatus(selectedProject))))
    )
    
    p(content)
  })
  
  ## Data Table #############
  
  # Initialize data table:
  # filtered_projects_df <- NULL
    
  # This observer is responsible for maintaining the datatable
  observe({
    # Create copy of global df
    filtered_projects_df <- projects_sf
    
    # Read user inputs
    topicFilter <- input$selectTopics
    piFilter <- input$selectPI
    statusFilter <- input$selectStatus
    
    # Filter projects dataframe based on user input
    if (length(topicFilter) > 0) {
      # Filter by topic
      projectsTopicFilter <- filtered_projects_df[grep(paste(topicFilter, collapse="|"), filtered_projects_df$topics),]
      projectsOtherTopicFilter <- filtered_projects_df[grep(paste(topicFilter, collapse="|"), filtered_projects_df$topics_other),]
      filtered_projects_df <- rbind(projectsTopicFilter, projectsOtherTopicFilter)
    }
    
    if (length(piFilter) > 0) {
      # Filter by PI
      projectsPIFilter <- filtered_projects_df[grep(paste(piFilter, collapse="|"), filtered_projects_df$projectLead),]
      projectsOtherPIFilter <- filtered_projects_df[grep(paste(piFilter, collapse="|"), filtered_projects_df$projectLead_other),]
      filtered_projects_df <- rbind(projectsPIFilter, projectsOtherPIFilter)
    }
    
    if (statusFilter == "In Progress") {
      # Filter by status
      currentYear <- as.numeric(format(as.Date(Sys.Date(), format = "%Y-%m-%d"), "%Y")) # Current year
      filtered_projects_df <- filtered_projects_df[(as.numeric(filtered_projects_df$yearStart) <= currentYear & as.numeric(filtered_projects_df$yearEnd) >= currentYear),]
    } else if (statusFilter == "Complete") {
      currentYear <- as.numeric(format(as.Date(Sys.Date(), format = "%Y-%m-%d"), "%Y")) # Current year
      filtered_projects_df <- filtered_projects_df[(as.numeric(filtered_projects_df$yearEnd) < currentYear),]
    }
    
    if (nrow(filtered_projects_df) > 0) {
      # Formatted columns
      filtered_projects_df$titleFormat <- str_to_title(filtered_projects_df$projectTitle)
      filtered_projects_df$leadFormat <- apply(filtered_projects_df, 1, formatProjectLead)
      filtered_projects_df$associatesFormat <- apply(filtered_projects_df, 1, formatProjectAssociates)
      filtered_projects_df$topicsFormat<- apply(filtered_projects_df, 1, formatTopics)
      
      # Select columns to keep
      filtered_projects_df <- as.data.frame(filtered_projects_df)
      filtered_projects_df <- filtered_projects_df[, c("titleFormat", "leadFormat", "associatesFormat", "topicsFormat", "yearStart", "yearEnd")]

    } else {
      # Empty df if no results  returned
      filtered_projects_df <- data.frame(titleFormat = character(),
                                         leadFormat = character(),
                                         associatesFormat = character(), 
                                         topicsFormat = character(), 
                                         yearStart = character(),
                                         yearEnd = character())
    }
    
    # DT Output
    output$projectDT <- DT::renderDataTable({
      cNames <- c("Project Title", "PI", "Project Associates", "Topics", "Start Year", "End Year")
      DT::datatable(filtered_projects_df, colnames = cNames)
    })
  })
}

shinyApp(ui = ui, server = server)
