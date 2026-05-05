library(rstudioapi)
library(arcgis)
library(sf)
library(shiny)
library(leaflet)
library(bslib)

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
  
  output$map <- renderLeaflet({
    leaflet() |>
      addTiles() |>
      fitBounds(aoi_bbox[1], aoi_bbox[2], aoi_bbox[3], aoi_bbox[4]) |>
      addMarkers(data = projects_sf) |>
      addPolygons(data = aoi_sf, fillOpacity = 0)
  })
  
}

shinyApp(ui = ui, server = server)
