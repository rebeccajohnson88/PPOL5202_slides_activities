#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(tidyverse)
library(leaflet)
library(leaflet.extras)
library(htmltools)
library(sf)
library(here)

homeless_facilities <- sf::st_read(here("fall_2024/slides/session5_geospatial_R/Homeless_Service_Facilities.geojson")) %>%
  st_transform(crs = 4326) %>%
  mutate(target_clean = case_when(trimws(TARGET) %in% 
                              c("Children", "Families",
                                "Youth") ~ "Children + Families",
                              is.na(TARGET) ~ "Unknown",
                              TRUE ~ trimws(TARGET)))



ui <- fluidPage(
  # Application title
  titlePanel("Map of Homeless Facilities in DC"),
  sidebarLayout(
    sidebarPanel(
      selectInput(inputId= "input_type",
                  label = "Type of facility:",
                  choices = sort(unique(homeless_facilities$target_clean)),
                  selected = "Men & Women",
                  multiple = TRUE)
    ),
    mainPanel(
      # Leaflet output
      leafletOutput("map"),
      tableOutput("selected_sites")
    )
  )

)

# Define server logic required to draw a histogram
server <- function(input, output) {
  
  d <- reactive({
    homeless_facilities %>%
      filter(target_clean %in% input$input_type)
    
  })

  output$map <- renderLeaflet({
    leaflet() %>%
      addProviderTiles("OpenStreetMap",
                       group = "OpenStreetMap")  %>%
      addCircleMarkers(data = d(), lat = ~LATITUDE,
                       lng = ~LONGITUDE,
                       label = sprintf("Name: %s<br>Target groups: %s",
                                       d()$PROGRAM_NAME,
                                       d()$target_clean) %>%
                         lapply(htmltools::HTML),
                       color = 'darkgreen', radius = 2) %>%
      addDrawToolbar(targetGroup = "draw",
                     polylineOptions = FALSE,
                     circleMarkerOptions = FALSE,
                     markerOptions = FALSE,
                     circleOptions = FALSE,
                     rectangleOptions = FALSE)
  })
  
  output$selected_sites <- renderTable({
    req(input$map_draw_stop)
    polygon_coordinates <- input$map_draw_new_feature$geometry$coordinates[[1]]
    drawn_polygon <- st_sfc(st_polygon(list(do.call(rbind,
                                  lapply(polygon_coordinates,function(x){c(x[[1]][1],x[[2]][1])}))))) 
    print("draw polygon complete")
    st_crs(drawn_polygon) <- st_crs(d())
    print("set crs of polygon")
    homeless_overlap_list <- as.data.frame(st_intersects(d(),
                            drawn_polygon))
    print("found intersecting rows")
    rows_keep <- intersect(row.names(d()), as.character(homeless_overlap_list$row.id))
    homeless_df <- d() %>% st_drop_geometry()
    homeless_sub <- homeless_df %>% filter(row.names(homeless_df) %in% rows_keep) %>%
                select(PROGRAM_NAME, WEBSITE_URL, target_clean)
  })

}

# Run the application 
shinyApp(ui = ui, server = server)