#
# This is a Shiny web application. You can run the application by clicking
# the 'Run App' button above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(here)
library(sf)
library(plotly)

airbnb <- read.csv(here("fall_2024/slides/session6_shiny_intro/data/airbnb_listings.csv"))
dc_state <- st_read(here('fall_2024/slides/session6_shiny_intro/data/Wards_from_2022.geojson'))


# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("DC airnb listings"),
  sidebarLayout(
    sidebarPanel(
      radioButtons(inputId= "input_roomtype",
                   label = "Room types to show:",
                   choices = unique(airbnb$room_type),
                   selected = "Entire home/apt"),
      sliderInput(inputId = "input_price",
                  label = "Price range:",
                  min = min(airbnb$price),
                  max = max(airbnb$price),
                  value = c(50, 200)),
      downloadButton("csv_download", "Download the results")
    ),
    mainPanel(
      plotlyOutput("map")
      #tableOutput("filtered_data")
    )
  )
)

# Define server logic required to draw a histogram
server <- function(input, output) {
  d <- reactive({airbnb %>% filter(room_type ==
                                     input$input_roomtype &
                                     price >= 
                                     input$input_price[1] &
                                     price <= 
                                     input$input_price[2])
  })
  d_map <- reactive({d() %>%
      st_as_sf(coords = c("longitude", "latitude"), 
               crs = st_crs(dc_state))
  })
  output$filtered_data <- renderTable(
    {
      d() %>% 
        select(id,host_name, name,
               room_type, price, neighbourhood) %>%
        arrange(price)},
    rownames = FALSE
  )
  output$map <- renderPlotly({
    p <- ggplot(dc_state) +
      geom_sf(data = d_map(), color = "red", alpha = 0.5,
              size = 0.5,
              aes(text = sprintf("Airbnb name: %s;<br>price: $%d",
                                 name, price))) +
      geom_sf(color = "black", fill = "white", alpha= 0.1) +
      theme_void() 
    ggplotly(p)
    }
  )
  output$csv_download <- downloadHandler(
    filename = function(){paste("airbnbfiltered-", 
                                Sys.time(),
                                ".csv", sep = "")},
    content = function(file){
      write.csv(d(), file)
    }
  )
}

# Run the application 
shinyApp(ui = ui, server = server)
