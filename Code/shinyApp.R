
library(jsonlite)

# Read the JSON files
business_data <- fromJSON('nashville_bars_nightlife.json')


# Create a data frame from the first JSON file
business <- as.data.frame(business_data)


library(shiny)
library(leaflet)

# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Bars and Nightlife in Nashville, TN"),
  
  # Tabs
  tabsetPanel(
    tabPanel("All Businesses",
             leafletOutput("map")),
    tabPanel("Selected Business Overview", 
             selectInput("postalCode", "Select Postal Code", choices = unique(business$postal_code))),
    tabPanel("Advice", 
             plotOutput("histogram")),
  )

)

# Define server logic required to draw a histogram
server <- function(input, output) {
  # Render leaflet map
  output$map <- renderLeaflet({
    leaflet(business) %>% 
      addProviderTiles("OpenStreetMap.Mapnik") %>% 
      addCircleMarkers(
        lat = ~latitude,
        lng = ~longitude,
        radius = ~sqrt(review_count) / 2,  # Adjust the scaling factor as needed
        fillOpacity = 0.7,
        color = ~colorNumeric(palette = viridisLite::viridis(9, direction = -1), domain = c(1, 5))(stars),
        popup = ~paste("Business: ", name, "<br>",
                       "Star Rating: ", stars, "<br>",
                       "Number of Reviews: ", review_count)
      ) %>%
      addLegend(
        position = "bottomright",
        colors = viridisLite::viridis(9, direction = -1),
        labels = c("1", "1.5", "2", "2.5", "3", "3.5", "4", "4.5", "5"),
        opacity = 1,
        title = "Average Star Rating"
      )%>%
      addControl(
        html = '<div id="size-label"><strong>Circle Size:</strong> Number of Reviews</div>',
        position = "bottomleft"
      )
  })
  
  
  
  
  
}

# Run the application 
shinyApp(ui = ui, server = server)
