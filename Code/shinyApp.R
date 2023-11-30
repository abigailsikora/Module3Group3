library(jsonlite)
library(shiny)
library(leaflet)
library(ggplot2)
library(dplyr)
# Read the JSON files
business_data <- fromJSON('nashville_bars_nightlife.json')


# Create a data frame from the first JSON file
business <- as.data.frame(business_data)


# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel("Bars and Nightlife in Nashville, TN"),
  HTML("<h6>The data for the project focuses on businesses in Yelp that had a category label that contained either bar or nightlife.<h6>
       <h7>If you have questions please email: amsikora2@wisc.edu, swang2297@wisc.edu, or vborwankar@wisc.edu<h7>"),
  
  # Tabs
  tabsetPanel(
    tabPanel("All Businesses",
             fluidRow(
               column(3,
                      sliderInput("starFilter", "Filter by Star Rating",
                                  min = 1, max = 5, value = c(1, 5), step = 0.1),
                      sliderInput("reviewFilter", "Filter by Number of Reviews",
                                  min = 0, max = max(business$review_count, na.rm = TRUE), value = c(0, max(business$review_count, na.rm = TRUE)))
               ),
               column(9,
                      leafletOutput("map"),
                      verbatimTextOutput("averageInfo")
               )
             )
    ),
    tabPanel("Selected Business Overview", 
             selectInput("business", "Select a business", choices = business$name)),
    tabPanel("Advice", 
             HTML("<div class='advice-container'>
                    <h4>Advice to consider:</h4>
                    <p>1. Happy hours do not impact star rating by much. The average star difference between those that do have a happy hour and those that don't is less than 0.3. Therefore it is not important in considering having a happy hour or not.</p>
                  </div>"),
             plotOutput("happyhour"))
  )
)



# Define server logic required to draw a histogram
server <- function(input, output) {
  # Render leaflet map
  output$map <- renderLeaflet({
    filtered_business <- filter(business, 
                                stars >= input$starFilter[1] & stars <= input$starFilter[2] &
                                  review_count >= input$reviewFilter[1] & review_count <= input$reviewFilter[2])
    
    leaflet(filtered_business) %>% 
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
  output$happyhour <- renderPlot({
    filtered<-subset(business, !is.na(attributes$HappyHour))
    ggplot(filtered, aes(x = attributes$HappyHour, y = stars)) +
      geom_bar(stat = "summary", fun = "mean", position = "dodge", fill = "skyblue") +
      labs(title = "Impact of Happy Hour on Star Rating",
           x = "Happy Hour",
           y = "Average Star Rating")
  })
  output$averageInfo <- renderPrint({
    avg_stars <- mean(business$stars, na.rm = TRUE)
    avg_reviews <- mean(business$review_count, na.rm = TRUE)
    cat("Average Star Rating: ", avg_stars, "\n")
    cat("Average Number of Reviews: ", avg_reviews, "\n")
  })

}
  
  
  


# Run the application 
shinyApp(ui = ui, server = server)
