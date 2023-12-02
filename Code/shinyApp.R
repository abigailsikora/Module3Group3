library(jsonlite)
library(shiny)
library(leaflet)
library(ggplot2)
library(dplyr)
library(topicmodels)
library(tidytext)
# Read the JSON files
business_data <- fromJSON('nashville_bars_nightlife.json')
reviews <- fromJSON("filtered_reviews.json")

# Convert reviews to a data frame
reviews_df <- as.data.frame(reviews)


# Create a data frame from the first JSON file
business <- as.data.frame(business_data)
unique_postal_codes <- unique(business$postal_code)
unique_business_names <- unique(business$name)
unique_business_ids <- unique(business$business_id)


# Define UI for application that draws a histogram
ui <- fluidPage(
  titlePanel(
    div(
      style = "text-align: center; color: #333; font-size: 24px; font-weight: bold;",
      "Bars and Nightlife in Nashville, TN"
    )
  ),
  HTML('
  <div style="text-align: center; margin-top: 10px;">
    <p style="font-size: 14px; color: #666; margin-bottom: 5px;">
      The data for the project focuses on businesses in Yelp that had a category label containing either "bar" or "nightlife."
    </p>
    <p style="font-size: 12px; color: #888;">
      If you have questions, please email:
      <a href="mailto:amsikora2@wisc.edu" style="color: #007BFF;">amsikora2@wisc.edu</a>,
      <a href="mailto:swang2297@wisc.edu" style="color: #007BFF;">swang2297@wisc.edu</a>, or
      <a href="mailto:vborwankar@wisc.edu" style="color: #007BFF;">vborwankar@wisc.edu</a>
    </p>
  </div>
'),
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
    tabPanel("Choose a Business", 
             selectInput("postalCode", "Select Postal Code", choices = unique_postal_codes),
             selectInput("business", "Select a Business", choices = unique_business_names),
             plotOutput("wordFrequencyPlot")
    ),
    tabPanel("Advice", 
             HTML("<div class='advice-container'>
                    <h4>Advice to consider:</h4>
                    <p>1. Happy hours do not impact star rating by much. The average star difference between those that do have a happy hour and those that don't is less than 0.3. Therefore it is not important in considering having a happy hour or not.</p>
                  </div>"),
             plotOutput("happyhour"))
  )
)



# Define server logic required to draw a histogram
server <- function(input, output, session) {
  observeEvent(input$postalCode, {
    # Update business dropdown based on selected postal code
    updateSelectInput(session, "business", choices = unique(business$name[business$postal_code == input$postalCode]))
  })
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
        title = "Star Rating"
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
  getReviews <- function(selectedBusiness) {
    filtered_reviews <- reviews$text[reviews$business_id == selectedBusiness]
    return(filtered_reviews)
  }
  

}


  
  
  


# Run the application 
shinyApp(ui = ui, server = server)
