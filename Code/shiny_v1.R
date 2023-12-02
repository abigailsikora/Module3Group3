library(jsonlite)
library(shiny)
library(leaflet)
library(ggplot2)
library(dplyr)
library(tm)
library(tidytext)
library(tidyr)

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
               ),
             ),
    ),
    tabPanel("Selected Business Overview", 
             selectInput("postalCode", "Select Postal Code", choices = unique_postal_codes),
             selectInput("business", "Select a Business", choices = unique_business_names),
             mainPanel(
               tableOutput("sentimentScores")
             )
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
  

# Seniment Analysis Code:

  sentiment_analysis <- reactive({
    # Filter reviews based on the selected business name
    selected_business_reviews <- reviews_df %>%
      filter(business_id == unique_business_ids[business$name == input$business])
    
    # Preprocess text
    corpus <- Corpus(VectorSource(selected_business_reviews$text))
    corpus <- tm_map(corpus, content_transformer(tolower)) %>%
      tm_map(removePunctuation) %>%
      tm_map(removeNumbers) %>%
      tm_map(removeWords, stopwords("en")) %>%
      tm_map(stripWhitespace)
    
    # Aspects to analyze sentiment for
    aspects <- c("service", "ambiance", "food", "drinks", "time", "bar", "vibe", "music", "crowd")
    
    # Function to perform aspect-based sentiment analysis
    perform_aspect_sentiment_analysis <- function(reviews_data, aspect) {
      aspect_reviews <- reviews_data %>%
        filter(grepl(aspect, text, ignore.case = TRUE))
      
      aspect_reviews_tokens <- aspect_reviews %>%
        unnest_tokens(output = word, input = text) %>%
        inner_join(get_sentiments("afinn"), by = "word") %>%
        mutate(aspect = aspect) %>%
        group_by(business_id, aspect) %>%
        summarize(sentiment_score = mean(value, na.rm = TRUE))
      
      return(aspect_reviews_tokens)
    }
    
    # Initialize an empty list to store aspect-based sentiment analysis results
    aspect_sentiments <- list()
    
    # Iterate through aspects for sentiment analysis
    for (aspect in aspects) {
      aspect_sentiments[[aspect]] <- perform_aspect_sentiment_analysis(selected_business_reviews, aspect)
    }
    
    # Combine aspect-based sentiment analysis results into a single dataframe
    aspect_sentiments_combined <- bind_rows(aspect_sentiments)
    
    # Aggregate sentiment scores by business_id and aspect
    aggregated_sentiments <- aspect_sentiments_combined %>%
      group_by(business_id, aspect) %>%
      summarize(avg_sentiment_score = mean(sentiment_score, na.rm = TRUE))
    
    # Pivot the data to have aspects as columns
    pivot_result <- aggregated_sentiments %>%
      pivot_wider(names_from = aspect, values_from = avg_sentiment_score, values_fill = NA)
    
    return(pivot_result)
  })
  
  # Display the sentiment analysis results in a table
  output$sentimentScores <- renderTable({
    sentiment_analysis()
  })

}

# Run the application 
shinyApp(ui = ui, server = server)