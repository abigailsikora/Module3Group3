library(jsonlite)
library(shiny)
library(leaflet)
library(ggplot2)
library(dplyr)
library(tm)
library(tidytext)
library(tidyr)
library(reshape2)

# Read the JSON files
business_data <- fromJSON('nashville_bars_nightlife.json')
reviews <- fromJSON("filtered_reviews.json")

# Convert reviews to a data frame
reviews_df <- as.data.frame(reviews)

# Create a data frame from the first JSON file
business <- as.data.frame(business_data)

sentiment_scores_by_postal_code <- read.csv("sentiment_by_postal_code.csv")
#print(head(sentiment_scores_by_postal_code))
sentiment_scores_business_id <- read.csv("sentiment_scores_by_business.csv")
#print(head(sentiment_scores_business_id))

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
               tableOutput("sentimentScores"),
               plotOutput("sentimentBarPlot")
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
        color = ~colorNumeric(palette = viridisLite::viridis(9, direction = 1), domain = c(1, 5))(stars),
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
  
  


  
  business_details <- reactive({
    selected_business_info <- sentiment_scores_business_id[sentiment_scores_business_id$business_id == unique_business_ids[business$name == input$business], ]
    selected_business_info
  })
  
  # Display selected business info in a table
  output$selectedBusinessInfo <- renderTable({
    business_details()
  })
  
  
  # Display the sentiment analysis results in a table
  #output$sentimentScores <- renderTable({
   # sentiment_analysis()
 # })
  
  output$sentimentBarPlot <- renderPlot({
    # Get the sentiment analysis results using the reactive function
    #sentiment_data <- sentiment_analysis()
    
    # Reshape the data for plotting (if needed, depending on your data format)
    business_scores <- pivot_longer(business_details(), col=colnames(business_details())[-c(1,2)], names_to='variable', values_to='value')
    
    # Filter sentiment scores by the selected postal code
    selected_postal_code <- input$postalCode
    postal_code_score_avgs <- subset(sentiment_scores_by_postal_code, postal_code == selected_postal_code)
    postal_code_score_avgs <- pivot_longer(postal_code_score_avgs, col=colnames(postal_code_score_avgs)[-1], names_to='variable', values_to='value')

    
    # Plotting the sentiment analysis data as a bar plot
    #TODO: add avg black bars here. another layer of geom_bar()
    ggplot(NULL, aes(variable, value)) +
      geom_bar(aes(fill="tomato3"), data=business_scores, stat = 'identity', show.legend = F) +
      geom_bar(aes(fill='steelblue3'), data=postal_code_score_avgs, stat = 'identity', show.legend = F) +
      #geom_bar(data = business_scores, aes(x = business_id, y = value, fill = variable), stat = "identity", position = "dodge") +
      #geom_bar(data = postal_code_scores, aes(x = postal_code, y = value), fill = "black", alpha = 0.5, stat ="identity", position = "dodge") +
      labs(title = "Sentiment Analysis by Aspect Across Businesses",
           y = "Sentiment Score") +
      theme_classic()
      #theme(axis.text.x = element_text(angle = 45, hjust = 1)) +
      #scale_fill_brewer(palette = "Set3") +  # Change the color palette if needed
      
  })

}

# Run the application 
shinyApp(ui = ui, server = server)