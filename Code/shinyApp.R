library(jsonlite)
library(shiny)
library(leaflet)
library(ggplot2)
library(dplyr)
library(tidytext)
library(tidyr)


sentiment_scores_by_postal_code <- read.csv("sentiment_by_postal_code.csv")
sentiment_scores_business_id <- read.csv("sentiment_scores_by_business.csv")
# Read the JSON files
business_data <- fromJSON('nashville_bars_nightlife.json')
census <- read.csv("census_postalcodes.csv")
census <- census %>%
  rename(
    `Postal Code` =postal_code,
    `Population` = population,
    `Median Household Income` = income_median_household,
    `Education` = eudcation,
    `Employment` = employment,
    `Households` = households,
    `Businesses` = business
  )

drinksdf <- data.frame( 
    Drink=c("Cocktail", "Wine", "Beer", "Tequila", "Whisky", "Brandy", "Vodka", "Sake"),
    `Sentiment Score`=c("0.30", "0.29", "0.27" ,"0.27","0.27","0.27","0.22","0.22")
  )

# Create a data frame from the first JSON file
business <- as.data.frame(business_data)
reviews <- fromJSON("filtered_reviews.json")

# Convert reviews to a data frame
reviews_df <- as.data.frame(reviews)
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
             fluidRow(
               column(3,
                      selectInput("postalCode", "Select Postal Code", choices = unique_postal_codes),
                      selectInput("business", "Select a Business", choices = unique_business_names)
               ),
               column(9,
                      h4("U.S. Census Data"),
                      tableOutput("censusPlot"),
                      h4("Number of Reviews Over Time"),
                      plotOutput("bus"),
                      plotOutput("sentimentBarPlot")
               )
             )
    ),
    tabPanel("Advice", 
             fluidRow(
               column(6, 
                      HTML("<div class='advice-container'>
              <h4>Advice to consider:</h4>
              <p>1. Happy hours do not impact star rating as discovered in two-sample t-tests. The average star difference between those that do have a happy hour and those that don't is less than 0.3. Therefore it is not important in considering having a happy hour or not.</p>
            </div>"),
                      HTML("<div class='advice-container'>
              <p>2. Location is important as discovered in ANOVA testing. Specifically, we advise opening a business in 37216 over 37221.</p>
            </div>"),
                      HTML("<div class='advice-container'>
              <p>3. Cocktails are the best drink to serve.It has the highest sentiment score. </p>
            </div>"),
               HTML("<div class='advice-container'>
              <p>4. We recommend you improve wait times as customers are sensitive to wait. 22.6% of reviews mentioned wait.</p>
            </div>"))
             ,
             column(6, 
                    plotOutput("happyhour"),
                    fluidRow(
                      column(6, tableOutput("drinks")),
                      column(6, plotOutput("postalcodeplot"))
                      )
                    )
    ))
))



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
           y = "Average Star Rating")+
      theme_minimal()
  })
  output$averageInfo <- renderPrint({
    avg_stars <- mean(business$stars, na.rm = TRUE)
    avg_reviews <- mean(business$review_count, na.rm = TRUE)
    cat("Average Star Rating: ", avg_stars, "\n")
    cat("Average Number of Reviews: ", avg_reviews, "\n")
  })

  output$censusPlot <-renderTable({
    census_filtered <- census %>%
      filter(`Postal Code` == input$postalCode)
})
  output$bus <- renderPlot({
    business_filtered <- business %>%
      filter(name == input$business)
    reviews_filtered <- reviews %>%
      filter(business_id == business_filtered$business_id) %>%
      mutate(date = as.Date(date),
             year = lubridate::year(date))
    
    # Calculate counts for each combination of year and star rating
    heatmap_data <- reviews_filtered %>%
      group_by(year, stars) %>%
      summarize(count = n())
    
    ggplot(heatmap_data, aes(x = year, y = stars, fill = count)) +
      geom_tile() +
      scale_fill_viridis_c() +
      labs(title = paste("Heatmap of Reviews Over Time for", input$business),
           x = "Year",
           y = "Star Rating",
           fill = "Number of Reviews") +
      theme_minimal()
  })
  
  
  
  output$postalcodeplot <- renderPlot({
    business %>%
      group_by(postal_code) %>%
      summarise(mean_stars = mean(stars, na.rm = TRUE)) %>%
      arrange(desc(mean_stars)) %>%
      ggplot(aes(x = mean_stars, y = reorder(postal_code, mean_stars))) +
      geom_bar(stat = "identity", fill = "skyblue") +
      labs(title = "Average Star Rating by Postal Code",
           x = "Average Star Rating",
           y = "Postal Code") +
      theme_minimal()
  })
  output$drinks <-renderTable({
    drinksdf
  })
  business_details <- reactive({
    selected_business_info <- sentiment_scores_business_id[sentiment_scores_business_id$business_id == unique_business_ids[business$name == input$business], ]
    selected_business_info
  })
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
      geom_bar(aes(fill="tomato3"), data=business_scores, stat = 'identity') +
      geom_bar(aes(fill='steelblue3'), data=postal_code_score_avgs, stat = 'identity') +
      labs(title = "Sentiment Analysis by Aspect Across Business and Postal Code",
           y = "Sentiment Score") +
      theme_classic()+
      theme(legend.position = "top") +
      scale_fill_manual(values = c("tomato3", "steelblue3"), 
                        name = "Legend",
                        labels = c("Selected Business Scores", "Selected Postal Code Scores"))
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)


#some of the the above code is from ChatGPT 3.5. Accessed the week of 11/27.
