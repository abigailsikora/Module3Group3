# Load required libraries
library(jsonlite)
library(dplyr)
library(tidytext)
library(tm)
library(textdata)
library(tidyr)
library(ggplot2)

# Load the filtered reviews data
reviews <- fromJSON("filtered_reviews.json")
business_data <- fromJSON("nashville_bars_nightlife.json", flatten = TRUE)

# Convert reviews to a data frame
reviews_df <- as.data.frame(reviews)
business_df <- as.data.frame(business_data)
#print(names(reviews_df))

# Group by business_id and filter out those with less than 10 reviews
filtered_reviews <- reviews_df %>%
  group_by(business_id) %>%
  filter(n() >= 10)


# Preprocess text
corpus <- Corpus(VectorSource(reviews_df$text))
corpus <- tm_map(corpus, content_transformer(tolower)) %>%
  tm_map(removePunctuation) %>%
  tm_map(removeNumbers) %>%
  tm_map(removeWords, stopwords("en")) %>%
  tm_map(stripWhitespace)



# Aspects to analyze sentiment for
aspects <- c("service", "ambiance", "food", "drinks", "time", "bar", "vibe", "music", "crowd", "employees", "price")

# Function to perform aspect-based sentiment analysis
perform_aspect_sentiment_analysis <- function(reviews_data, aspect) {
  # Filter reviews mentioning the current aspect
  aspect_reviews <- reviews_data %>%
    filter(grepl(aspect, text, ignore.case = TRUE))
  
  # Tokenize the text and perform sentiment analysis
  aspect_reviews_tokens <- aspect_reviews %>%
    unnest_tokens(output = word, input = text) %>%
    inner_join(get_sentiments("afinn"), by = "word") %>%
    mutate(aspect = aspect) %>%  # Assign the aspect to each sentiment score
    group_by(business_id, aspect) %>%
    summarize(sentiment_score = mean(value, na.rm = TRUE))
  
  return(aspect_reviews_tokens)
}

# Initialize an empty list to store aspect-based sentiment analysis results
aspect_sentiments <- list()

# Iterate through aspects for sentiment analysis
for (aspect in aspects) {
  aspect_sentiments[[aspect]] <- perform_aspect_sentiment_analysis(reviews_df, aspect)
}

# Combine aspect-based sentiment analysis results into a single dataframe
aspect_sentiments_combined <- bind_rows(aspect_sentiments)
#print(head(aspect_sentiments_combined))

# Aggregate sentiment scores by business_id and aspect
aggregated_sentiments <- aspect_sentiments_combined %>%
  group_by(business_id, aspect) %>%
  summarize(avg_sentiment_score = mean(sentiment_score, na.rm = TRUE))

min_max_normalize <- function(scores) {
  min_score <- min(scores, na.rm = TRUE)
  max_score <- max(scores, na.rm = TRUE)
  normalized_scores <- (scores - min_score) / (max_score - min_score) * 2 - 1
  return(normalized_scores)
}

# Normalize avg_sentiment_score within aggregated_sentiments dataframe using min-max normalization
aggregated_sentiments$avg_sentiment_score <- min_max_normalize(aggregated_sentiments$avg_sentiment_score)

# Ensure the values strictly fall between -1 and +1
aggregated_sentiments$avg_sentiment_score <- pmax(-1, pmin(aggregated_sentiments$avg_sentiment_score, 1))

#print(head(aggregated_sentiments))

# Pivot the data to have aspects as columns
pivot_result <- aggregated_sentiments %>%
  pivot_wider(names_from = aspect, values_from = avg_sentiment_score, values_fill = NA)

# Display the aggregated sentiment scores by business_id with aspects as columns
print(pivot_result)
pivot_result <- pivot_result %>%
  left_join(select(business_df, business_id, postal_code), by = "business_id")

pivot_result <- pivot_result %>%
  select(postal_code, everything())

# View the updated pivot_result with postal_code
print(pivot_result)

# Replace NA values with 0 in the pivot_result dataframe
pivot_result[is.na(pivot_result)] <- 0

# Write the pivot_result dataframe to a CSV file
write.csv(pivot_result, file = "sentiment_scores_by_business.csv", row.names = FALSE)

sentiment_by_postal_code <- pivot_result %>%
  group_by(postal_code) %>%
  summarize(across(-business_id, ~mean(., na.rm = TRUE)))

print(sentiment_by_postal_code)

write.csv(sentiment_by_postal_code, file = "sentiment_by_postal_code.csv", row.names = FALSE)

# Merge the 'pivot_result' and 'stars_aggregated' dataframes based on 'business_id'
merged_data <- merge(pivot_result, stars_aggregated, by = "business_id", all.x = TRUE)

# Select the columns of interest (excluding postal_code and business_id)
data_for_correlation <- merged_data[, -c(1, 2)]  # Exclude postal_code and business_id columns

# Calculate the correlation matrix between total_stars and sentiment scores
#correlation_matrix <- cor(data_for_correlation)

#print(correlation_matrix)


# Perform linear regression for 'service' sentiment score and 'total_stars'
lm_model <- lm(total_stars ~ service + ambiance + food + drinks + music + price + time + vibe + employees , data = merged_data)

# Summary of the linear regression model
summary(lm_model)

coefficients <- c("service" = 0.81356, "ambiance" = -0.12941, "food" = 0.51394, 
                  "drinks" = 0.10822, "music" = 0.09064, "price" = 0.12596, 
                  "time" = 1.61301, "vibe" = 0.15209, "employees" = 0.11890)

p_values <- c("service" = "< 0.001", "ambiance" = "0.0504", "food" = "< 0.001", 
              "drinks" = "0.2104", "music" = "0.2534", "price" = "0.1604", 
              "time" = "< 0.001", "vibe" = "0.0383", "employees" = "0.0733")

# Create a data frame for plotting
data_plot <- data.frame(aspect = names(coefficients), 
                        coefficient = as.numeric(coefficients),
                        p_value = factor(p_values, levels = c("< 0.001", "0.001", "0.01", "0.05", "0.1", "1")))

# Assigning labels for significance levels
significance_labels <- c("< 0.001" = "***", "0.001" = "**", "0.01" = "*", "0.05" = ".", "0.1" = "", "1" = "")

# Plotting the coefficients and their significance
ggplot(data_plot, aes(x = aspect, y = coefficient, fill = p_value)) +
  geom_bar(stat = "identity", position = "dodge", color = "black") +
  geom_text(aes(label = significance_labels[p_value]),
            vjust = -0.5, size = 3) +
  labs(title = "Impact of Different Aspects on Star Ratings",
       x = "Aspect", y = "Coefficient") +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 0, hjust = 1)) +
  scale_fill_manual(values = c("#FF0000", "#FFA500", "#FFFF00", "#00FF00", "#0000FF", "#FFFFFF"),
                    labels = c("***", "**", "*", ".", "", "N.S."), 
                    guide = guide_legend(title = "Significance")) +
  geom_hline(yintercept = 0, linetype = "dashed", color = "black")
