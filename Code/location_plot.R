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

# Group by 'business_id' and calculate the total stars for each business
stars_aggregated <- reviews_df %>%
  group_by(business_id) %>%
  summarize(total_stars = mean(stars, na.rm = TRUE))

# Display the aggregated stars for each business_id
print(head(stars_aggregated))

#print(paste("Number of rows before filtering:", nrow(reviews_df)))
#print(paste("Number of rows after filtering:", nrow(filtered_reviews)))
reviews_df <- filtered_reviews

# Calculate average ratings for each postal code
avg_ratings <- business_df %>%
  group_by(postal_code) %>%
  summarise(AvgRating = mean(stars, na.rm = TRUE)) %>%
  arrange(desc(AvgRating))


unique_ratings <- unique(avg_ratings$AvgRating)
colors <- rainbow(length(unique_ratings))  # Using viridis color palette

rating_color_map <- setNames(colors, unique_ratings)

avg_ratings$RatingColor <- rating_color_map[avg_ratings$AvgRating]


ggplot(avg_ratings, aes(x = reorder(postal_code, AvgRating), y = AvgRating, fill = RatingColor)) +
  geom_bar(stat = "identity", width = 0.8) +
  scale_fill_identity() +
  labs(title = "Average Ratings by Postal Code",
       x = "Postal Code",
       y = "Average Rating") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1)) +
  coord_flip()



# Calculate average ratings and number of reviews for each postal code
agg_data <- business_df %>%
  group_by(postal_code) %>%
  summarise(AvgRating = mean(stars, na.rm = TRUE),
            NumReviews = n()) %>%
  arrange(desc(AvgRating))

unique_ratings <- unique(agg_data$AvgRating)
colors <- rainbow(length(unique_ratings))

# Map each unique rating to a color in a dictionary
rating_color_map <- setNames(colors, unique_ratings)

# Assign colors based on AvgRating
agg_data$RatingColor <- rating_color_map[agg_data$AvgRating]

ggplot(agg_data, aes(x = reorder(postal_code, AvgRating), y = AvgRating, fill = RatingColor, size = NumReviews)) +
  geom_bar(stat = "identity", width = 0.8) +
  scale_fill_identity() +
  labs(title = "Average Ratings by Postal Code",
       x = "Postal Code",
       y = "Average Rating",
       size = "Number of Reviews") +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 0, vjust = 0.5, hjust = 1)) +
  scale_size_continuous(name = "Number of Reviews", labels = scales::comma) + # Legend for size
  guides(fill = guide_legend(title = "Average Rating")) + # Legend for color
  coord_flip()
