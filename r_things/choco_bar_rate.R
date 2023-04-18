# Load necessary packages
library(ggplot2)  # For data visualization
library(dplyr)    # For data manipulation
library(readxl)   # For reading excel files
library(janitor)  # For cleaning column names

# Read in the cocoa data from an excel file
df_cocoa <- read_excel("cocoa_data.xlsx")

# Clean column names using the clean_names function from the janitor package
df_cocoa <- clean_names(df_cocoa)

# View the first few rows of the dataset
head(df_cocoa)

# Check for duplicates
if (count(df_cocoa) == count(unique(df_cocoa))) {
  print("There are no duplicate values") 
} else {
  print("There are duplicates")
}

# Check for missing values in the "rating" column
sum(is.na(df_cocoa$rating) == TRUE)

# Identify the best cocoa beans grown by calculating the mean rating for each broad_bean_origin
best_cocoa_grown <- df_cocoa %>% 
  select(broad_bean_origin, rating) %>% 
  group_by(broad_bean_origin) %>% 
  summarise(average_rating = mean(rating))

# View the first few rows of the best_cocoa_grown dataset
head(best_cocoa_grown)

# Count the number of cocoa beans with a certain rating
cocoa_count <- c(
  sum(best_cocoa_grown$average_rating < 6 & best_cocoa_grown$average_rating >= 5),
  sum(best_cocoa_grown$average_rating < 5 & best_cocoa_grown$average_rating >= 4),
  sum(best_cocoa_grown$average_rating < 4 & best_cocoa_grown$average_rating >= 3),
  sum(best_cocoa_grown$average_rating < 3 & best_cocoa_grown$average_rating >= 2),
  sum(best_cocoa_grown$average_rating < 2 & best_cocoa_grown$average_rating >= 1)
)

# Create a data frame with the counts for each rating
df_cocoa_count <- data.frame(
  Ratings = c("5", "4", "3", "2", "1"),
  Counts = cocoa_count
)

# Visualize the number of cocoa beans with each rating using a bar plot
ggplot(df_cocoa_count, aes(Ratings, Counts)) + 
  geom_bar(stat = "identity", fill = "blue") + 
  geom_text(aes(label = Counts), nudge_y = 2) +
  labs(title = "Counts of each Rating")

# Filter cocoa beans with an average rating of 4 or higher
best_cocoa_grown <- best_cocoa_grown %>% 
  filter(average_rating >= 4)

# Create a data frame of the highest rated chocolate bars by company location
highest_rated_bar <- df_cocoa %>% 
  select(company_location, rating) %>% 
  group_by(company_location) %>% 
  summarise(highest_rated = max(rating))

# Display the first few rows of the highest_rated_bar data frame
head(highest_rated_bar)

# Create a vector of counts for each rating category
highest_count <- c(
  sum(highest_rated_bar$highest_rated < 6 & highest_rated_bar$highest_rated >= 5),
  sum(highest_rated_bar$highest_rated < 5 & highest_rated_bar$highest_rated >= 4),
  sum(highest_rated_bar$highest_rated < 4 & highest_rated_bar$highest_rated >= 3),
  sum(highest_rated_bar$highest_rated < 3 & highest_rated_bar$highest_rated >= 2),
  sum(highest_rated_bar$highest_rated < 2 & highest_rated_bar$highest_rated >= 1)
)

# Create a data frame of the highest rated bars count for each rating category
df_highest_count <- data.frame(
  Ratings = c("5", "4", "3", "2", "1"),
  Counts = highest_count
)

# Create a bar chart of the highest rated bars count for each rating category
ggplot(df_highest_count, aes(Ratings, Counts)) + 
  geom_bar(stat = "identity", fill = "blue") + 
  geom_text(aes(label = Counts), nudge_y = 2) +
  labs(title = "Counts of each Ratings")

# Filter the highest_rated_bar data frame to display only bars with a rating of 5 or higher
filter(highest_rated_bar, highest_rated >= 5)

# Filter the df_cocoa data frame to display only bars with a rating of 5 and a company location of Italy
df_cocoa %>% 
  select(company_location, rating, broad_bean_origin) %>% 
  filter(company_location == "Italy", rating == 5)

# Filter cocoa beans with an average rating of 4 or higher
best_cocoa_grown %>% 
  filter(average_rating >= 4)

# Create a data frame of cocoa percentage and rating
corr_cocoa_rating <- df_cocoa %>% 
  select(cocoa_percent, rating)

# Display the first few rows of the corr_cocoa_rating data frame
head(corr_cocoa_rating)

# Calculate the correlation coefficient between cocoa percentage and rating
corr_r <- cor(corr_cocoa_rating$cocoa_percent, corr_cocoa_rating$rating)

# Create a scatter plot of cocoa percentage and rating with a trendline and correlation coefficient label
ggplot(corr_cocoa_rating, aes(x = cocoa_percent, y = rating)) + 
  geom_point() + 
  geom_smooth(formula = y ~ x, method = lm, se = FALSE) +
  annotate("text", x = 0.5, y = 4.5, label = paste("R = ", format(round(corr_r, 2), nsmall = 2))) +
  labs(title = "Correlation of Cocoa Percentage and Ratings")
