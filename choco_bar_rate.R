# load packages
library(ggplot2)
library(dplyr)
library(readxl)
library(janitor)

# read data
df_cocoa = read_excel("cocoa_data.xlsx")
df_cocoa = clean_names(df_cocoa)
head(df_cocoa)

# check for duplicates
if (count(df_cocoa) == count(unique(df_cocoa))) {
  print("There are no duplicate values") 
} else {
  print("There are duplicates")
}

# check for missing values in ratings
sum(is.na(df_cocoa$rating) == TRUE)

# best cocoa beans grown df
best_cocoa_grown <- df_cocoa %>% 
  select(broad_bean_origin, rating) %>% 
  group_by(broad_bean_origin) %>% 
  summarise(average_rating = mean(rating))

head(best_cocoa_grown)

cocoa_count <- c(
  sum(best_cocoa_grown$average_rating < 6 & best_cocoa_grown$average_rating >= 5),
  sum(best_cocoa_grown$average_rating < 5 & best_cocoa_grown$average_rating >= 4),
  sum(best_cocoa_grown$average_rating < 4 & best_cocoa_grown$average_rating >= 3),
  sum(best_cocoa_grown$average_rating < 3 & best_cocoa_grown$average_rating >= 2),
  sum(best_cocoa_grown$average_rating < 2 & best_cocoa_grown$average_rating >= 1)
)

df_cocoa_count <- data.frame(
  Ratings = c("5", "4", "3", "2", "1"),
  Counts = cocoa_count
)

# visualize best cocoa beans grown
ggplot(df_cocoa_count, aes(Ratings, Counts)) + geom_bar(stat = "identity", fill = "blue") + 
  geom_text(aes(label = Counts), nudge_y = 2) +
  labs(title = "Counts of each Ratings")

best_cocoa_grown %>% 
  filter(average_rating >= 4)

# highest-rated bars df
highest_rated_bar <- df_cocoa %>% 
  select(company_location, rating) %>% 
  group_by(company_location) %>% 
  summarise(highest_rated = max(rating))

head(highest_rated_bar)

highest_count <- c(
  sum(highest_rated_bar$highest_rated < 6 & highest_rated_bar$highest_rated >= 5),
  sum(highest_rated_bar$highest_rated < 5 & highest_rated_bar$highest_rated >= 4),
  sum(highest_rated_bar$highest_rated < 4 & highest_rated_bar$highest_rated >= 3),
  sum(highest_rated_bar$highest_rated < 3 & highest_rated_bar$highest_rated >= 2),
  sum(highest_rated_bar$highest_rated < 2 & highest_rated_bar$highest_rated >= 1)
)

df_highest_count <- data.frame(
  Ratings = c("5", "4", "3", "2", "1"),
  Counts = highest_count
)

# visualize highest-rated bars
ggplot(df_highest_count, aes(Ratings, Counts)) + geom_bar(stat = "identity", fill = "blue") + 
  geom_text(aes(label = Counts), nudge_y = 2) +
  labs(title = "Counts of each Ratings")

filter(highest_rated_bar, highest_rated >= 5)

df_cocoa %>% 
  select(company_location, rating, broad_bean_origin) %>% 
  filter(company_location == "Italy", rating == 5)

best_cocoa_grown %>% 
  filter(average_rating >= 4)

# relationship between cocoa percentage and rating df
corr_cocoa_rating <- df_cocoa %>% 
  select(cocoa_percent, rating)

head(corr_cocoa_rating)

# correlation between cocoa percentage and rating visualization
corr_r <- cor(corr_cocoa_rating$cocoa_percent, corr_cocoa_rating$rating)

ggplot(corr_cocoa_rating, aes(x = cocoa_percent, y = rating)) + 
  geom_point() + 
  geom_smooth(formula = y ~ x, method = lm, se = FALSE) +
  annotate("text", x = 0.5, y = 4.5, label = paste("R = ", format(round(corr_r, 2), nsmall = 2))) +
  labs(title = "Correlation of Cocoa Percentage and Ratings")
