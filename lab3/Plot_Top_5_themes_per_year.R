sets <- read.csv("C:\\Users\\Komputer\\OneDrive\\Pulpit\\rebrickable\\sets.csv", header=T)
themes <- read.csv("C:\\Users\\Komputer\\OneDrive\\Pulpit\\rebrickable\\themes.csv", header=T)

# Join the themes dataframe to the sets dataframe based on the theme_id column
merged_df <- merge(sets, themes, by.x = "theme_id", by.y = "id", all.x = TRUE)
library(dplyr)

sets_per_theme_per_year <- merged_df %>%
  group_by(year, name.y) %>%
  summarize(num_sets = n()) %>%
  ungroup()

merged_df_popularity <- sets_per_theme_per_year %>%
  group_by(year) %>%
  mutate(popularity = dense_rank(desc(num_sets)))

merged_df_popularity<- subset(merged_df_popularity, popularity <= 5)


###############################################################################







library(dplyr)

# Group by year and popularity ranking and count the number of occurrences
popularity_counts <- merged_df_popularity %>%
  group_by(year, popularity) %>%
  summarise(count = n()) %>%
  filter(count > 1)

# Joining the data.frames
result_df <- popularity_counts %>%
  left_join(merged_df_popularity, by = c("year", "popularity")) %>%
  dplyr::select(year, popularity, name.y, count) %>%
  arrange(year, popularity, name.y, desc(count))


# Select the first row in result_df
first_row <- head(result_df, 1)
# Increase the value of popularity parameter by one
new_popularity <- first_row$popularity + 1
# Update the popularity ranking in merged_df_popularity
merged_df_popularity <- merged_df_popularity %>%
  mutate(popularity = ifelse(year == first_row$year & name.y == first_row$name.y,
                             new_popularity, popularity))










# Putting it in a loop 
while (nrow(popularity_counts) > 0) {
  popularity_counts <- merged_df_popularity %>%
    group_by(year, popularity) %>%
    summarise(count = n()) %>%
    filter(count > 1)
  
  if (nrow(popularity_counts)==0){
    print("finished")
    break
  }
  
  result_df <- popularity_counts %>%
    left_join(merged_df_popularity, by = c("year", "popularity")) %>%
    dplyr::select(year, popularity, name.y, count) %>%
    arrange(year, popularity, name.y, desc(count))
  
  # Select the first row in result_df
  first_row <- head(result_df, 1)
  # Increase the value of popularity parameter by one
  new_popularity <- first_row$popularity + 1
  # Update the popularity ranking in merged_df_popularity
  merged_df_popularity <- merged_df_popularity %>%
    mutate(popularity = ifelse(year == first_row$year & name.y == first_row$name.y,
                               new_popularity, popularity))
}



# Deleting Data with popularity lower than 5

merged_df_popularity<- subset(merged_df_popularity, popularity <= 5)

# Oh my Fucking God I can finally plot this bitch

ggplot(head(merged_df_popularity,10), aes(x = year, y = popularity, color = name.y)) +
  geom_line() +
  labs(title = "Popularity of Themes Over Time", 
       x = "Year", y = "Popularity Ranking")

# NEvermind I fucking cannot, let's go again
selected_years <- c(1950, 1960, 1970, 1980, 1990, 2000, 2010, 2020)

popular_themes <- merged_df %>%
  count(name.y, sort = TRUE) %>%
  head(5)

popular_themes <- popular_themes$name.y[1:5] # use the top 10 popular themes
merged_df_filtered <- merged_df[merged_df$name.y %in% popular_themes, ]

theme_year_counts <- merged_df_filtered %>%
  group_by(name.y, year) %>%
  summarise(num_sets = n()) %>%
  ungroup()

p<-ggplot(data = theme_year_counts, aes(x = year, y = num_sets, color = name.y)) +
  geom_line(size = 1) +
  scale_x_continuous(breaks = selected_years) +
  labs(x = "Year", y = "Number of sets", color = "Theme")


p <- ggplotly(p, tooltip = c("name.y", "num_sets"))
p <- style(p, hoveron = "plot")
p



# Let's go one more thingy
theme_year_counts_cumsum <- theme_year_counts %>%
  group_by(name.y) %>%
  mutate(cum_sum = cumsum(num_sets))


ggplot(data = theme_year_counts_cumsum, aes(x = year, y = cum_sum, color = name.y)) +
  geom_line(size = 1) +
  scale_x_continuous(breaks = selected_years) +
  labs(x = "Year", y = "Number of sets", color = "Theme")



### You can also correlate the growth of company with number of produced sets and 
# correlate number of sets with release of star wars films