sets <- read.csv("C:\\Users\\Komputer\\OneDrive\\Pulpit\\rebrickable\\sets.csv", header=T)
themes <- read.csv("C:\\Users\\Komputer\\OneDrive\\Pulpit\\rebrickable\\themes.csv", header=T)

# Join the themes dataframe to the sets dataframe based on the theme_id column
merged_df <- merge(sets, themes, by.x = "theme_id", by.y = "id", all.x = TRUE)

avg_parts_per_year <- merged_df %>%
  group_by(year) %>%
  summarize(avg_parts = mean(num_parts))

#First Plot
ggplot(data = avg_parts_per_year, aes(x = year, y = avg_parts)) +
  geom_line(size = 1) +
  labs(x = "Year", y = "Average Number of Parts per Set") +
  ggtitle("Average Number of Parts per Set per Year")

# Create a data frame with the average number of parts per theme
theme_part_counts <- merged_df %>%
  group_by(name.y) %>%
  summarize(avg_parts = mean(num_parts))

# Append the maximum number of parts found in a set with a given theme
theme_part_counts <- theme_part_counts %>%
  left_join(merged_df %>%
              group_by(name.y) %>%
              summarize(max_parts = max(num_parts)),
            by = "name.y")

top_20_avg_parts <- head(theme_part_counts %>% arrange(desc(avg_parts)),20)
top_20_max_parts <- head(theme_part_counts %>% arrange(desc(max_parts)),20)

# Second Plot
# Version A with Dots
ggplot(top_20_avg_parts, aes(x = reorder(name.y, avg_parts), y = avg_parts)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Theme", y = "Average Number of Parts") +
  geom_point(aes(reorder(name.y, avg_parts),y=max_parts)) +
  ggtitle("Average and Maximum Number of Parts in Themes for Chosen Themes")
# Version B without Dots
ggplot(top_20_avg_parts, aes(x = reorder(name.y, avg_parts), y = avg_parts)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Theme", y = "Average Number of Parts in a Set") +
  ggtitle("Average Number of Parts in Themes for Chosen Themes")


#Third Plot with Dots
# Version B with Dots
ggplot(top_20_max_parts, aes(x = reorder(name.y, avg_parts), y = avg_parts)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Theme", y = "Number of Parts in a Set") +
  geom_point(aes(reorder(name.y, avg_parts),y=max_parts)) +
  ggtitle("Average and Maximum Number of Parts in Themes for Chosen Themes")
# Version B without Dots
ggplot(top_20_max_parts, aes(x = reorder(name.y, avg_parts), y = avg_parts)) +
  geom_bar(stat = "identity") +
  coord_flip() +
  labs(x = "Theme", y = "Number of Parts in a Set") +
  ggtitle("Average Number of Parts in Themes for Chosen Themes")
