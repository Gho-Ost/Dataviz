sets <- read.csv("C:\\Users\\Komputer\\OneDrive\\Pulpit\\rebrickable\\sets.csv", header=T)
themes <- read.csv("C:\\Users\\Komputer\\OneDrive\\Pulpit\\rebrickable\\themes.csv", header=T)
inventories <- read.csv("C:\\Users\\Komputer\\OneDrive\\Pulpit\\rebrickable\\inventories.csv", header=T)
inventory_parts <-read.csv("C:\\Users\\Komputer\\OneDrive\\Pulpit\\rebrickable\\inventory_parts.csv", header=T)
parts <- read.csv("C:\\Users\\Komputer\\OneDrive\\Pulpit\\rebrickable\\parts.csv", header=T)
part_categories <-read.csv("C:\\Users\\Komputer\\OneDrive\\Pulpit\\rebrickable\\part_categories.csv", header=T)

merged_df <- merge(sets, themes, by.x = "theme_id", by.y = "id", all.x = TRUE)
merged_df_inventories <- merge(inventories, merged_df, by = "set_num", all.x = TRUE)
inventory_parts <- inventory_parts %>% rename(id = inventory_id)
part_categories <- inventory_parts %>% rename(part_cat_id = id)

merged_df_inventory_parts <- merge(merged_df_inventories, inventory_parts, by="id")

merged_df_inventory_parts_final <- merge(merged_df_inventory_parts, parts, by = "part_num")

parts_count <- merged_df_inventory_parts_final %>% count(part_num, name = "count")
merged_counts <- merge(merged_df_inventory_parts_final, parts_count, by = "part_num", all.x=T)
result <- merged_counts %>%
  filter(count == 1)

# Plots
result<-result %>%
  count(name.y)
result<-na.omit(result)
result<-arrange(result, desc(n))
result<-head(result,20)

# Plot 1 A->including the Database Sets Theme
ggplot(result, aes(x = n, y = reorder(name.y, n))) +
  geom_col(fill = "lightblue") +
  labs(title = "20 Themes with the Highest Amount of Unique LEGO Parts",
       x = "Number of Unique Parts",
       y = "Theme")

#Creating alternative visualization without Database Sets
result_2 <- merged_counts %>%
  filter(count == 1)
result_2<-result_2 %>%
  count(name.y)
result_2<-na.omit(result_2)
result_2<-arrange(result_2, desc(n))
result_2 <- filter(result_2, name.y != "Database Sets")
result_2<-head(result_2,20)

#Plot 1 B-> Excluding Database Sets
ggplot(result_2, aes(x = n, y = reorder(name.y, n))) +
  geom_col(fill = "lightblue") +
  labs(title = "20 Themes with the Highest Amount of Unique LEGO Parts",
       x = "Number of Unique Parts",
       y = "Theme")

################################################################################

# plot of number of unique pieces through the years
result <- merged_counts %>%
  filter(count == 1)
result<-result %>%
  count(year)
result<-na.omit(result)

#Plot 2
ggplot(result, aes(x = year, y = n)) +
  geom_line() +
  labs(x = "Year", y = "Number of Unique Lego Parts") +
  ggtitle("Number of Unique Lego Blocks per Year")

result_cumsum <- result %>%
  arrange(year) %>%
  mutate(n_cumsum = cumsum(n))

#Plot 3
ggplot(result_cumsum, aes(x = year, y = n_cumsum)) +
  geom_line() +
  labs(x = "Year", y = "Number of Rare Lego Parts") +
  ggtitle("Cumulative Sum of Rare Lego Parts through Years")

# Last plot -> scatterplot 
result <- merged_counts
theme_counts <- result %>%
  group_by(name.y) %>%
  summarize(num_parts = mean(num_parts), count = sum(count)) %>%
  arrange(desc(count))

theme_counts<-head(arrange(theme_counts,desc(count)),100)
theme_counts <- theme_counts %>% rename('Average Number of Parts' = num_parts) %>%
  rename('Number of Rare Lego Parts'=count)

#Plot 4 ->Interactive
ggplotly(ggplot(theme_counts, aes(x = theme_counts$'Average Number of Parts',
                                         y = theme_counts$'Number of Rare Lego Parts',
                                         text = name.y)) +
           geom_point() +
           labs(x = "Average Number of Parts", y = "Number of Rare Lego Parts") +
           ggtitle("Number of Rare Lego Parts vs Average Number of Parts for a Theme"))

