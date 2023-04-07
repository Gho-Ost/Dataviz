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

# Uncomment below, maybe
#merged_result <- merge(merged_df_inventory_parts_final, part_categories, by="part_cat_id")

# Oh my Fucking God how did my computer not die 

parts_count <- merged_df_inventory_parts_final %>% count(part_num, name = "count")
merged_counts <- merge(merged_df_inventory_parts_final, parts_count, by = "part_num", all.x=T)
result <- merged_counts %>%
  filter(count == 1)

# I can finally plot oh boy
result<-result %>%
  count(name.y)
result<-na.omit(result)
result<-arrange(result, desc(n))
result<-head(result,20)

# Possibly drop database sets

ggplot(result, aes(x = n, y = reorder(name.y, n))) +
  geom_col(fill = "lightblue") +
  labs(title = "20 Themes with the Highest Amount of Rare LEGO Blocks",
       x = "Number of Rare Blocks",
       y = "Theme")

# plot of number of unique pieces through the years
result <- merged_counts %>%
  filter(count == 1)
result<-result %>%
  count(year)
result<-na.omit(result)


ggplot(result, aes(x = year, y = n)) +
  geom_line() +
  labs(x = "Year", y = "Number of rare Lego blocks")

result_cumsum <- result %>%
  arrange(year) %>%
  mutate(n_cumsum = cumsum(n))

ggplot(result_cumsum, aes(x = year, y = n_cumsum)) +
  geom_line() +
  labs(x = "Year", y = "Number of rare Lego blocks")

# Last plot -> scatterplot 
result <- merged_counts
theme_counts <- result %>%
  group_by(name.y) %>%
  summarize(num_parts = mean(num_parts), count = sum(count)) %>%
  arrange(desc(count))

theme_counts<-head(arrange(theme_counts,desc(count)),100)

ggplotly(ggplot(theme_counts, aes(x = num_parts, y = count, text = name.y)) +
           geom_point() +
           labs(x = "Average Number of Parts", y = "Sum of Counts"))

