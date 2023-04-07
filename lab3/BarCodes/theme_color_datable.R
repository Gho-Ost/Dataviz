install.packages("DT")
library(DT)

colors <- fread("rebrickable/colors.csv.gz")
inventory_parts <- fread("rebrickable/inventory_parts.csv.gz")
inventories <- fread("rebrickable/inventories.csv.gz")
sets <- fread("rebrickable/sets.csv.gz")
themes <- fread("rebrickable/themes.csv.gz")

set_themes <- sets %>% 
  rename(set_name=name) %>%
  merge(rename(themes, theme_name=name), by.x = "theme_id", by.y = "id")

set_theme_inventories <- inventories %>% merge(set_themes, by="set_num")
set_theme_inventories_parts <- inventory_parts %>% merge(set_theme_inventories, by.x="inventory_id", by.y="id")
set_theme_inventories_parts_colors <- colors %>% rename(color_name=name) %>% 
  merge(set_theme_inventories_parts, by.x="id", by.y="color_id") %>% rename(color_id=id)


# Datable (no children themes?)
theme_colors <- set_theme_inventories_parts_colors %>% filter(is.na(parent_id)) %>%
  select(one_of(c("color_name","rgb","theme_name","theme_id","color_id")))

grouped_theme_colors <- theme_colors %>% group_by(theme_name, color_name, color_id, rgb) %>%
  summarise(count=n(), .groups="keep") %>% arrange(theme_name, desc(count))


prettyTable <- function(table_df, round_columns_func=is.numeric, round_digits=0) {
  DT::datatable(table_df, style="bootstrap", filter = "top", rownames = FALSE, extensions = "Buttons", 
                options = list(dom = 'Bfrtip', buttons = c('copy', 'csv', 'excel', 'pdf', 'print'))) %>%
    formatRound(unlist(lapply(table_df, round_columns_func)), round_digits)
}

prettyTable(grouped_theme_colors)








