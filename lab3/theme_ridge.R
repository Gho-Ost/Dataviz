install.packages("ggridges")
library(ggridges)

sets <- fread("rebrickable/sets.csv.gz")
themes <- fread("rebrickable/themes.csv.gz")

# Only parent themes
set_themes <- rename(sets, set_name=name) %>% merge(rename(themes, theme_name=name), 
                                                    by.x="theme_id", by.y="id") %>% 
  filter(is.na(parent_id))

#set_themes %>% filter(is.na(parent_id)) %>% distinct(theme_name)

# Ridge year to theme(only parents) amount distribution
set_themes %>% ggplot(aes(x=year, y=theme_name)) +
  geom_density_ridges(alpha=0.6, stat="binline", bins=20) +
  theme_ridges()

# Ridge year to theme(parents+children) amount distribution
parents <- themes %>% filter(is.na(parent_id)) %>% group_by(id, name)

find_parent <- function(parent_id) {
  if (is.na(parent_id)) {
    "parent"
  } else {
    prev_id <- parent_id
    while(!is.na(parent_id)){
      prev_id <- parent_id
      parent_id <- themes$parent_id[themes$id==parent_id]
    }
    parents$name[parents$id==prev_id]
  }
}

set_themes_children <- rename(sets, set_name=name) %>% merge(rename(themes, theme_name=name), 
                                                    by.x="theme_id", by.y="id") 

set_themes_children$parent_name <- lapply(set_themes_children$parent_id, find_parent)
set_themes_children <- set_themes_children %>% 
  mutate(parent_name = ifelse(parent_name=="parent", theme_name, parent_name))


set_themes_children %>% 
  ggplot(aes(x=year, y=as.character(parent_name))) +
  geom_density_ridges(alpha=0.6, stat="binline", bins=20) +
  theme_ridges()




