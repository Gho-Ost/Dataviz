install.packages("ggridges")
library(ggridges)

sets <- fread("rebrickable/sets.csv.gz")
themes <- fread("rebrickable/themes.csv.gz")

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

# rainbow density ## good?
set_themes_children %>% head(1000) %>%
  ggplot(aes(x=year, y=as.character(parent_name), fill=as.character(parent_name))) +
  geom_density_ridges(alpha=0.6, bandwidth=1.7) +
  theme_ridges() +
  ylab(label = "theme") +
  theme(legend.position = "none")

# repeat colors density ##  also good
gg_color_hue <- function(n) {
  hues = seq(15, 375, length = n + 1)
  hcl(h = hues, l = 65, c = 100)[1:n]
}
n = 5
my_cols = gg_color_hue(n)

set_themes_children %>% 
  ggplot(aes(x=year, y=as.character(parent_name), fill=as.character(parent_name))) +
  geom_density_ridges(alpha=0.6, bandwidth=1.7) +
  theme_ridges() +
  ylab(label = "theme") +
  theme(legend.position = "none") +
  scale_fill_manual(values = rep(my_cols, length.out=nrow(set_themes_children)))


# rainbow bins ## poor visibility
set_themes_children %>% head(1000) %>%
  ggplot(aes(x=year, y=as.character(parent_name), fill=as.character(parent_name))) +
  geom_density_ridges(alpha=0.6, stat="binline", bins=20) +
  theme_ridges() +
  ylab(label = "theme") +
  theme(legend.position = "none")

# color relative to year ## heavy (opacity makes it much heavier :/ )
#install.packages("hrbrthemes")
#library(hrbrthemes)
install.packages("viridis")
library(viridis)

set_themes_children %>% head(1000)
  ggplot(aes(x=year, y=as.character(parent_name), fill=after_stat(x))) +
  geom_density_ridges_gradient(bandwidth = 1.7, scale = 4, rel_min_height = 0.01) +
  scale_fill_viridis() +
  theme_ridges() +
  ylab(label = "theme") +
  theme(legend.position = "none")



##########################
# Only parent themes
set_themes <- rename(sets, set_name=name) %>% merge(rename(themes, theme_name=name), 
                                                    by.x="theme_id", by.y="id") %>% 
  filter(is.na(parent_id))

#set_themes %>% filter(is.na(parent_id)) %>% distinct(theme_name)

# Ridge year to theme(only parents) amount distribution
set_themes %>% ggplot(aes(x=year, y=theme_name)) +
  geom_density_ridges(alpha=0.6, stat="binline", bins=20) +
  theme_ridges()

