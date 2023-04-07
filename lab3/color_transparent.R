colors <- fread("rebrickable/colors.csv.gz")

trans_count <- colors %>% group_by(is_trans) %>% summarise(count = n()) %>% 
  mutate(
    proportions = paste(round(count/sum(count),2)*100, "%", sep=''),
    is_transparent = ifelse(is_trans == 'f', "No", "Yes"))

trans_count %>% 
  ggplot(aes(x="", y=count, fill=is_transparent, label=proportions)) + 
  geom_bar(stat="identity", width=1) +
  coord_polar("y", start = 0) + 
  ggtitle("Proportion of transparent colors") +
  theme_void() +
  geom_text(nudge_y = c(-30,-19), size=9, color="white", fontface="bold") +
  theme(plot.title = element_text(hjust=-1.75, vjust=0.1, color="#d1d1d1", size=25),
        plot.background = element_rect(fill="#2d2d2d"),
        legend.text = element_text(color="#d1d1d1", size=20),
        axis.ticks.length = unit(0, "pt"),
        legend.title = element_text(size=20, color="#d1d1d1")) +
  guides(fill=guide_legend(title="Is transparent  "))

