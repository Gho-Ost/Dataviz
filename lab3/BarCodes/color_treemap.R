install.packages('R.utils')
install.packages("treemapify")
library(data.table)
library(tidyr)
library(ggplot2)
library(dplyr)
library(treemapify)

#library(shiny)
#library(shinythemes)

#toc: true
#toc_float:
#  toc_collapsed: true

colors <- fread("rebrickable/colors.csv.gz")
inv_parts <- fread("rebrickable/inventory_parts.csv.gz")

# Pie chart of color amounts
# colors to hex
color_id_sums <- inv_parts %>% group_by(color_id) %>% summarise(sum = sum(quantity))

color_sums <- inner_join(x=colors, y=color_id_sums, by=join_by(id==color_id))

color_sums <- color_sums %>% mutate(
  r = paste("0x", substr(rgb, start=1, stop=2), sep=''),
  g = paste("0x", substr(rgb, start=3, stop=4), sep=''),
  b = paste("0x", substr(rgb, start=5, stop=6), sep='')
)

#pie(top_colors$sum, col = rgb(top_colors$r, top_colors$g, top_colors$b, maxColorValue=255))

# Bare top n
top_n <- 34
top_colors <- color_sums %>% arrange(sum) %>% tail(top_n) %>% mutate(order = seq.int(top_n))

top_colors %>% ggplot(aes(area=sum, fill=factor(order), label=rgb)) + 
  geom_treemap() +
  geom_treemap_text(min.size = 8, size=20, color=c(rep("#ffffff", nrow(top_colors)-3), 1, rep("#ffffff", 2))) +
  scale_fill_manual(values=rgb(top_colors$r, top_colors$g, top_colors$b, maxColorValue=255)) +
  ggtitle("Most frequent colors") +
  theme(legend.position="none", plot.background = element_rect(fill="#2d2d2d"),
        plot.title=element_text(hjust=.5, size=25, colour = "#d1d1d1"))

# shiny stuff

runtime: shiny
```{r shinytopncolors}
ui <- fluidPage(
  theme = shinytheme("slate"),
  sliderInput("a", label = "number of colors", min = 5, max = 105, value = 10, step=1, width='100%'),
  plotOutput("plot")
)

server <- function(input, output, session) {
  
  df <- reactive({
    alpha <- as.numeric(input$a)
    df <- color_sums %>% arrange(sum) %>% tail(alpha) %>% mutate(order = seq.int(alpha))
    data.frame(df)
  })
  
  output$plot <- renderPlot({
    df2 <- df()
    ggplot(df2, aes(area=sum, fill=factor(order), label=rgb)) + 
      geom_treemap() +
      geom_treemap_text(min.size = 8, size=20, color=c(rep("#ffffff", nrow(df2)-3), 1, rep("#ffffff", 2))) +
      scale_fill_manual(values=rgb(df2$r, df2$g, df2$b, maxColorValue=255)) +
      ggtitle("Most frequent colors") +
      theme(legend.position="none", plot.background = element_rect(fill="#2d2d2d"),
            plot.title=element_text(hjust=.5, size=25, colour = "#d1d1d1"))
  })
}

shinyApp(ui, server, options = list(height=500))
```





















