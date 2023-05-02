library(tidyverse)
library(igraph)
library(rcartocolor)
####################
### creating initial datase
####################
flowchart <- tibble(from = c("population\n1,000人",
                             "population\n1,000人",
                             "sick\n10人",
                             "sick\n10人",
                             "not_sick\n990人",
                             "not_sick\n990人"),
                    to = c("sick\n10人",
                          "not_sick\n990人",
                          "true_positive\n9人",
                          "false_negative\n1人",
                          "true_negative\n891人",
                          "false_positive\n99人"))
####################
### defining the layout
####################
g <-  graph_from_data_frame(flowchart, directed = TRUE)
coords <- igraph::layout_as_tree(g)
colnames(coords) <-  c("x", "y")
####################
### adding attributes
####################
output_df <-  as_tibble(coords) |> 
  mutate(step = vertex_attr(g, "name"),
         label = gsub("\\d+$", "", step),
         x = x*1,
        type = factor(c(2, 2, 2, 1, 3, 3, 1)))
#         type = factor(c(1, 4, 5, 2, 3, 3, 2)))
###################
### making the boxes
###################
plot_nodes <-  output_df |> 
  mutate(xmin = x - 0.45,
         xmax = x + 0.45,
         ymin = y - 0.25,
         ymax = y + 0.25)
###################
### making the edges
###################
plot_edges = flowchart |> 
  mutate(id = row_number()) |> 
  pivot_longer(cols = c("from", "to"),
               names_to = "s_e",
               values_to = "step") |> 
  left_join(plot_nodes, by = "step") |> 
  select(-c(label, type, y, xmin, xmax)) |> 
  mutate(y = ifelse(s_e == "from", ymin, ymax)) |> 
  select(-c(ymin, ymax))
##################
### plotting a flowchart uing ggplot2
#################
p <-  ggplot() +
  geom_rect(data = plot_nodes,
            mapping = aes(xmin = xmin, ymin = ymin, 
                          xmax = xmax, ymax = ymax, 
                          fill = type, colour = type),
            alpha = 0.5) 
###################
### adding labels
###################
 p <- p + 
  geom_text(data = plot_nodes,
            mapping = aes(x = x, y = y, label = label)) 
###################
### drawing the arrows
###################
p <-  p + 
  geom_path(data = plot_edges,
            mapping = aes(x = x, y = y, group = id),
            colour = "#585c45",
            arrow = arrow(length = unit(0.3, "cm"), type = "closed"))
###################
### coolour schemes
###################
p = p + 
  rcartocolor::scale_fill_carto_d(palette = "Antique") +
  rcartocolor::scale_colour_carto_d(palette = "Antique")
###################
### adding text
###################
p <-  p + 
  labs(title = "どれくらい??",
       caption = "有病率   1%\n感度　 90%\n特異度 90%") 
###################
### editing themes
###################
p = p + 
  theme_void() +
  theme(plot.margin = unit(c(1, 1, 0.5, 1), "cm"),
        legend.position = "none",
        plot.background = element_rect(colour = "#f2e4c1", fill = "#f2e4c1"),
        panel.background = element_rect(colour = "#f2e4c1", fill = "#f2e4c1"),
        plot.title = element_text(family = "Meiryo", hjust = 0, face = "bold",
                                  size = 20, color = "#585c45",
                                  margin = margin(t = 10, r = 0, b = 10, l = 0)),
        plot.caption = element_text(family = "Meiryo", hjust = 0,
                                    size = 12, color = "#585c45",
                                    margin = margin(t = 10)))

