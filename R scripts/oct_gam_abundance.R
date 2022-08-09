library (readxl)
library (tidyverse)
library(ggthemes) # Load

Oct_graph_data <- read_excel ('C:/Users/Alexandria Quinlan/Documents/Master_Thesis/raw_data/xcel/OctoberSurvey.xlsx', sheet = 'gam_distribution')

my_colors <- c('#90A955', '#31572c')

ggsave ('octplots.png', width = 9, height = 7, units = 'in')


ggplot (Oct_graph_data, 
        aes (fill = zone, x = fct_inorder(host), y = gam)) +
  theme_igray() + 
  geom_bar (
    position = 'stack', 
    stat = 'identity',
    color = NA, width = 0.8) + 
  facet_grid (~plot, scales="free_x", space = "free_x") +
  scale_fill_manual(values = my_colors) + 
  xlab ('Host') + 
  ylab ('# of gametophytes') + 
  ggtitle ('Gametophyte Abundance and Distribution', subtitle = 'October') +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'),
    plot.subtitle = element_text(hjust = 0.5, size = 14)) +
  scale_y_continuous(breaks = seq(0, 90, 10)) +
  theme(axis.text.x = element_text (family = 'sans', size = 10, angle = 45, hjust = 1)) + theme(axis.title.y = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))

dev.off ()

