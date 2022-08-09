library (readxl)
library (tidyverse)
library(ggthemes)

a.podo_rich <- read_excel ('C:/Users/Alexandria Quinlan/Documents/Master_Thesis/R/data/gam_analysis.xlsx', sheet = 'A.podo_rich')
windows()
ggplot (a.podo_rich, aes (x = fct_inorder(Host), y = count)) +
  theme_igray() + 
  geom_bar (aes (fill = species),
            position = 'stack', 
            stat = 'identity')+
  scale_fill_manual(values = c('#e3e688', '#7659db', '#ded2b6', '#5fe784', '#d84b91', '#e5a83b', '#809e48', '#7592d5', '#e14cda', '#e0e150', '#6adce7', '#993ae8'))+
  coord_flip() +
  ggtitle ("Gametophyte Species Composition", subtitle = 'Alsohpila podophylla') +
  xlab ("Host")+
  ylab ('# of gametophyte individuals') +
  theme(plot.margin = unit(c(2,2,2,2),"cm"))+
  theme(plot.subtitle=element_text(face="italic"))
        