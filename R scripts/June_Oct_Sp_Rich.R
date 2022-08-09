library (readxl)
library (tidyverse)
library(ggthemes)


sp_rich <- read_excel ('C:/Users/Alexandria Quinlan/Documents/Master_Thesis/R/data/gam_analysis.xlsx', sheet = 'sp_rich')


#install.packages("randomcoloR")
#library(randomcoloR)
#n <- 43

palette <- c ('#E3E688', '#7659DB', '#B0CFE4', '#DED2B6', 
  '#61EF4C', '#5FE784', '#D84B91', '#D7E5AD', '#E5A83B',
  '#96E09A', '#809E48', '#649B8D', '#E36042', '#D7A979', 
  '#876458', '#5FEACC', '#E5E6E5', '#7592D5', '#B6EBDA', 
  '#8D5D8A', '#8983E2', '#A7E854', '#E14CDA', '#DD7FDE', 
  '#E0E150', '#D6C4E1', '#71A7D2', '#E37F8A', '#6ADCE7',
  '#E4B4B6', '#DEA2DB', '#993AE8')

windows()
ggplot (sp_rich, aes (x = fct_inorder(abbrv), y = count)) +
  theme_igray() + 
  geom_bar (aes (fill = species),
    position = 'stack', 
    stat = 'identity')+
  facet_wrap (~Season, scales = 'free')+
  scale_fill_manual(values = palette)+
  theme(axis.title.x = element_text(margin = margin(t = 0, r = 20, b = 0, l = 0)))+
  coord_flip() +
  ggtitle ("Gametophyte Species Composition") +
  scale_y_continuous (breaks = seq (0,80,5))+
  xlab ('Host')+
  ylab ('# of gametophyte individuals')


