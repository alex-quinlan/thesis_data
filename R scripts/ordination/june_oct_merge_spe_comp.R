library (readxl)
library (tidyverse)
library (vegan)

#Gametophyte sp. composition
Oct_June_spe <- read_excel ('C:/Users/Alexandria Quinlan/Documents/Master_Thesis/R/data/spe_comp_data.xlsx', sheet = 'June_Oct_merge')

Oct_June_spe <- as.data.frame (Oct_June_spe)
rownames(Oct_June_spe) <- Oct_June_spe[,1]
Oct_June_spe <- Oct_June_spe[,-1]

#hull data 
by_season <- read_excel ('C:/Users/Alexandria Quinlan/Documents/Master_Thesis/R/data/spe_comp_data.xlsx', sheet = 'hull_data')

NMDS <- metaMDS (Oct_June_spe,  distance="bray", trace=FALSE, trymax=100)
NMDS

windows()
ordiplot (NMDS, display = 'si', type = 't', font = 2, main = 'June vs. October Gametophyte Species Composition') 
orditorp(NMDS, display = "species", priority =  colSums (Oct_June_spe) , scaling = scl, air = 0.5,col = "forestgreen", font = 1, pch = 2, cex = 0.7)
#mysubtitle <- "June"
#mtext(side = 3, line = 0.25, at = 1, adj = 3, mysubtitle)

#display in 2 plots
windows()
par (mfrow = c(1,2))
plot (NMDS, type = 'n')
ordihull (NMDS, groups = by_season$season, col = 'blue', lty = 'dotted')
title ('Hosts', line = -1, cex.main = 1)
text (NMDS, display = 'si', font = 2, cex = 0.9)
plot (NMDS, type = 'n')
title ('Gametophyte Species', line = -1, cex.main = 1)
orditorp(NMDS, display = "species", priority =  colSums (Oct_June_spe) , scaling = scl, air = 0.5,col = "forestgreen", font = 1, pch = 2, cex = 0.9)
mtext ('June vs. October Gametophyte Species Composition', side = 3, line = -2, outer = TRUE, cex = 1, font = 2)


