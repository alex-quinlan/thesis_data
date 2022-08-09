library (readxl)
library (tidyverse)
library (vegan)
#reference blog: https://john-quensen.com/tutorials/procrustes-analysis/

#Gametophyte sp. composition
Oct_gam_spe <- read_excel ('C:/Users/Alexandria Quinlan/Documents/Master_Thesis/R/data/spe_comp_data.xlsx', sheet = 'Oct_spe_by_host')

Oct_gam_spe <- as.data.frame (Oct_gam_spe)
rownames(Oct_gam_spe) <- Oct_gam_spe[,1]
Oct_gam_spe <- Oct_gam_spe[,-1]

DCA_gam <- decorana (log1p (Oct_gam_spe))
DCA_gam #first axis = 3.3, interesting, June is more heterogeneous #between 3 and 4, will still perform CCA for consistency

#create CA
CA_gam <- cca (log1p (Oct_gam_spe))
CA_gam

windows()
par (mfrow = c(1,2))
ordiplot (CA_gam, display = 'si', type = 't')
plot(CA_gam, type = "n")
ordilabel(CA_gam, dis="sp", font=2, cex = 0.5, priority=colSums(Oct_gam_spe))
mtext ('October Species Composition', side = 3, line = -3, outer = TRUE, cex = 1, font = 2)

#omit MP_O3 to see how the ordination changes
Oct_gam_spe2 <- Oct_gam_spe [1:8,1:21]
Oct_gam_spe2 <- Oct_gam_spe2 [,-11]

CA_gam2 <- cca (log1p (Oct_gam_spe2))
CA_gam2

windows()
par (mfrow = c(1,2))
ordiplot (CA_gam2, display = 'si', type = 't')
plot(CA_gam2, type = "n")
ordilabel(CA_gam2, dis="sp", font=2, cex = 0.6, priority=colSums(Oct_gam_spe2))
mtext ('October Species Composition', side = 3, line = -3, outer = TRUE, cex = 1, font = 2)

#How will PCA be different?
PCA_gam <- rda (log1p (Oct_gam_spe))
PCA_gam

windows()
ordiplot (PCA_gam, display = c('si'), type = 't') 
ordilabel (PCA_gam, dis = 'sp', font = 2, cex = 0.6, priority = colSums (Oct_gam_spe))
mtext ('October Species Composition', cex = 1, font = 2)

#NMDS
NMDS_gam <- metaMDS (Oct_gam_spe,  distance="bray", k=2)
windows()
ordiplot (NMDS_gam, display = c('si','sp'), type = 't', main = 'Gametophyte Species Composition of Hosts')
ordilabel (NMDS_gam, dis = 'sp', font = 2, cex = 0.6, border = NA, col = 'blue', priority = colSums (Oct_gam_spe))
mysubtitle <- "October"
mtext(side = 3, line = 0.25, at = 1, adj = 3, mysubtitle)

windows()
ordiplot (NMDS_gam, display = 'si', type = 't', font = 2, main = 'Gametophyte Species Composition of Hosts') 
orditorp(NMDS_gam, display = "species", priority =  colSums (Oct_gam_spe) , scaling = scl, air = 0.5,col = "forestgreen", font = 1, pch = 2, cex = 0.7)
mysubtitle <- "October"
mtext(side = 3, line = 0.25, at = 1, adj = 9.5, mysubtitle)

#sporophyte gametophyte merge
Oct_merge <- read_excel ('C:/Users/Alexandria Quinlan/Documents/Master_Thesis/R/data/spe_comp_data.xlsx', sheet = 'merge_Oct')

labels <- read_excel ('C:/Users/Alexandria Quinlan/Documents/Master_Thesis/R/data/spe_comp_data.xlsx', sheet = 'labels_oct')

Oct_merge <- as.data.frame (Oct_merge)
rownames(Oct_merge) <- Oct_merge[,1]
Oct_merge <- Oct_merge[,-1]

NMDS_merge <- metaMDS (Oct_merge,  distance="bray", trace=FALSE, trymax=100)


#display in 2 plots*
windows()
par (mfrow = c(1,2))
plot (NMDS_merge, type = 'n')
title ('Hosts', line = -1, cex.main = 1)
text (NMDS_merge, display = 'si', col = labels$col, font = 2, cex = 0.9)
plot (NMDS_merge, type = 'n')
title ('Species', line = -1, cex.main = 1)
orditorp(NMDS_merge, display = "species", priority =  colSums (Oct_merge) , scaling = scl, air = 0.5,col = "forestgreen", font = 1, pch = 2, cex = 0.9)
mtext ('Comparison of Gametophyte and Sporophyte Species Composition', side = 3, line = -2, outer = TRUE, cex = 1, font = 2)
mtext ('October', side = 3, line = -3, outer = TRUE, cex = 1, font = 1)


#October mature sporophytes
oct_merge_mature <- read_excel ('C:/Users/Alexandria Quinlan/Documents/Master_Thesis/R/data/spe_comp_data.xlsx', sheet = 'Oct_M_spor')

oct_merge_mature <- as.data.frame (oct_merge_mature)
rownames(oct_merge_mature) <- oct_merge_mature[,1]
oct_merge_mature <- oct_merge_mature[,-1]

NMDS_M <- metaMDS (oct_merge_mature,  distance="bray", trace=FALSE, trymax=100)

windows()
par (mfrow = c(1,2))
plot (NMDS_M, type = 'n')
title ('Hosts', line = -1, cex.main = 1)
text (NMDS_M, display = 'si', col = labels$col, font = 2, cex = 0.9)
plot (NMDS_M, type = 'n')
title ('Species', line = -1, cex.main = 1)
orditorp(NMDS_M, display = "species", priority =  colSums (oct_merge_mature) , scaling = scl, air = 0.5,col = "forestgreen", font = 1, pch = 2, cex = 0.9)
mtext ('Comparison of Gametophyte and Mature Sporophyte Species Composition', side = 3, line = -2, outer = TRUE, cex = 1, font = 2)
mtext ('October', side = 3, line = -3, outer = TRUE, cex = 1, font = 1)