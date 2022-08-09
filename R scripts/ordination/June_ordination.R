library (readxl)
library (tidyverse)
library (vegan)
#reference blog: https://john-quensen.com/tutorials/procrustes-analysis/

#Gametophyte sp. composition
June_gam_spe <- read_excel ('C:/Users/Alexandria Quinlan/Documents/Master_Thesis/R/data/spe_comp_data.xlsx', sheet = 'June_spe_by_host')

June_gam_spe <- as.data.frame (June_gam_spe)
rownames(June_gam_spe) <- June_gam_spe[,1]
June_gam_spe <- June_gam_spe[,-1]

labels <- read_excel ('C:/Users/Alexandria Quinlan/Documents/Master_Thesis/R/data/spe_comp_data.xlsx', sheet = 'merge_labels')

#first use DCA to determine if linear or unimodal

DCA_gam <- decorana (log1p (June_gam_spe))
DCA_gam #first axis = 4.16

#if length of first axis is larger than 4, use a unimodal test 

CA_gam <- cca (log1p (June_gam_spe)) #log1p due to high abundance of 0 in dataset
CA_gam

#display in one plot
windows()
June_gam <- ordiplot (CA_gam, type = 'none')
text (June_gam, 'sites', col = 'black', cex = 1)
text (June_gam, 'species', col = 'red', cex = 0.6)

#display in 2 plots
windows()
par (mfrow = c(1,2))
ordiplot (CA_gam, display = 'si', type = 't')
plot(CA_gam, type = "n")
ordilabel(CA_gam, dis="sp", font=2, cex = 0.6, priority=colSums(June_gam_spe))
mtext ('June Species Composition', side = 3, line = -3, outer = TRUE, cex = 1, font = 2)

#NMDS Gam*
NMDS_gam <- metaMDS (June_gam_spe,  distance="bray", trace=FALSE, trymax=100)

windows()
ordiplot (NMDS_gam, display = 'si', type = 't', font = 2, main = 'Gametophyte Species Composition of Hosts') 
orditorp(NMDS_gam, display = "species", priority =  colSums (June_gam_spe) , scaling = scl, air = 0.5,col = "forestgreen", font = 1, pch = 2, cex = 0.7)
mysubtitle <- "June"
mtext(side = 3, line = 0.25, at = 1, adj = 3, mysubtitle)





#NMDS for spor_gam comparison

June_merge_spe <- read_excel ('C:/Users/Alexandria Quinlan/Documents/Master_Thesis/R/data/spe_comp_data.xlsx', sheet = 'gam_spor_merge')

June_merge_spe <- as.data.frame (June_merge_spe)
rownames(June_merge_spe) <- June_merge_spe[,1]
June_merge_spe <- June_merge_spe[,-1]

NMDS <- metaMDS (June_merge_spe,  distance="bray", trace=FALSE, trymax=100)
NMDS

#display in one plot
windows()
ordiplot (NMDS, display = c('si'), type = 't') 
ordilabel (NMDS, dis = 'sp', font = 2, cex = 0.6, priority = colSums (June_merge_spe))
mtext ('June Gametophyte/Sporophyte Species Composition', cex = 1, font = 2)

#display in 2 plots*
windows()
par (mfrow = c(1,2))
plot (NMDS, type = 'n')
title ('Hosts', line = -1, cex.main = 1, font = 1)
text (NMDS, display = 'si', col = labels$type, font = 2, cex = 0.9)
plot (NMDS, type = 'n')
title ('Species', line = -1, font = 1, cex.main = 1)
orditorp(NMDS, display = "species", priority =  colSums (June_merge_spe) , scaling = scl, air = 0.5,col = "forestgreen", font = 1, pch = 2, cex = 0.9)
mtext ('Comparison of Gametophyte and Sporophyte Species Composition', side = 3, line = -2, outer = TRUE, cex = 1, font = 2)
mtext ('June', side = 3, line = -3, outer = TRUE, cex = 1, font = 1)

June_merge_mature <- read_excel ('C:/Users/Alexandria Quinlan/Documents/Master_Thesis/R/data/spe_comp_data.xlsx', sheet = 'June_M_spor')

Labels_M <- June_merge_mature <- read_excel ('C:/Users/Alexandria Quinlan/Documents/Master_Thesis/R/data/spe_comp_data.xlsx', sheet = 'labels_J_M')

June_merge_mature <- as.data.frame (June_merge_mature)
rownames(June_merge_mature) <- June_merge_mature[,1]
June_merge_mature <- June_merge_mature[,-1]

NMDS_M <- metaMDS (June_merge_mature,  distance="bray", trace=FALSE, trymax=100)

windows()
par (mfrow = c(1,2))
plot (NMDS_M, type = 'n')
title ('Hosts', line = -1, cex.main = 1)
text (NMDS_M, display = 'si', col = Labels_M$type, font = 2, cex = 0.9)
plot (NMDS_M, type = 'n')
title ('Species', line = -1, cex.main = 1)
orditorp(NMDS_M, display = "species", priority =  colSums (June_merge_mature) , scaling = scl, air = 0.5,col = "forestgreen", font = 1, pch = 2, cex = 0.9)
mtext ('Comparison of Gametophyte and Mature Sporophyte Species Composition', side = 3, line = -2, outer = TRUE, cex = 1, font = 2)
mtext ('June', side = 3, line = -3, outer = TRUE, cex = 1, font = 1)






##attempt at procrustes - does not work with my data set
#edit gametophyte data to use procrustes
#gam_spe_pro <- June_gam_spe [c(-2,-3),]#omit LS1_J1 and IF_J1 for procrustes comparison b/c they have no sporophyte growth and need same # of rows for the analysis 

#CA_gam_pro <- cca (log1p (gam_spe_pro))
#CA_gam_pro
#ordiplot (CA_gam_pro, display = c('si'), type = 't')

#pro <- procrustes (X = CA_gam_pro, Y = CA_spor, symmetric = FALSE)

#plot(pro, kind = 1, type = "text")
#plot(pro, kind = 2)