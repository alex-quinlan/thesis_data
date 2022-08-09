library (readxl)
library (tidyverse)
library (vegan)

#import data
Oct_spe <- read_excel ('C:/Users/Alexandria Quinlan/Documents/Master_Thesis/R/data/gam_analysis.xlsx', sheet = 'Oct_spe_by_host')

Oct_spe <- as.data.frame (Oct_spe)
rownames(Oct_spe) <- Oct_spe[,1]
Oct_spe <- Oct_spe[,-1]

hist (Oct_spe [Oct_spe > 0])

#log transform to see if it changes right skew
hist (log10 (Oct_spe [Oct_spe > 0]))

oct_spe_log <- log1p (Oct_spe)

#use transformed data in PCA
PCA <-rda (X = oct_spe_log)

PCA
head (summary (PCA))

windows ()
#displays site numbers
ordiplot (PCA, display = 'si', type = 't') 
#adds on species vectors
biplot (PCA, display = 'species', type = 't')

biplot (PCA, type = 't', scaling = 'species')

#clean the plot, need source function
cleanplot.pca (PCA, scaling = 1)


##environmental variables
host_data <- read_excel ('C:/Users/Alexandria Quinlan/Documents/Master_Thesis/R/data/gam_analysis.xlsx', sheet = 'host_ref')

Host_data <- host_data [13:21,1:15]

tbRDA <- rda (oct_spe_log ~ total_light + total_host_area, data = Host_data) 

constrained_eig <- tbRDA$CCA$eig/tbRDA$tot.chi*100
unconstrained_eig <- tbRDA$CA$eig/tbRDA$tot.chi*100
expl_var <- c(constrained_eig, unconstrained_eig)
barplot (expl_var[1:20], col = c(rep ('red', length (constrained_eig)), rep ('black', length (unconstrained_eig))), las = 2, ylab = '% variation')
tbRDA

windows()
ordiplot (tbRDA, display = 'species', type = 't')

#how do I figure out what PC1 is?