library (readxl)
library (tidyverse)
library (vegan)

#import data for gametophyte species
June_gam_spe <- read_excel ('C:/Users/Alexandria Quinlan/Documents/Master_Thesis/R/data/gam_analysis.xlsx', sheet = 'June_spe_by_host')

June_gam_spe <- as.data.frame (June_gam_spe)
rownames(June_gam_spe) <- June_gam_spe[,1]
June_gam_spe <- June_gam_spe[,-1]

windows()
hist (June_gam_spe [June_gam_spe > 0])
#log transform to see if it changes right skew
hist (log10 (June_gam_spe [June_gam_spe > 0])) #hardly

#transform data for true zeros
June_gam_spe_log <- log1p (June_gam_spe)

#use transformed data in PCA
PCA <-rda (X = June_gam_spe)

PCA
head (summary (PCA))

#displays site numbers
windows()
ordiplot (PCA, display = 'si', type = 't') 
#adds on species vectors
biplot (PCA, display = 'species', type = 't')

biplot (PCA, type = 't', scaling = 'species')

#import data for sporophyte species
June_spor_spe <- read_excel ('C:/Users/Alexandria Quinlan/Documents/Master_Thesis/R/data/gam_analysis.xlsx', sheet = 'June_spor_by_host')

June_spor_spe <- as.data.frame (June_spor_spe)
rownames(June_spor_spe) <- June_spor_spe[,1]
June_spor_spe <- June_spor_spe[,-1]

windows()
hist (June_spor_spe [June_spor_spe > 0])
#log transform to see if it changes right skew
hist (log10 (June_spor_spe [June_spor_spe > 0])) #hardly

#transform data for true zeros
June_spor_spe_log <- log1p (June_spor_spe)

#use transformed data in PCA
PCA <-rda (X = June_spor_spe)

PCA
head (summary (PCA))

#displays site numbers
windows()
ordiplot (PCA, display = 'si', type = 't') 
#adds on species vectors
biplot (PCA, display = 'species', type = 't')

biplot (PCA, type = 't', scaling = 'species')
