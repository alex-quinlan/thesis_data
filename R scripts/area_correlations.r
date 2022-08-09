library (readxl)
library (tidyverse)
library(ggthemes)
library (cowplot)
library (ggpubr)


host_data <- read_excel ('C:/Users/Alexandria Quinlan/Documents/Master_Thesis/R/data/stat_analysis.xlsx', sheet = 'host_data')

#assess normality of the data
ggqqplot (host_data$dbh)
ggqqplot (host_data$`surface area`)
ggqqplot (host_data$individuals)
ggqqplot (host_data$gam_species)
ggqqplot (host_data$spor_species)

#linear regressions
#dbh and gam abundance

##correlation with dbh for each season #Figure 4
#June
windows()
#correlation between dbh and abundance 
ggscatter(host_data[1:11,], x ="dbh", y = "individuals", conf.int = TRUE, 
cor.coef = TRUE, cor.method = "pearson",
xlab = "Diameter at Breast Height (DBH)", ylab = "# of gametophyte individuals", label = "abbrev")

#October
ggscatter(host_data[12:20,], x ="dbh", y = "individuals", conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", xlab = "Diameter at Breast Height (DBH)", ylab = "# of gametophyte individuals", label = "abbrev")


#correlations between gametophyte richness, gametophyte abundance, and host surface area for all hosts (values are logged here)

host_data$surf_area_log <- log10 (host_data$`surface area`) 
host_data$gam_spe_log <- log10 (host_data$gam_species)
host_data$gam_ind_sq <- sqrt (host_data$individuals)

#Figure 6
ggscatter(host_data, x = 'surf_area_log', y = 'gam_spe_log', conf.int = TRUE, cor.coef = TRUE, cor.method = "pearson", xlab = "log10(surface area)", ylab = "log10(# of gametophyte species)", label = 'abbrev', font.label = c(10, 'plain'))

#correlation between dbh and abundance for all hosts 
#Figure 5.1
windows()
ggscatter(host_data, x = "surf_area_log", y = "gam_ind_sq", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", xlab = "log10(surface area)", ylab = "sqrt (# of gametophyte individuals)", label = 'abbrev', font.label = c(10, 'plain')) #reveals a moderate positive relationship


# correlation between spor_richness and dbh for all hosts 
#figure 5.2
windows()
ggscatter(host_data, x = "dbh", y = "spor_species", conf.int = TRUE, 
          cor.coef = TRUE, cor.method = "pearson", xlab = "Diameter at Breast Height (DBH)", ylab = "# of sporophyte species", label = 'abbrev', font.label = c(10, 'plain'))
