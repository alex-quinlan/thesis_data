library (readxl)
library (ggpubr)

richness <- read_excel ('C:/Users/Alexandria Quinlan/Documents/Master_Thesis/R/data/stat_analysis.xlsx', sheet = 'sp_rich')
abundance <- read_excel ('C:/Users/Alexandria Quinlan/Documents/Master_Thesis/R/data/stat_analysis.xlsx', sheet = 'abundance')



library (ggplot2)
library (car)

#ANOVA example 
data("PlantGrowth")

ggplot(PlantGrowth, aes(PlantGrowth$weight)) + 
  geom_density(aes(data = PlantGrowth$weight, fill = PlantGrowth$group), position = 'identity', alpha = 0.5) +
  labs(x = 'Weight', y = 'Density') + scale_fill_discrete(name = 'Group') + scale_x_continuous(limits = c(2, 8))

qqPlot(PlantGrowth$weight, col = PlantGrowth$group)

plant.aov <- anova(lm(weight ~ group, data = PlantGrowth))
plant.aov

#my data
#species richness
windows ()
ggplot(richness, aes(sp_rich)) + 
  geom_density(aes(data = sp_rich, fill = host), position = 'identity', alpha = 0.5) +
  labs(x = 'species richness', y = 'Density') + scale_fill_discrete(name = 'Host') + scale_x_continuous(limits = c(2, 8))

qqPlot(richness$sp_rich)

rich_anov <- aov(sp_rich ~ host, data = richness)
summary (rich_anov)

rich_anov_log <- aov(log10(sp_rich) ~ host, data = richness)

windows()
par(mfrow=c(2,2))
plot(rich_anov)
par(mfrow=c(1,1))

tukey_rich <- TukeyHSD(rich_anov)
tukey_rich

#abundance
abund_anov <- aov(abund ~ host, data = abundance)
summary (abund_anov)

windows()
par(mfrow=c(2,2))
plot(abund_anov)
par(mfrow=c(1,1))

tukey_rich <- TukeyHSD(abund_anov)
tukey_rich

