library (readxl)
library (ggpubr)
library (car)
library (ggplot2)


#ANOVA for RH Data
all_85 <- read_excel ('C:/Users/Alexandria Quinlan/Documents/Master_Thesis/R/data/all_85.xlsx', sheet = 'summary')

RH_85_anov <- aov(counts ~ host, data = all_85)
summary (RH_85_anov)

tukey_RH_85 <- TukeyHSD (RH_85_anov)
tukey_RH_85

all_95 <- read_excel ('C:/Users/Alexandria Quinlan/Documents/Master_Thesis/R/data/all_95.xlsx', sheet = 'summary')

RH_95_anov <- aov(counts ~ host, data = all_95)
summary (RH_95_anov)

tukey_RH_95 <- TukeyHSD (RH_95_anov)
tukey_RH_95


#ggplot 85
windows()
plot_85 <- ggplot(data = all_85, aes(x= min_duration, y= freq, color= host))+
  geom_line(size=1) + 
  ylab("Frequency (per day)") +
  xlab("Duration (hrs)") +
  scale_x_continuous(breaks=seq(0.5, 7, by=1), limits = c(0.5,7)) +
  scale_y_continuous(breaks=seq(0, 0.35, by=.05), limits = c(0, 0.35))+ 
  labs (color = 'host') + 
  scale_color_manual(values = c("#FC4E07", "#00AFBB", "#E7B800"), na.translate = F) +
  ggtitle ("Time hosts spent under 85% RH", subtitle = "from May to August") +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'), plot.subtitle = element_text(hjust = 0.5))

#ggplot95
windows()
plot_95 <- ggplot(data = all_95, aes(x= min_duration, y= freq, color= host))+
  geom_line(size=1) + 
  ylab("Frequency (per day)") +
  xlab("Duration (hrs)") +
  scale_x_continuous(breaks=seq(0.5, 18.5, by=2), limits = c(0.5,18.5)) +
  scale_y_continuous(breaks=seq(0, 1.6, by=.20), limits = c(0,1.6))+ 
  labs (color = 'host') + scale_color_manual(values = c("#FC4E07", "#00AFBB", "#E7B800"), na.translate = F) +
  ggtitle ("Time hosts spent under 95% RH", subtitle = "from May to August") +
  theme(plot.title = element_text(hjust = 0.5, face = 'bold'), plot.subtitle = element_text(hjust = 0.5))

#add statistical significance table
windows()
plot_85 +  annotate ("label", x = 6.4, y = 0.34, label = 'Angiosperm-Live Tree Fern: p < 2e-16 \n Angiosperm-Dead Tree Fern: p < 2e-16 \n Live Tree Fern-Dead Tree Fern: NS', fill = 'white')

windows()
plot_95 + annotate ("label", x = 16.6, y = 1.55, label = 'Angiosperm-Live Tree Fern: p < 0.00001 \n Angiosperm-Dead Tree Fern: p < 0.00003 \n Live Tree Fern-Dead Tree Fern: NS', fill = 'white')
