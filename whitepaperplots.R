#Graphs for the short-term white paper

library(tidyverse)
library(lubridate)
library(DroughtData)

load("data/NutrientsChlorophyll.RData")

yrs = read_csv("data/yearassignments.csv") %>%
  rename(YearAdj = Year) %>%
  select(YearAdj, Whitepaper)

#nitrate graph
Nitrate = left_join(Nitrate, yrs) %>%
  mutate(Whitepaper = factor(Whitepaper, levels = c("Critical", "Dry", "Below Normal", "Above Normal", "Wet", "2020", "2021", "2022"),
                             ordered = T))



ggplot(Nitrate, aes(x = Whitepaper, y = DissNitrateNitrite, fill = YearType)) + geom_boxplot()+
  drt_color_pal_yrtype() + theme_bw() + ylab("Dissolved Nitrate+Nitrite (mg/L)")+xlab(NULL)

ggsave("plots/whitepaper/NitrateWhitepaper.tiff", device = "tiff", width = 8, height = 6)


#Phosphorus graph
Phos = left_join(Phos, yrs) %>%
  mutate(Whitepaper = factor(Whitepaper, levels = c("Critical", "Dry", "Below Normal", "Above Normal", "Wet", "2020", "2021", "2022"), ordered = T))



ggplot(Phos, aes(x = Whitepaper, y = DissOrthophos, fill = YearType)) + geom_boxplot()+
  drt_color_pal_yrtype() + theme_bw() + ylab("Dissolved Orthophosphate (mg/L)")+xlab(NULL)

ggsave("plots/whitepaper/PHosWhitepaper.tiff", device = "tiff", width = 8, height = 6)

#Chlorophyll
ChlaA = left_join(ChlaA, yrs) %>%
  mutate(Whitepaper = factor(Whitepaper, levels = c("Critical", "Dry", "Below Normal", "Above Normal", "Wet", "2020", "2021", "2022"), ordered = T))





ggplot(ChlaA, aes(x = Whitepaper, y = log(Chlorophyll), fill = YearType)) + geom_boxplot()+
  drt_color_pal_yrtype() + theme_bw() + ylab("log Chlorophyll-a (ug/L)")+xlab(NULL)

ggplot(ChlaA, aes(x = Whitepaper, y = Chlorophyll, fill = YearType)) + geom_boxplot()+
  drt_color_pal_yrtype() + theme_bw() + ylab("Chlorophyll-a (ug/L)")+xlab(NULL) +
  facet_wrap(~Region, nrow = 4, strip.position = "right")+ 
  scale_x_discrete(labels = c("Critical", "Dry", "Below\nNormal", "Above\nNormal", "Wet", "2020", "2021"))+
  theme(legend.position ="none")
  
ggsave("plots/whitepaper/ChlWhitepaper.tiff", device = "tiff", width = 5, height = 6)

############################################
# Temperature

yrs = read_csv("data/yearassignments.csv") %>%
  rename(YearAdj = Year) %>%
  dplyr::select(YearAdj, Whitepaper)
wq = lt_avg_wq   %>%
  left_join(yrs)%>%
  mutate(Whitepaper = factor(Whitepaper, levels = c("Critical", "Dry", "Below Normal", "Above Normal", "Wet", "2020", "2021", "2022"),
                             ordered = T),
         Season = factor(Season, levels = c("Winter", "Spring", "Summer", "Fall")))


#secchi depth
ggplot(wq, aes(x = Whitepaper, y = Secchi, fill = YearType)) + geom_boxplot()+
  drt_color_pal_yrtype() + theme_bw() + 
  ylab("Secchi Depth (cm)")+xlab(NULL)+ 
  scale_x_discrete(labels = c("Critical", "Dry", "Below\nNormal", "Above\nNormal", "Wet", "2020", "2021"))+
  theme(legend.position ="none")
ggsave("plots/whitepaper/SecchiWhitepaper.tiff", device = "tiff", width = 5, height = 4)

#Temperature
ggplot(wq, aes(x = Whitepaper, y = Temperature, fill = YearType)) + geom_boxplot()+
  drt_color_pal_yrtype() + 
  facet_wrap(~Season, nrow = 4, scales = "free_y",  strip.position = "right")+
  theme_bw() + 
  scale_x_discrete(labels = c("Critical", "Dry", "Below\nNormal", "Above\nNormal", "Wet", "2020", "2021"))+
  ylab("Water Temperature (C)")+xlab(NULL)+ 
  theme(legend.position ="none")
ggsave("plots/whitepaper/TempWhitepaper.tiff", device = "tiff", width = 5, height = 6)

#Salinity
ggplot(wq, aes(x = Whitepaper, y = Salinity, fill = YearType)) + geom_boxplot()+
  drt_color_pal_yrtype() + facet_wrap(~Region, nrow = 5, scales = "free_y", strip.position = "right")+
  theme_bw() + scale_x_discrete(labels = c("Critical", "Dry", "Below\nNormal", "Above\nNormal", "Wet", "2020", "2021"))+
  ylab("Salinity (PSU)")+xlab(NULL)+ theme(legend.position ="none")
ggsave("plots/whitepaper/SalWhitepaper.tiff", device = "tiff", width = 5, height = 7)


