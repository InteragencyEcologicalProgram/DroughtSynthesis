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

ggplot(ChlaA, aes(x = Whitepaper, y = log(Chlorophyll), fill = YearType)) + geom_boxplot()+
  drt_color_pal_yrtype() + theme_bw() + ylab("Chlorophyll-a (ug/L)")+xlab(NULL)
ggsave("plots/whitepaper/ChlWhitepaper.tiff", device = "tiff", width = 8, height = 6)
