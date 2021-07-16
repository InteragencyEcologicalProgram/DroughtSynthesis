#Quick plot of the major droughts and stuff

library(waterYearType)
library(tidyverse)
library(smonitr)

indecies = water_year_indices
i1820 = data.frame(WY = c(2018,2019,2020, 2018,2019,2020), Index = c(7.14, 10.34, 6.0, 3.03, 4.94, 2.1), 
                   Yr_type = c("Below Normal", "Wet", "Dry", "Below Normal", "Wet", 
                              "Critical"), location = c(rep("Sacramento Valley", 3), rep("San Joaquin Valley", 3)))
indecies = bind_rows(indecies, i1820)

ggplot(filter(indecies, location != "San Joaquin Valley"), aes(x = WY, y = Index))+
  geom_bar(stat = "identity", aes(fill = Yr_type))+  
  scale_fill_manual(values = c("chartreuse3", "darkorange", "firebrick", "firebrick1", "dodgerblue"))+
  geom_smooth(method = "lm") +

  #facet_grid(location~., scales = "free_y")+
  coord_cartesian(xlim = c(1900, 2020))+
  theme(legend.position = "bottom")


Seasons = data.frame(Season = c("Spring", "Summer", "Winter", "Fall"))


indseason = filter(indecies, location == "Sacramento Valley") %>%
  merge(Seasons)
write.csv(indseason, "indseason.csv")

#Have droughts increased in frequency?

#bin dry/wet
indecies = filter(indecies,  location == "Sacramento Valley") %>%
  mutate(DY = case_when(Yr_type %in% c("Critical", "Dry", "Below Normal") ~ "dry",
                    Yr_type %in% c("Above Normal", "Wet") ~ "wet"))

indecies$num <- sequence(rle(indecies$DY)$lengths)

droughts = read.csv("WYs_1906-2020.csv")
lm1 = glm(Drought ~WY, data = droughts, family = "binomial")
summary(lm1)
library(visreg)
visreg(lm1)
lm2 = glm(Index ~WY, data = droughts)
summary(lm2)
visreg(lm2)

#look at the palmer drought severity index
pdsi <- read_csv("4-pdsi-all-3-1895-2021.csv")
pdsi_annual = mutate(pdsi, Year = signif(Date, digits = 4)) %>%
  group_by(Year) %>%
  summarize(pdsi = mean(Value))
ggplot(pdsi_annual, aes(x = Year, y = pdsi) ) + geom_bar(stat = "identity") + geom_smooth(method = "lm")

lm3 = lm(pdsi~Year, data = pdsi_annual)
summary(lm3)

#just the sacramento drainage
pdsiSAC <- read_csv("sacramento-pdsi-all-2-1895-2021.csv")
pdsi_annualSAC = mutate(pdsiSAC, Year = signif(Date, digits = 4)) %>%
  group_by(Year) %>%
  summarize(pdsi = mean(Value))
ggplot(pdsi_annualSAC, aes(x = Year, y = pdsi) ) + geom_bar(stat = "identity") + geom_smooth(method = "lm")+
  annotate("text", x = 192000, y = -3, label = "-5.70e-5x +1.10, p = 0.154")+ ggtitle("Sacramento Drainage")

lm3SAC = lm(pdsi~Year, data = pdsi_annualSAC)
summary(lm3SAC)


#just the San Joaquin drainage
pdsiSJ <- read_csv("SJ-pdsi-all-2-1895-2021.csv")
pdsi_annualSJ = mutate(pdsiSJ, Year = signif(Date, digits = 4)) %>%
  group_by(Year) %>%
  summarize(pdsi = mean(Value))
ggplot(pdsi_annualSJ, aes(x = Year, y = pdsi) ) + geom_bar(stat = "identity") + geom_smooth(method = "lm")+
  annotate("text", x = 192000, y = -4, label = "-1.53e-4x +2.98, p = 0.002")+ ggtitle("San Joaquin Drainage")

lm3SJ = lm(pdsi~Year, data = pdsi_annualSJ)
summary(lm3SJ)

