#Quick plot of the major droughts and stuff

library(waterYearType)
library(tidyverse)
library(smonitr)
library(DroughtData)
pal_yrtype <- c( "Critical" = "#FDE333", "Dry" = "#53CC67", "Below Normal" = "#009B95","Above Normal" = "#00588B", "Wet" = "#4B0055")
indecies = filter(water_year_indices, location != "San Joaquin Valley")
i1820 = data.frame(WY = c(2018,2019,2020,2021), Index = c(7.14, 10.34, 6.0, 4.0), 
                   Yr_type = c("Below Normal", "Wet", "Dry", "Critical"), location = c(rep("Sacramento Valley", 4)))
indecies = bind_rows(indecies, i1820) %>%
  mutate(Yr_type = factor(Yr_type, levels = c("Critical", "Dry", "Below Normal", "Above Normal", "Wet")))
write.csv(indecies, "data/indecies.csv")

WYs = read.csv("data/yearassignments.csv")
WYs = mutate(WYs,Yr_type = factor(Yr_type, levels = c("Critical", "Dry", "Below Normal", "Above Normal", "Wet")))

ggplot(WYs)+
  geom_tile(data = filter(WYs, Drought == "D"), aes(x = Year, y = -1, height = 0.5), fill = "#FDE333", color = "#FDE333")  +
  geom_tile(data = filter(WYs, Drought == "W"), aes(x = Year, y = -1, height = 0.5), fill = "#00588B", color = "#00588B")  +
  geom_tile(data = filter(WYs, Drought == "N"), aes(x = Year, y = -1, height = 0.5), fill = "#53CC67", color = "#53CC67")  +
  
  geom_bar(aes(x = Year, y = Index, fill = Yr_type), stat = "identity")+
 scale_fill_manual(values = pal_yrtype, name = "Water Year Type")+
  coord_cartesian(xlim = c(1906, 2022))+theme_bw()+
  theme(legend.position = "top") +
  ylab("Sacramento Valley Index")+
  xlab(NULL)+
  annotate("text", x = 1960, y = -2, label = "Drought (yellow)/Neutral(green)/Wet Period (blue)")

#year types by decade
ggplot(WYs, aes(x = Year, fill = Yr_type))+ geom_histogram(binwidth = 10)+
  drt_color_pal_yrtype()

ggplot(filter(WYs, Year > 1969), aes(x = Year, fill = Yr_type))+ geom_histogram(binwidth = 10)+
  drt_color_pal_yrtype()

WYs = mutate(WYs, decYear = as.numeric(substr(as.character(Year), 4,4)), decade = floor(Year/10)*10)

ggplot(WYs, aes(x = decade, y = decYear, fill = Yr_type))+ geom_tile()+
  geom_text(aes(label = Year))+
  drt_color_pal_yrtype()


#without the drought periods
ggplot(WYs)+
  
  geom_bar(aes(x = Year, y = Index, fill = Yr_type), stat = "identity")+
  scale_fill_manual(values = pal_yrtype, name = "Water Year Type")+
  coord_cartesian(xlim = c(1906, 2022))+theme_bw()+
  theme(legend.position = "top") +
  ylab("Sacramento Valley Index")+
  xlab("Water Year")



#for HABWeed


ggplot(filter(WYs, Year >2006))+
  geom_bar(aes(x = Year, y = Index, fill = Yr_type), stat = "identity")+
  scale_fill_manual(values = pal_yrtype, name = "Water Year Type")+
  theme_bw()+
  theme(legend.position = "top") +
  ylab("Sacramento Valley Index")+
  xlab(NULL)


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

droughts = read.csv("data/WYs_1906-2020.csv")
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

###########################################3333
#Quick plot of drought/wet years

DW = read_excel("data/Integrated data set.xlsx", sheet = "yearassignments")
ggplot(DW, aes(x = Year, y = Index, fill = Drought)) + geom_col() + 
  scale_fill_manual(values = c("red", "grey", "blue"), 
                    labels = c("Drought", "Neutral", "Wet Period"))

#recent years
DWrecent = data.frame(Year = 2011:2021, DW = c("Wet", "Neutral", "Neutral", 
                                               "Dry", "Dry + Barrier", "Neutral",
                                               "Wet", "Neutral",  "Wet",
                                               "Dry", "Dry + Barrier"), 
                      Index = c(filter(DW, Year >2010)$Index, 4))

ggplot(DWrecent, aes(x = Year, y = Index, fill = DW)) + geom_col() + 
  scale_fill_manual(values = c("orange", "red", "grey", "blue"), name = NULL)+
  scale_x_continuous(breaks = c(2011, 2014, 2015, 2017, 2019, 2020, 2021))


ggplot(DW, aes(x = Year, y = Index, fill = Yr_type)) + geom_col() +  
  scale_fill_manual(values = c("chartreuse3", "darkorange", "firebrick", "firebrick1", "dodgerblue"))
