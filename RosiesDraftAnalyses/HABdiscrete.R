#Habs, from visual assessments

library(tidyverse)
library(discretewq)
library(sf)
library(emmeans)
library(lubridate)
library(ggmap)


HABs = read_csv("data/HABs/WQ_data_integrated4.csv")
str(HABs)
HABs = mutate(HABs,  Datex = as.Date(Date2, origin = "1899-12-30"),
              MonthYear = NULL, Datetime = NULL, Month = month(Datex), Year = year(Datex))


#load regions shapefile
regions = st_read("C:/Users/rhartman/OneDrive - California Department of Water Resources/Drought/Barrier/BarrierRegions/shpExport.shp") %>%
  st_make_valid()


HABssf = filter(HABs, !is.na(Longitude), !is.na(Latitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = st_crs(4326))

foo = filter(HABssf, Source == "STN", Year == 2021)

ggplot() + geom_sf(data = WW_Delta) + geom_sf(data = HABssf)+
  geom_sf_label(data = HABssf, aes(label = Station), 
                position = "jitter", label.size = 0.05)
#crop it to the area right around the barrier
BarHABs = st_crop(HABssf, regions)

ggplot() + geom_sf(data = regions) + geom_sf(data = BarHABs)

BH =   st_join(BarHABs, regions) %>%
  st_drop_geometry() %>%
  filter(!is.na(Regions), Month >4) %>%
  mutate(Yearf = as.factor(Year))

BHm = filter(BH, !is.na(Microcystis), Year >2013, Month %in% c(5,6,7,8,9, 10))

ggplot(BHm, aes(x = as.factor(Year), y = Microcystis)) +geom_boxplot()+ facet_wrap(~Regions)

ggplot(BHm, aes(x = Regions, y = Microcystis)) +geom_boxplot()+ facet_wrap(~Year)

ggplot(BH, aes(x = Date, y = Temperature)) +geom_point()+ facet_wrap(~Year)


ggplot(BHm, aes(x = Regions, fill = as.factor(Microcystis))) +geom_bar(position = "fill")+ facet_wrap(~Year) +
  scale_fill_manual(values = c("white", "tan2", "yellow", "red", "darkred"), 
                    labels = c("absent", "low", "medium", "high", "very high"),
                    name = "Microcystis")+ ylab("Relative Frequency")

#definitely more microcystis reported in 2016 and 2018 than 2015



BH2015.2014 = filter(BHm, Year %in% c(2014, 2015, 2016))
BH2018 = filter(BHm, Year == 2018)


ggplot(BH2015.2014, aes(x = Month, fill = as.factor(Microcystis))) +geom_bar(position = "fill")+ facet_wrap(~Regions)  +
  scale_fill_manual(values = c("white", "tan2", "yellow", "red", "darkred"), 
                    labels = c("absent", "low", "medium", "high", "veryhigh"),
                    name = "Microcystis")

ggplot(BH2015.2014, aes(x = as.factor(Microcystis), y = Temperature)) + geom_boxplot()+ facet_wrap(~Year)
ggplot(BH2015.2014, aes(x = Microcystis, y = Temperature, color = Yearf)) +
  geom_point()+ geom_smooth(method = "lm") + theme_bw()


ggplot(BH2015.2014, aes(x = as.factor(Microcystis), y = Secchi)) + geom_boxplot()+ facet_wrap(~Year)
ggplot(BH2015.2014, aes(x = Microcystis, y = Secchi, color = Yearf)) +
  geom_point()+ geom_smooth(method = "lm") + theme_bw() + ylab("Secchi Depth (cm)")


ggplot(BH2018, aes(x = Month, fill = as.factor(Microcystis))) +geom_bar(position = "fill")+ facet_wrap(~Regions)  +
  scale_fill_manual(values = c("white", "tan2", "yellow", "red", "darkred"), 
                    labels = c("absent", "low", "medium", "high", "veryhigh"),
                    name = "Microcystis")



#OK, how do we model this? Maybe converte to presence/absence?
BHm = mutate(BHm, HABPA = case_when(
  Microcystis == 1 ~ FALSE,
  Microcystis > 1 ~ TRUE)) %>%
filter(Year>2013)

#now a glm (binomial)
bn1 = glm(HABPA ~ Month + Temperature + Yearf*Regions, data = BHm, family = "binomial")
summary(bn1)
emmeans(bn1, pairwise ~ Yearf)
plot(bn1)

library(visreg)
visreg(bn1, scale = "response")
visreg(bn1, xvar = "Regions", by = "Yearf")
#we really weren't seeing anything in 2015!

emp = filter(HABs, Source == "EMP", !is.na(Microcystis), Year == 2015)
table(emp$Month)
#I guess EMP only started doing microcystis midway through 2015 darn

#Look at potential drivers of the bloom
library(lme4)
dat = filter(BHm, Year > 2013)
mm1 = glmer(HABPA ~ scale(Temperature) + scale(Secchi) +Month+ (1|Yearf) + (1|Station), data = dat, family = "binomial")
summary(mm1)
visreg(mm1)



#############################################################################################
#What do I use for the SFHSR?

library(deltamapr)
HABs2021 = filter(HABs, Year == 2021)
sumfall = filter(HABs, Month %in% c(6,7,8,9,10))
ggplot(sumfall, aes(x = Year, fill = as.factor(Microcystis))) +geom_bar(position = "fill")+ 
  scale_fill_manual(values = c("white", "tan2", "yellow", "red", "darkred"), 
                    labels = c("absent", "low", "medium", "high", "very high"),
                    name = "Microcystis")+ ylab("Relative Frequency")

reg2 = R_EDSM_Regions_1819P1 %>%
  st_transform(crs = st_crs(4326))

reg3 = R_EDSM_Strata_1718P1%>%
  st_transform(crs = st_crs(4326))


HABssf1 = filter(sumfall, !is.na(Longitude), !is.na(Latitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = st_crs(4326))

ggplot() + geom_sf(data = reg3) + geom_sf(data = HABssf1)
#crop it to the area we are interested in
sfhab = st_crop(HABssf1, reg3)

#plot the map
ggplot() + geom_sf(data = WW_Delta, fill = "lightgrey")+
  geom_sf(data = reg3, aes(fill = Stratum), alpha = 0.4) + 
  geom_sf(data = sfhab, aes(shape = Source)) +
  scale_fill_brewer(palette = "Set3", guide = NULL)+
  #coord_sf(xlim = c(-122.3, -121.4), ylim = c(37.8, 38.6))+
  geom_sf_label(data = reg3, aes(label = Stratum), 
                position = "jitter", label.size = 0.05)+
  theme_bw()

SFH =   st_join(sfhab, reg3) %>%
  st_drop_geometry() %>%
  filter(!is.na(Stratum), !is.na(Microcystis)) %>%
  mutate(Yearf = as.factor(Year),
         Month2 = factor(Month, levels = c(6,7,8,9,10),
                         labels = c("Jun", "Jul", "Aug", "Sep", "Oct")),
         Stratum = factor(Stratum, levels = c("Western Delta", "Suisun Bay", "Suisun Marsh", "Lower Sacramento",
                                              "Lower San Joaquin", "Eastern Delta", "Southern Delta",
                                              "Cache Slough/Liberty Island", "Sac Deep Water Shipping Channel",
                                              "Upper Sacramento"), 
                          labels = c("Far West", "Suisun Bay", "Suisun Marsh", "Lower Sac",
                                     "Lower SJ", "East Delta", "South Delta", "Cache/Liberty", "SDWSC",
                                     "Upper Sac")))        

ggplot(SFH, aes(x = Stratum, fill = as.factor(Microcystis))) +geom_bar(position = "fill")+ facet_wrap(~Year)+
  scale_fill_manual(values = c("white", "tan2", "yellow", "red", "darkred"), 
                    labels = c("absent", "low", "medium", "high", "very high"),
                    name = "Microcystis")+ ylab("Relative Frequency") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))


ggplot(filter(SFH, Year == 2021), aes(x = Stratum, fill = as.factor(Microcystis))) + 
  geom_bar(position = "fill")+ 
  facet_grid(.~Month2)+
  scale_fill_manual(values = c("white", "tan2", "yellow", "red", "darkred"), 
                    labels = c("absent", "low", "medium", "high", "very high"),
                    name = "Microcystis")+ ylab("Relative Frequency")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1))


ggplot(SFH, aes(x = Year, fill = as.factor(Microcystis))) +geom_bar(position = "fill")+ 
  scale_fill_manual(values = c("white", "tan2", "yellow", "red", "darkred"), 
                    labels = c("absent", "low", "medium", "high", "very high"),
                    name = "Microcystis")+ ylab("Relative Frequency") 

ggplot(filter(SFH, Year == 2021), aes(x = Month, fill = as.factor(Microcystis))) + 
  geom_bar(position = "fill")+ 
  #facet_grid(.~Month2)+
  scale_fill_manual(values = c("white", "tan2", "yellow", "red", "darkred"), 
                    labels = c("absent", "low", "medium", "high", "very high"),
                    name = "Microcystis")+ ylab("Relative Frequency")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1))

###################################################################################
#box plots by stratum
library(wql)

SFH2021 = filter(SFH, Year == 2021) %>%
  mutate(Salinity = ec2pss(Conductivity/1000, t = Temperature))


ggplot(SFH2021, aes(x = Stratum, y = Temperature, fill = Stratum)) + 
  geom_boxplot()+ 
  facet_grid(.~Month2)+ ylab("Water Temperature C")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1))

ggplot(SFH2021, aes(x = Stratum, y = Turbidity)) + 
  geom_boxplot()+ 
  facet_grid(.~Month2)+ ylab("Turbidity (FNU)")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1))

ggplot(SFH2021, aes(x = Stratum, y = Conductivity)) + 
  geom_boxplot()+ 
  facet_grid(.~Month2)+ ylab("Conductivity uS/cm")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1))



ggplot(SFH2021, aes(x = Stratum, y = Salinity)) + 
  geom_boxplot()+ 
  facet_grid(.~Month2)+ ylab("Salinity (PSU)")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1))

####################################################################################
#Models
library(readxl)
SFH2 = mutate(SFH, HABPA = case_when(
  Microcystis == 1 ~ FALSE,
  Microcystis > 1 ~ TRUE)) %>%
  filter(Year >2013)

#year types
yeartypes = read_excel("data/Integrated data set.xlsx", sheet = "yearassignments")

SFHwflow = left_join(SFH2, yeartypes)
#now a glm (binomial)

bn1 = glm(HABPA ~ Month + Yearf+Stratum, data = SFH2, family = "binomial")
summary(bn1)
emmeans(bn1, pairwise ~ Yearf)
plot(bn1)

library(visreg)
library(lme4)
library(lmerTest)
visreg(bn1, scale = "response")
visreg(bn1, xvar = "Regions", by = "Yearf")


Ind1 = glmer(HABPA ~ Month + Index +Salinity + Temperature+ (1|Yearf), data = SFHwflow, family = "binomial")
summary(bn1)
plot(bn1)
visreg(bn1)
library(visreg)

#Could I use the effect of the TUCP on water year index to figure out how big of an impact
#it might have had on weeds?

flow1= glmer(HABPA ~ Month + ShortTerm +Salinity + Temperature+ (1|Yearf), data = SFHwflow, family = "binomial")
summary(flow1)
emmeans(flow1, pairwise ~ ShortTerm)
plot(flow1)
visreg(bn1)
library(visreg)
