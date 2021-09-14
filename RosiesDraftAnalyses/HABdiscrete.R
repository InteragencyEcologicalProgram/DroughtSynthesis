#Habs, from visual assessments

library(tidyverse)
library(discretewq)
library(sf)
library(emmeans)

HABs = wq( Start_year = 2010,End_year = 2020,
  Sources = c("EMP", "STN", "FMWT", "EDSM", "DJFMP", "20mm", 
              "Baystudy", "USGS"))

str(HABs)

#load regions shapefile
regions = st_read("C:/Users/rhartman/OneDrive - California Department of Water Resources/Drought/Barrier/BarrierRegions/shpExport.shp") %>%
  st_make_valid()


HABssf = filter(HABs, !is.na(Longitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = st_crs(4326))

ggplot() + geom_sf(data = regions) + geom_sf(data = HABssf)
#crop it to the area right around the barrier
BarHABs = st_crop(HABssf, regions)

ggplot() + geom_sf(data = regions) + geom_sf(data = BarHABs)

BH =   st_join(BarHABs, regions) %>%
  st_drop_geometry() %>%
  filter(!is.na(Regions), Month >4 & Month <11) %>%
  mutate(Yearf = as.factor(Year))

BHm = filter(BH, !is.na(Microcystis))

ggplot(BHm, aes(x = as.factor(Year), y = Microcystis)) +geom_boxplot()+ facet_wrap(~Regions)

ggplot(BHm, aes(x = Regions, y = Microcystis)) +geom_boxplot()+ facet_wrap(~Year)

ggplot(BH, aes(x = Date, y = Temperature)) +geom_boxplot()+ facet_wrap(~Year)


ggplot(BHm, aes(x = Regions, fill = as.factor(Microcystis))) +geom_bar(position = "fill")+ facet_wrap(~Year) +
  scale_fill_manual(values = c("white", "tan2", "yellow", "red", "darkred"), 
                    labels = c("absent", "low", "medium", "high", "very high"),
                    name = "Microcystis")+ ylab("Relative Frequency")

#definitely more microcystis reported in 2016 and 2018 than 2015

BH2015.2014 = filter(BHm, Year %in% c(2014, 2015, 2016))
BH2018 = filter(BHm, Year == 2018)


ggplot(BH2015.2014, aes(x = Month, fill = as.factor(Microcystis))) +geom_bar(position = "fill")+ facet_wrap(~Regions)  +
  scale_fill_manual(values = c("white", "tan2", "yellow", "red"), 
                    labels = c("absent", "low", "medium", "high"),
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
  Microcystis > 1 ~ TRUE))

#now a glm (binomial)
bn1 = glm(HABPA ~ Month + Yearf+Regions, data = BH, family = "binomial")
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
mm1 = glmer(HABPA ~ scale(Temperature) + scale(Secchi) +Month+ (1|Yearf) + (1|Station), data = BHm, family = "binomial")
summary(mm1)
visreg(mm1)
