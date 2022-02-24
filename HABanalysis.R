#Final HABs graphs and analyses
library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(sf)
library(Deltamapr)
library(brms)
library(DHARMa)
library(visreg)
library(MASS)
library(car)
library(DroughtData)

#import data with all the visual index data
load("HABs.RData")



#load regions shapefile
regions = st_read("C:/Users/rhartman/OneDrive - California Department of Water Resources/Drought/Barrier/BarrierRegions/shpExport.shp") %>%
  st_make_valid()

# regions = R_EDSM_Strata_1718P1%>%
#   st_transform(crs = st_crs(4326)) %>%
#   filter(Stratum %in% c("Lower Sacramento", "Lower San Joaquin", "Southern Delta"))

HABssf = filter(HABs, !is.na(Longitude), !is.na(Latitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = st_crs(4326))

ggplot() + geom_sf(data = WW_Delta) + geom_sf(data = HABssf)+
  geom_sf_label(data = HABssf, aes(label = Station), 
                position = "jitter", label.size = 0.05)

############################################################################
#crop it to the area right around the barrier
BarHABs = st_crop(HABssf, regions)

ggplot() + geom_sf(data = regions) + geom_sf(data = BarHABs)

BH =   st_join(BarHABs, regions) %>%
  st_drop_geometry() %>%
  filter(!is.na(Regions), Month >4) %>%
  mutate(Yearf = as.factor(Year))

BHm = filter(BH, !is.na(Microcystis), Year >2013, Month %in% c(5,6,7,8,9, 10))



ggplot(BHm, aes(x = Regions, fill = as.factor(Microcystis))) +geom_bar(position = "fill")+ facet_wrap(~Year) +
  scale_fill_manual(values = c("white", "tan2", "yellow", "red", "darkred"), 
                    labels = c("absent", "low", "medium", "high", "very high"),
                    name = "Microcystis")+ ylab("Relative Frequency")+
  scale_x_discrete(labels = c("Central Delta", "Sacramento", "San Joaquin"))


###################################################################

#look at regions across the Delta
sumfall = filter(HABs, Month %in% c(6,7,8,9,10), !is.na(Microcystis))
ggplot(sumfall, aes(x = Year, fill = as.factor(Microcystis))) +geom_bar(position = "fill")+ 
  scale_fill_manual(values = c("white", "tan2", "yellow", "red", "darkred"), 
                    labels = c("absent", "low", "medium", "high", "very high"),
                    name = "Microcystis")+ ylab("Relative Frequency")

reg2 = R_EDSM_Regions_1819P1 %>%
  st_transform(crs = st_crs(4326))

reg3 = R_EDSM_Strata_1718P1%>%
  st_transform(crs = st_crs(4326)) %>%
  mutate(Stratum2 = factor(Stratum, 
                           levels = c("Western Delta", "Suisun Bay", "Suisun Marsh", "Lower Sacramento",
                                      "Lower San Joaquin", "Eastern Delta", "Southern Delta",
                                      "Cache Slough/Liberty Island", "Sac Deep Water Shipping Channel",
                                      "Upper Sacramento"), 
                           labels = c("Far West", "Suisun Bay", "Suisun Marsh", "Lower Sac",
                                      "Lower SJ", "East Delta", "South Delta", "Cache/Liberty", "SDWSC",
                                      "Upper Sac")),
         nudge = c(-.05,0,0,0,0,0,0,0,.1,0))


HABssf1 = filter(sumfall, !is.na(Longitude), !is.na(Latitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = st_crs(4326))

ggplot() + geom_sf(data = reg3) + geom_sf(data = HABssf1)

#crop it to the area we are interested in
sfhab = st_crop(HABssf1, reg3)%>%
  st_join(reg3)# %>%
#filter(Year == 2021) 



SFH =   sfhab %>%
  st_drop_geometry() %>%
  #  filter(!is.na(Stratum), !is.na(Microcystis), 
  #        Stratum %in% c("Suisun Marsh", "Suisun Bay", "Lower Sacramento", "Cache Slough/Liberty Island", "Sac Deep Water Shipping Channel")) %>%
  mutate(Yearf = as.factor(Year),
         Month2 = factor(Month, levels = c(6,7,8,9,10),
                         labels = c("Jun", "Jul", "Aug", "Sep", "Oct")))   


####################################################################################
#Models for HAB weed report


HWR =   sfhab%>%
  st_drop_geometry() %>%
  filter(!is.na(Stratum), !is.na(Microcystis)) %>% 
  mutate(Yearf = as.factor(Year),
         Month2 = factor(Month, levels = c(6,7,8,9,10),
                         labels = c("Jun", "Jul", "Aug", "Sep", "Oct")),
         HABord = case_when(
           Microcystis == 1 ~ "absent",
           Microcystis %in% c(2,3) ~ "Low",
           Microcystis %in% c(4,5) ~ "High")) %>%
  mutate(HABord = factor(HABord, levels = c("absent", "Low", "High"), ordered = T)) %>%
  filter(Year >2013)

SFH2 = mutate(HWR, HABPA = case_when(
  Microcystis == 1 ~ FALSE,
  Microcystis > 1 ~ TRUE)) %>%
  filter(Year >2013)

#year types
yeartypes = read_csv("data/yearassignments.csv")

SFHwflow = left_join(SFH2, yeartypes)
#now a glm (binomial)


bn1 = glm(HABPA ~ Month + Yearf+Stratum, data = SFH2, family = "binomial")
summary(bn1)
emmeans(bn1, pairwise ~ Yearf)
plot(bn1)



visreg(bn1, scale = "response")
visreg(bn1, xvar = "Regions", by = "Yearf")

#model of month, Sac vally index, salinity, and temperature
Ind1 = glmer(HABPA ~ Month + Index +Salinity + Temperature+ (1|Yearf), data = SFHwflow, family = "binomial")
summary(Ind1)
plot(bn1)
visreg(bn1)

##############################################################
#ordered logistic regression
Regions<-read_csv("RosiesDraftAnalyses/Rosies_regions2.csv")


## Load Delta Shapefile from Brian
Delta<-deltamapr::R_EDSM_Subregions_Mahardja_FLOAT%>%
  filter(SubRegion%in%unique(Regions$SubRegion))%>%  #Filter to regions of interest
  dplyr::select(SubRegion)

Regs = unique(Regions[,c(1,5)])
Delta = merge(Delta, Regs) %>%
  st_transform(crs = 4326)



Habs2 =   st_join(HABssf, Delta) %>%
  st_drop_geometry() %>%
  filter(!is.na(Region), !is.na(Microcystis)) %>% 
  mutate(Yearf = as.factor(Year),
         Month2 = factor(Month, levels = c(6,7,8,9,10),
                         labels = c("Jun", "Jul", "Aug", "Sep", "Oct")))    



SFH2a = mutate(Habs2, HABord = case_when(
  Microcystis == 1 ~ "absent",
  Microcystis %in% c(2,3) ~ "Low",
  Microcystis %in% c(4,5) ~ "High")) %>%
  mutate(HABord = factor(HABord, levels = c("absent", "Low", "High"), ordered = T)) %>%
  filter(Year >2013)

#now an orgered logistic regression

ord2 = polr(HABord ~Yearf, data = SFH2a, Hess = T)
summary(ord2)
Anova(ord2)
pairs = emmeans(ord2, pairwise ~ Yearf)$contrasts
#write.csv(pairs, "visualdata_alldelta.csv")
pr <- profile(ord2)
confint(pr)
plot(pr)
pairs(pr)

(ctable <- coef(summary(ord1)))
## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

## combined table
(ctable <- cbind(ctable, "p value" = p))
(ci <- confint(ord1))
exp(cbind(OR = coef(ord1), ci))



###################################################
#import temperature analyses
Temps = read_csv("C:/Users/rhartman/OneDrive - California Department of Water Resources/Drought/Barrier/DWR_TempCount.csv")

HAB3 = pivot_wider(HWR, id_cols = c(Yearf, Stratum2), 
                   names_from = HABord, values_from = Microcystis,
                   values_fn = length, values_fill = 0) %>%
  ungroup() %>%
  mutate(present = Low + High)

#connect stratums to temperature stations
cdec = read.csv("data/CDEC_StationsEC.csv")
cdecsf = st_as_sf(cdec, coords = c("Longitude", "Latitude"), crs = 4326)

stasT = filter(cdecsf, STA %in% c("BET", "DWS", "FRK", "LIB", "LPS",
                                  "MAL","MRZ", "NSL", "SJJ", "SRH"))
stasT2 = st_join(stasT, reg3) %>%
  st_drop_geometry() %>%
  rename(Station = STA) %>%
  dplyr::select(Station, Stratum2)

#join temperatures to regions
Temps = left_join(Temps, stasT2)

#Summarize by stratum
Tempssum = group_by(Temps, Stratum2, Year, Threshold) %>%
  summarize(Days = mean(Days)) %>%
  mutate(Yearf = as.factor(Year))

#Attach to HAB data.
HAB3a = left_join(HAB3, Tempssum) %>%
  mutate(PresentPerc = present/(present + absent), Total = present + absent) %>%
  filter(!is.na(Threshold)) %>%
  mutate(Threshold = factor(Threshold, labels = c("19 C", "25 C")))

ggplot(HAB3a, aes(x = Days, y = PresentPerc)) + geom_point(aes(shape = Yearf, color = Stratum2))+
  facet_wrap(~Threshold, scales = "free_x") + geom_smooth(method = "lm")+
  scale_shape_manual(values = c(16,17,15,4,5,6,7,8), name = NULL)+
  scale_color_brewer(palette = "Dark2", name = NULL)+theme_bw()+
  ylab("Percent of observations\n with Microcystis present")+
  xlab("Days above temperature threshold")

HAB3b = group_by(HAB3a, Year, Threshold) %>%
  summarize(Days = mean(Days), Percent = mean(PresentPerc), 
            total = sum(Total), present = sum(present))
ggplot(HAB3b, aes(x = Days, y = Percent)) + geom_point()+
  facet_wrap(~Threshold, scales = "free_x") + geom_smooth(method = "lm")+
  ylab("Percent of observations\n with Microcystis Present")+
  xlab("Days above temperature threshold")
#binomial regression

Thresh25 = filter(HAB3a, Threshold == "Temp25")
Thresh19 = filter(HAB3a, Threshold == "Temp19")

b1 = glmer(present/Total~ Days + (1|Stratum2), data = Thresh19,
           family = "binomial")
summary(b1)
visreg(b1, scale = "response", gg = TRUE)+ xlab("Days above 19 C")+
  ylab("Probability of Microcystis Presence") + theme_bw()

plot(b1)
b2 = glmer(present/Total~ Days + (1|Stratum2), data = Thresh25,
           family = "binomial")
summary(b2)
visreg(b2, scale = "response")

##############################################################################
#Now the flow analysis



flowX = raw_hydro_1975_2021 %>%
  rename(Year = YearAdj)
Temps = raw_wq_1975_2021 %>%
  rename(Year = YearAdj)

names(flowX)
names(Temps)
names(HWR)
HWR = mutate(HWR, Date = as.Date(Date))
test = left_join(HWR, dplyr::select(flowX, -Year, -Season)) %>%
  filter(Temperature >0, Temperature <30)




#start with this. I may also want month or day of year in here.
#Exports will be another interesting explanitory variable. I also need
#to figure out if I"m using the right distribution

#Note: All of these models take a long time to run. If you want to just get
#the results and look at them, use:
load("HABsbrimsresults.RData")

#Three terms and a random effect using all the data looks like its going to take days to run.
#####################

#let's simplify

SoDelta = filter(test, Stratum2 == "South Delta")


#OK, this is super, duper not working. WHat can I do here?

M5.1 = brm(HABord ~ Temperature + Outflow + Export +  Yearf, data = SoDelta, family = acat,
           iter = 2000,   backend = "cmdstanr", normalize = FALSE, 
           control = list(max_treedepth = 15),
           chains = 2, cores=4, threads = threading(2))
#Why did this one run so fast? 

summary(M5.1)
conditional_effects(M5.1, categorical = TRUE)
max_mc1_effects <- conditional_effects(M5.1, "Temperature", categorical= TRUE)$Temperature
ggplot(max_mc1_effects) + geom_smooth(aes(x = Temperature, y = estimate__, color = HABord)) +
  geom_ribbon(aes(x = Temperature, ymin = lower__, ymax = upper__))


#hmmm.... this one is definitley telling me that outflow is a better predictor than exports.
#Oh, but I wanted to scale this. And maybe log-transform exports
SoDelta = mutate(SoDelta, day = yday(Date), Outscale = scale(Outflow), Exscale = scale(Export), Tempscale = scale(Temperature))

M5.2 = brm(HABord ~ Tempscale + Outscale + Exscale + (1|Yearf/day), data = SoDelta, family = acat,
           iter = 2000,   backend = "cmdstanr", normalize = FALSE, 
           control = list(max_treedepth = 15),
           chains = 2, cores=4, threads = threading(2))
#Why did this one run so fast? 

summary(M5.2)
conditional_effects(M5.2, categorical = TRUE)
#hmmm.... this one is definitley telling me that outflow is a better predictor than exports.
#what the actual fuck. Now it's super fast. 
#was it the 'acat' instead of cumulative?
M5.3 = brm(HABord ~ Tempscale + Outscale + Exscale + Yearf + (1|day), data = SoDelta, family = cumulative,
           iter = 2000,   backend = "cmdstanr", normalize = FALSE, 
           control = list(max_treedepth = 15),
           chains = 2, cores=4, threads = threading(2))
# Nope. Not that. 
summary(M5.3)
conditional_effects(M5.3, categorical = TRUE)
#definitley a lower outflow effect and a bigger export effect when you take day of year into account
#save(M5.3, M5.2, M5.1, file = "HABsbrimsresults.RData")