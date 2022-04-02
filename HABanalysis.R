#Final HABs graphs and analyses
library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
library(sf)
library(deltamapr)
library(brms)
library(DHARMa)
library(visreg)
library(MASS)
library(car)
library(DroughtData)
<<<<<<< HEAD
library(lubridate)
=======
library(here)

i_am("HABanalysis.R")
>>>>>>> 187b6e57e63a3b6c897a4f01222a57d76f63b79c

#import data with all the visual index data
load("HABs.RData")



#load Barrier regions shapefile
regions = st_read(here("EDB/Spatial_data/EDB_Regions.shp")) %>%
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

BHm = filter(BH, !is.na(Microcystis), Year >2013, Month %in% c(6,7,8,9, 10))



ggplot(BHm, aes(x = Regions, fill = as.factor(Microcystis))) +geom_bar(position = "fill")+ facet_wrap(~Year) +
  scale_fill_manual(values = c("white", "tan2", "yellow", "red", "darkred"), 
                    labels = c("absent", "low", "medium", "high", "very high"),
                    name = "Microcystis")+ ylab("Relative Frequency")+
  scale_x_discrete(labels = c("Central Delta", "Sacramento", "San Joaquin"))


###################################################################

#look at regions across the Delta
sumfall = filter(HABs, Month %in% c(6,7,8,9,10), !is.na(Microcystis))

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

#Now let's do the entire year, by regions
sfhaball2 = st_crop(HABssf, reg3)%>%
  st_join(reg3)%>%
  st_drop_geometry() %>%
  filter(!is.na(Stratum), !is.na(Microcystis)) %>%
  #        Stratum %in% c("Suisun Marsh", "Suisun Bay", "Lower Sacramento", "Cache Slough/Liberty Island", "Sac Deep Water Shipping Channel")) %>%
  mutate(Yearf = as.factor(Year), Yearm = Year + (Month-1)/12, Mic = factor(Microcystis, levels = c(1,2,3,4,5), labels = c(
    "absent", "low", "med", "high", "v.high")))  

ggplot() + geom_sf(data = reg3) + geom_sf(data = HABssf1)

#crop it to the area we are interested in
sfhab = st_crop(HABssf1, reg3)%>%
  st_join(reg3)# %>%
#filter(Year == 2021) 

#do the whole delta, but broken up into subregions
sfhaball = filter(HABs, !is.na(Microcystis), !is.na(Longitude), !is.na(Latitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = st_crs(4326)) %>%
st_crop(HABssf1, reg3)%>%
  st_join(reg3)


SFHall =   sfhaball2 %>%
    filter(!is.na(Stratum), !is.na(Microcystis)) %>%
  #        Stratum %in% c("Suisun Marsh", "Suisun Bay", "Lower Sacramento", "Cache Slough/Liberty Island", "Sac Deep Water Shipping Channel")) %>%
  mutate(Yearf = as.factor(Year), Yearm = Year + (Month-1)/12, Mic = factor(Microcystis, levels = c(1,2,3,4,5), labels = c(
    "absent", "low", "med", "high", "v.high")))   


###############################
#plot of just 2021, all months

ggplot(filter(SFHall, Year == 2021, Month <10), aes(x = Stratum2, fill = Mic))+geom_bar(position = "fill")+
  facet_wrap(~Month, nrow = 3)+
  scale_fill_manual(values = c("white", "tan2", "yellow", "red", "darkred"), name = "Microcystis") + 
  theme_bw()+
  theme(legend.position = "top")+
  ylab(NULL) + xlab(NULL)+
  theme(axis.text.x = element_text(angle = 90, hjust = 1))


SFHall2 = group_by(SFHall, Yearm, Yearf, Year, Month, Mic, Stratum) %>%
  summarize(n = n()) %>%
  mutate(Yearm2 = as.factor(Yearm)) %>%
  ungroup()

SFHallzeros =   pivot_wider(SFHall2, id_cols = c(Yearm, Yearf, Year, Month, Stratum), 
                            names_from = Mic, values_from = n, values_fill = 0) %>%
  pivot_longer(cols = c(absent, low, med, high, v.high), values_to = "n", names_to = "Mic") %>%
  mutate(Mic = factor(Mic, levels = c(
    "absent", "low", "med", "high", "v.high")))

ggplot(filter(SFHallzeros, Stratum != "Western Delta"), aes(x = Yearm, y = n, fill = Mic, group = Mic)) + geom_area(position = "fill")+
  facet_wrap(~Stratum, nrow = 4)+
  scale_fill_manual(values = c("white", "tan2", "yellow", "red", "darkred"), name = "Microcystis") + 
  theme_bw()+
  theme(legend.position = "top")+
  ylab(NULL) + xlab(NULL)

ggsave("Microtimeseries.tiff", device = "tiff", width = 11, height = 8, units = "in")

ggplot(filter(SFHallzeros, Stratum != "Western Delta"), aes(x = Yearm, y = n, color = Mic)) + geom_point()+
  facet_wrap(~Stratum, nrow = 3) 

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


effort = group_by(HWR, Year, Stratum) %>%
  summarize(N = n()) %>%
  pivot_wider(id_cols = Stratum, names_from = Year, values_from = N)

write.csv(effort, "visualindexeffort.csv")

SFH2 = mutate(HWR, HABPA = case_when(
  Microcystis == 1 ~ FALSE,
  Microcystis > 1 ~ TRUE)) %>%
  filter(Year >2013)

#year types
yeartypes = read_csv("data/yearassignments.csv")

SFHwflow = left_join(SFH2, yeartypes)
#now a glm (binomial)



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
<<<<<<< HEAD
  filter(Year >2013) %>%
  droplevels()


foo = group_by(SFH2a, Year) %>%
  summarize(table(HABord))
=======
  filter(Year >2013) %>% 
  mutate(Yearf = fct_drop(Yearf))
>>>>>>> 187b6e57e63a3b6c897a4f01222a57d76f63b79c

#now an orgered logistic regression

ord2 = polr(HABord ~Yearf, data = SFH2a, Hess = T)
summary(ord2)
Anova(ord2)
pairs = emmeans(ord2, pairwise ~ Yearf)
cont = pairs$contrasts
plot(emmeans(ord2, pairwise ~ Yearf), comparisons = TRUE)
tukcfg = cld(emmeans(ord2, pairwise ~ Yearf), Letters = letters) %>%
  mutate(Year = as.numeric(as.character(Yearf)), 
         Letter = str_trim(.group)) 



#write.csv(pairs, "visualdata_alldelta.csv")
pr <- profile(ord2)
confint(pr)
plot(pr)
pairs(pr)


#Plot across the whole Delta, just summer/fall
ggplot(sumfall, aes(x = Year, fill = as.factor(Microcystis))) +
  geom_bar(position = "fill")+ 
  scale_fill_manual(values = c("white", "tan2", "yellow", "red", "darkred"), 
                    labels = c("absent", "low", "medium", "high", "very high"),
                    name = "Microcystis")+ ylab("Relative Frequency") +
  geom_text(data = tukcfg, aes(x = Year, y = 0.7, label = Letter), inherit.aes = F)


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

library(RColorBrewer)
library(smonitr)
pal = c(brewer.pal(8, "Set2"), brewer.pal(8, "Dark2"))

flowX = raw_hydro_1975_2021 %>%
  rename(Year = YearAdj)
Temps = raw_wq_1975_2021 %>%
  rename(Year = YearAdj)


Dayflow = get_odp_data(pkg_id = "dayflow", fnames = "Dayflow Results")


DF1997_2020 =  Dayflow$`Dayflow Results 1997 - 2020` %>%
  mutate( Date = as.Date(Date, format = "%m/%d/%Y")) %>%
  dplyr::select(Date, OUT, EXPORTS, SJR)


DF2021 =  Dayflow$`Dayflow Results 2021` %>%
  mutate( Date = as.Date(Date, format = "%m/%d/%Y")) %>%
  dplyr::select(Date, OUT, EXPORTS, SJR) 


#now I can put them all together!
DF = bind_rows(DF1997_2020, DF2021)



ggplot(filter(flowX, Outflow >0, Year > 2007), 
       aes(y = Outflow, x = Export, color = as.factor(Year))) + geom_point()+
  scale_y_log10()+ ylab("Daily Average Delta Outflow (CFS)")+
  xlab("Daily Average SWP+CVP Exports (CFS)")+
  scale_color_manual(values = pal, name = NULL)+
  theme_bw()

names(DF)
names(flowX)
names(Temps)
names(HWR)
HWR = mutate(HWR, Date = as.Date(Date))
test = left_join(HWR, dplyr::select(flowX, -Year, -Season)) %>%
  filter(Temperature >0, Temperature <30) %>%
  left_join(DF)




#start with this. I may also want month or day of year in here.
#Exports will be another interesting explanitory variable. I also need
#to figure out if I"m using the right distribution

#Note: All of these models take a long time to run. If you want to just get
#the results and look at them, use:
load("HABsbrimsresults.RData")

#Three terms and a random effect using all the data looks like its going to take days to run.
#####################

#let's simplify

SoDelta = dplyr::filter(test, Stratum2 %in% c("Lower SJ", "Lower Sac", "South Delta"))


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
SoDelta = mutate(SoDelta, day = yday(Date), Outscale = scale(OUT),
                 Exscale = scale(EXPORTS), SJRs = scale(SJR), Tempscale = scale(Temperature), Secchs = scale(Secchi)) %>%
  filter(!is.na(Outscale), !is.na(Tempscale), !is.na(SJRs), !is.na(Exscale), !is.na(Secchs))

M5.2 = brm(HABord ~ Tempscale + Outscale + Exscale + (1|Yearf/day), data = SoDelta, family = cumulative,
           iter = 2000,   backend = "cmdstanr", normalize = FALSE, 
           control = list(max_treedepth = 15),
           chains = 2, cores=4, threads = threading(2))
#Why did this one run so fast? 

summary(M5.2)
conditional_effects(M5.2, categorical = TRUE)
#hmmm.... this one is definitley telling me that outflow is a better predictor than exports.
#what the actual fuck. Now it's super fast. 
#was it the 'acat' instead of cumulative?
M5.3 = brm(HABord ~ Tempscale + Outscale + Exscale + (1|Yearf) + (1|day), data = SoDelta, family = cumulative,
           iter = 2000,   backend = "cmdstanr", normalize = FALSE, 
           control = list(max_treedepth = 15),
           chains = 2, cores=4, threads = threading(2))
# Nope. Not that. 
#Look at odds ratio. What does the cahnge in CFS change in probability of microcystis. 

#Adding SJR to the model made things get wierd, i think it's too autocorrelated with outflow
#but I should try turbidity/secchi


# M5.4 = brm(HABord ~ Tempscale + Outscale + SJRs+ (1|Yearf) + (1|day), data = SoDelta, family = cumulative,
#            iter = 2000,   backend = "cmdstanr", normalize = FALSE, 
#            control = list(max_treedepth = 15),
#            chains = 2, cores=4, threads = threading(2))

M5.41 = brm(HABord ~ Tempscale + Outscale + Secchs+ (1|Yearf) + (1|day), data = SoDelta, family = cumulative,
                      iter = 2000,   backend = "cmdstanr", normalize = FALSE, 
                       control = list(max_treedepth = 15),
                       chains = 2, cores=4, threads = threading(2))

M5.5 = brm(HABord ~ Tempscale + Outscale + Exscale+ Secchs + (1|Yearf) + (1|day), data = SoDelta, family = cumulative,
           iter = 2000,   backend = "cmdstanr", normalize = FALSE, 
           control = list(max_treedepth = 15),
           chains = 2, cores=4, threads = threading(2))


# M5.6 = brm(HABord ~ Tempscale +  Exscale+ SJRs + (1|Yearf) + (1|day), data = SoDelta, family = cumulative,
#            iter = 2000,   backend = "cmdstanr", normalize = FALSE, 
#            control = list(max_treedepth = 15),
#            chains = 2, cores=4, threads = threading(2))

M5.61 = brm(HABord ~ Tempscale +  Exscale+ Secchs + (1|Yearf) + (1|day), data = SoDelta, family = cumulative,
           iter = 2000,   backend = "cmdstanr", normalize = FALSE, 
           control = list(max_treedepth = 15),
           chains = 2, cores=4, threads = threading(2))

# M5.7 = brm(HABord ~   Exscale+ SJRs + (1|Yearf) + (1|day), data = SoDelta, family = cumulative,
#            iter = 2000,   backend = "cmdstanr", normalize = FALSE, 
#            control = list(max_treedepth = 15),
#            chains = 2, cores=4, threads = threading(2))

M5.71 = brm(HABord ~   Exscale+ Secchs + (1|Yearf) + (1|day), data = SoDelta, family = cumulative,
           iter = 2000,   backend = "cmdstanr", normalize = FALSE, 
           control = list(max_treedepth = 15),
           chains = 2, cores=4, threads = threading(2))


M5.8 = brm(HABord ~ Tempscale + (1|Yearf) + (1|day), data = SoDelta, family = cumulative,
           iter = 2000,   backend = "cmdstanr", normalize = FALSE, 
           control = list(max_treedepth = 15),
           chains = 2, cores=4, threads = threading(2))



# M5.9 = brm(HABord ~  SJRs + (1|Yearf) + (1|day), data = SoDelta, family = cumulative,
#            iter = 2000,   backend = "cmdstanr", normalize = FALSE, 
#            control = list(max_treedepth = 15),
#            chains = 2, cores=4, threads = threading(2))

M5.91 = brm(HABord ~  Secchs + (1|Yearf) + (1|day), data = SoDelta, family = cumulative,
                      iter = 2000,   backend = "cmdstanr", normalize = FALSE, 
                       control = list(max_treedepth = 15),
                       chains = 2, cores=4, threads = threading(2))
           


# M5.10 = brm(HABord ~  SJRs + Tempscale+ (1|Yearf) + (1|day), data = SoDelta, family = cumulative,
#            iter = 2000,   backend = "cmdstanr", normalize = FALSE, 
#            control = list(max_treedepth = 15),
#            chains = 2, cores=4, threads = threading(2))

M5.101 = brm(HABord ~  Secchs + Tempscale+ (1|Yearf) + (1|day), data = SoDelta, family = cumulative,
            iter = 2000,   backend = "cmdstanr", normalize = FALSE, 
            control = list(max_treedepth = 15),
            chains = 2, cores=4, threads = threading(2))

M5.11 = brm(HABord ~  Outscale+ (1|Yearf) + (1|day), data = SoDelta, family = cumulative,
            iter = 2000,   backend = "cmdstanr", normalize = FALSE, 
            control = list(max_treedepth = 15),
            chains = 2, cores=4, threads = threading(2))

M5.12 = brm(HABord ~  Exscale+ (1|Yearf) + (1|day), data = SoDelta, family = cumulative,
            iter = 2000,   backend = "cmdstanr", normalize = FALSE, 
            control = list(max_treedepth = 15),
            chains = 2, cores=4, threads = threading(2))

M5.13 = brm(HABord ~  Tempscale + Outscale+ (1|Yearf) + (1|day), data = SoDelta, family = cumulative,
            iter = 2000,   backend = "cmdstanr", normalize = FALSE, 
            control = list(max_treedepth = 15),
            chains = 2, cores=4, threads = threading(2))

M5.14 = brm(HABord ~  Secchs + Outscale+ (1|Yearf) + (1|day), data = SoDelta, family = cumulative,
            iter = 2000,   backend = "cmdstanr", normalize = FALSE, 
            control = list(max_treedepth = 15),
            chains = 2, cores=4, threads = threading(2))

M5.15 = brm(HABord ~  Exscale + Outscale+ (1|Yearf) + (1|day), data = SoDelta, family = cumulative,
            iter = 2000,   backend = "cmdstanr", normalize = FALSE, 
            control = list(max_treedepth = 15),
            chains = 2, cores=4, threads = threading(2))

M5.16 = brm(HABord ~  Exscale + Outscale+ Secchs + (1|Yearf) + (1|day), data = SoDelta, family = cumulative,
            iter = 2000,   backend = "cmdstanr", normalize = FALSE, 
            control = list(max_treedepth = 15),
            chains = 2, cores=4, threads = threading(2))

save.image()

summary(M5.4)
M5.41 = add_criterion(M5.41, "loo")
M5.41 = add_criterion(M5.41, "waic")
M5.3 = add_criterion(M5.3, "loo")
M5.3 = add_criterion(M5.3, "waic")
M5.5 = add_criterion(M5.5, "loo")
M5.5 = add_criterion(M5.5, "waic")
M5.61 = add_criterion(M5.61, "loo")
M5.61 = add_criterion(M5.61, "waic")
M5.71 = add_criterion(M5.71, "loo")
M5.71 = add_criterion(M5.71, "waic")
M5.8 = add_criterion(M5.8, "loo")
M5.8 = add_criterion(M5.8, "waic")
M5.91 = add_criterion(M5.91, "loo")
M5.91 = add_criterion(M5.91, "waic")
M5.101 = add_criterion(M5.101, "loo")
M5.101 = add_criterion(M5.101, "waic")
M5.11 = add_criterion(M5.11, "loo")
M5.11 = add_criterion(M5.11, "waic")
M5.12 = add_criterion(M5.12, "waic")
M5.13 = add_criterion(M5.13, "waic")
M5.14 = add_criterion(M5.14, "waic")
M5.15= add_criterion(M5.15, "waic")
M5.16= add_criterion(M5.16, "waic")

cex = conditional_effects(M5.41, categorical = TRUE)
pp_check(M5.41)

hypothesis(M5.4, c("Tempscale > Outscale", "Outscale > SJRs", "SJRs = 0"))

test = waic(M5.41, M5.3, M5.5, M5.61, M5.71, M5.8, M5.91, M5.101, M5.11)

test = loo_compare(M5.41, M5.3,  M5.5, M5.61, M5.71, M5.8, M5.91, M5.101, M5.11, criterion = "loo")
test = loo_compare(M5.41, M5.3, M5.5, M5.61,  M5.71, M5.8, M5.91, M5.101, M5.11, 
                   M5.12, M5.13, M5.15, M5.14, M5.16, criterion = "waic")
write.csv(test, "WAICscores.csv")

pp_check(M5.5)
cex5.5 = conditional_effects(M5.5, categorical = TRUE)
cex5.5

pp_check(M5.61)
cex5.61 = conditional_effects(M5.61, categorical = TRUE)
cex5.61

pp_check(M5.4)
cex5.4= conditional_effects(M5.4, categorical = TRUE)
cex5.4a= conditional_effects(M5.4, effects = "Outscale", conditions = data.frame(SJRs = c(-1,0,1,2), Tempscale = c(0,0,0,0)),categorical = TRUE)
cex5.4


save(M5.41, M5.3, M5.5, M5.61,M5.71, M5.8, M5.91, M5.101, M5.11, M5.12, M5.13, M5.14, M5.15, M5.16, file = "MCmodels30mar2022")

ggplot(SoDelta, aes(x = day, y = Export, color = Yearf)) + geom_point()
ggplot(SoDelta, aes(x = day, y = Outflow, color = Yearf)) + geom_point()
ggplot(SoDelta, aes(x =Export, y = Outflow, color = Yearf)) + geom_point()
ggplot(SoDelta, aes(x =Outflow, y = Export, color = Yearf)) + geom_point()
ggplot(SoDelta, aes(x =Outflow, y = OUT, color = Yearf)) + geom_point()
ggplot(SoDelta, aes(x =Export, y = EXPORTS, color = Yearf)) + geom_point()
ggplot(SoDelta, aes(x =EXPORTS, y = SJR, color = Yearf)) + geom_point()
ggplot(SoDelta, aes(x =OUT, y = SJR, color = Yearf)) + geom_point()+
coord_cartesian(xlim = c(0, 20000))


ggplot(SoDelta, aes(x = day, y = Temperature, color = Yearf)) + geom_point()+
  ylab("Water Temperature, degrees C") + xlab("Day of the Year")
ggplot(SoDelta, aes(x = Temperature, y = Outflow, color = Yearf)) + geom_point()
ggplot(SoDelta, aes(x = Temperature, y = Export, color = Yearf)) + geom_point()

summary(M5.3)
pp_check(M5.3)
ce = conditional_effects(M5.3, categorical = TRUE)

#I should write a function for thsi. 
#plot temperature effect
  temp = cex5.61$`Tempscale:cats__`
  lm = lm(Temperature ~Tempscale, data = SoDelta)
  foo = as.data.frame(summary(lm)$coefficients)
  
  newdata = data.frame(Tempscale = c(-2,0,2))
  newdata = mutate(temp, Temperature = Tempscale*foo$Estimate[2] + foo$Estimate[1])
  
  
  ggplot(newdata, aes(x = Temperature, y = estimate__)) +
    geom_ribbon(aes(ymin = lower__, ymax = upper__, fill = cats__), alpha = 0.3)+
    geom_line(aes(color = cats__))+
    scale_fill_manual(values = c("blue", "orange", "red"), 
                      labels = c("Absent", "Low", "High"), name = "Microcystis")+
    scale_color_manual(values = c("blue", "orange", "red"), 
                       labels = c("Absent", "Low", "High"), name = "Microcystis")+
    xlab("Temperature")+
    ylab("Probability")+
    theme_bw()
   ggsave("MicTemp.tiff", device = "tiff", width = 6, height = 4, units = "in") 
   
   
   #now outflow
  # outs = cex5.4$`Outscale:cats__`
  # lmO = lm(OUT ~Outscale, data = SoDelta)
  # foo = as.data.frame(summary(lmO)$coefficients)
  # newdataO = mutate(outs, Outflow = Outscale*foo$Estimate[2] + foo$Estimate[1])
  # 
  # 
  # 
  # ggplot(newdataO, aes(x = Outflow, y = estimate__)) +
  #   geom_ribbon(aes(ymin = lower__, ymax = upper__, fill = cats__), alpha = 0.3)+
  #   geom_line(aes(color = cats__))+
  # #  scale_fill_manual(values = c("blue", "orange", "red"), 
  # #                    labels = c("Absent", "Low", "High"), name = "Microcystis")+
  #  # scale_color_manual(values = c("blue", "orange", "red"), 
  #   #                   labels = c("Absent", "Low", "High"), name = "Microcystis")+
  #   xlab("Delta Outflow (cfs)")+
  #   ylab("Probability")+
  #  geom_vline(xintercept = 3000, color = "black", linetype = 2)+
  #   geom_vline(xintercept = 4000, color = "black")+
  #   annotate("text", x = 2300, y = 0.4, label = "TUCP Outflow", angle = 90)+
  #   annotate("text", x = 4500, y = 0.4, label = "D-1641 Outflow", angle = 90)+
  #   theme_bw()
  # 
  # ggsave("MicOutflow.tiff", device = "tiff", width = 6, height = 4, units = "in")
  
  #Check baseline conditions from the origional TUCP. 
  
  ex = cex5.61$`Exscale:cats__`
  lmE = lm(Export ~Exscale, data = SoDelta)
  fooE = as.data.frame(summary(lmE)$coefficients)
  newdataE = mutate(ex, Exports = Exscale*fooE$Estimate[2] + fooE$Estimate[1])
  
  
  ggplot(newdataE, aes(x = Exports, y = estimate__)) +
    geom_ribbon(aes(ymin = lower__, ymax = upper__, fill = cats__), alpha = 0.3)+
    geom_line(aes(color = cats__))+
    scale_fill_manual(values = c("blue", "orange", "red"), 
                      labels = c("Absent", "Low", "High"), name = "Microcystis")+
    scale_color_manual(values = c("blue", "orange", "red"), 
                       labels = c("Absent", "Low", "High"), name = "Microcystis")+
    xlab("Project Exports (cfs)")+
    ylab("Probability")+
    geom_vline(xintercept = 1500, linetype = 2)+
    annotate("text", x = 1300, y = 0.4, label = "TUCP Export Limit", angle = 90)+
    theme_bw()
  ggsave("MicExports.tiff", device = "tiff", width = 6, height = 4, units = "in")


#definitley a lower outflow effect and a bigger export effect when you take day of year into account
#save(M5.3, M5.2, M5.1, file = "HABsbrimsresults.RData")
  
  
  
  turb = cex5.61$`Secchs:cats__`
  lmS = lm(Secchi ~Secchs, data = SoDelta)
  fooS = as.data.frame(summary(lmS)$coefficients)
  newdataS = mutate(turb, Secchi = Secchs*fooS$Estimate[2] + fooS$Estimate[1])
  
  
  ggplot(newdataS, aes(x = Secchi, y = estimate__)) +
    geom_ribbon(aes(ymin = lower__, ymax = upper__, fill = cats__), alpha = 0.3)+
    geom_line(aes(color = cats__))+
    scale_fill_manual(values = c("blue", "orange", "red"), 
                      labels = c("Absent", "Low", "High"), name = "Microcystis")+
    scale_color_manual(values = c("blue", "orange", "red"), 
                       labels = c("Absent", "Low", "High"), name = "Microcystis")+
    xlab("Secchi Depth (cm)")+
    ylab("Probability")+
    theme_bw()
  ggsave("MicSecchi.tiff", device = "tiff", width = 6, height = 4, units = "in")
  


#compare probability of HABs for 2020 (no TUCP, dry) to 2021
SoDeltasum = group_by(SoDelta, Year, Yearf, Month2) %>%
  summarize(Exscale = mean(Exscale), Outscale = mean(Outscale), Export = mean(Export), 
            Outflow = mean(Outflow), Tempscale = mean(Tempscale), 
            Secchs = mean(Secchs),
            Temperature = mean(Temperature), day = median(day)) %>%
  filter(Yearf %in% c("2021", "2020")) %>%
  droplevels()


newdata2 = data.frame(Exscale = filter(SoDeltasum, Yearf == '2021')$Exscale, 
                      Outscale = c(-.47, -.47, -.569157, -.586204), 
                      Tempscale = filter(SoDeltasum, Yearf == '2021')$Tempscale,
                      Secchs = filter(SoDeltasum, Yearf == '2021')$Secchs,
                      day = c(165,190, 221, 252), Yearf = "2021", Scenario = "NoTUCP")

newdata3 = data.frame(Exscale = filter(SoDeltasum, Yearf == '2021')$Exscale, 
                      Outscale = filter(SoDeltasum, Yearf == '2021')$Outscale, 
                      Tempscale = filter(SoDeltasum, Yearf == '2021')$Tempscale,
                      Secchs = filter(SoDeltasum, Yearf == '2021')$Secchs,
                      day = c(165,190, 221, 252), Yearf = "2021", Scenario = "TUCP")
allnew = bind_rows(newdata2, newdata3)

prede = predict(M5.61, newdata = SoDeltasum)
SoDeltasum = bind_cols(SoDeltasum, prede)
ggplot(SoDeltasum, aes(x = Outscale, y = `P(Y = High)`)) + geom_point()
ggplot(SoDeltasum, aes(x = Exscale, y = `P(Y = High)`)) + geom_point()
ggplot(SoDeltasum, aes(x = Tempscale, y = `P(Y = High)`)) + geom_point()

prede = as.data.frame(predict(M5.3, newdata = allnew, allow_new_levels = TRUE)) %>%
#  mutate(Scenario = c(rep("NoTUCP", 5), rep("TUCP", 5))) %>%
  bind_cols(allnew)

Predictions = prede %>%
  pivot_longer(cols = c("P(Y = absent)","P(Y = Low)","P(Y = High)"), names_to = "HABs", values_to = "Probability")

ggplot(Predictions, aes(x = Scenario, y = Probability, fill = HABs)) + geom_col(position = "dodge") +
  facet_wrap(~day)

ggplot(filter(Predictions, day < 225),  aes(x = as.factor(day), y = Probability, fill = Scenario)) + 
         geom_col(position = "dodge") +
  facet_wrap(~HABs)+
  scale_x_discrete(labels = c("June", "July", "August"))

diff = group_by(Predictions, HABs, day) %>%
  summarize(Difference = Probability[1]-Probability[2])

#What is a "normal" level of exports for the summer?

Exes = filter(SoDelta, Yearf %in% c("2016", "2018", "2020")) %>%
  group_by(Month2) %>%
           summarize(Exports = mean(Export), Exscale = mean(Exscale))

newdata2 = data.frame(Exscale = Exes$Exscale, Outscale = c(-.47, -.47, -.569157, -.586204, -.5245), 
                      Tempscale = filter(SoDeltasum, Yearf == '2021')$Tempscale,
                      day = c(165,190, 221, 252, 293), Yearf = "2021")

#Eh. I really don't know what a good point of comparison would be here.
#maybe start with just Outflow?
newdata2 = data.frame(Exscale =  filter(SoDeltasum, Yearf == '2021')$Exscale, Outscale = c(-.47, -.47,-.569157, -.586204, -.5245), 
                      Tempscale = filter(SoDeltasum, Yearf == '2021')$Tempscale,
                      day = c(165,190, 221, 252, 293), Yearf = "2021")

#OK, so a lot hinges on exports. 

#Instead of "With and without TUCP" I"m just going to say "With an outflow of 3,000 CFS versus 4,000 CFS



#San JOaquin flow may also be important. Exports from New Melones were big.

M5.4 = brm(HABord ~ Tempscale + Outscale + Exscale + Yearf +Stratum2+ (1|day), data = SoDelta, family = cumulative,
           iter = 2000,   backend = "cmdstanr", normalize = FALSE, 
           control = list(max_treedepth = 15),
           chains = 2, cores=4, threads = threading(2))

summary(M5.4)
ce2 = conditional_effects(M5.4, categorical = TRUE)
ce2

#Exports of 1500 CFS = -1.18 exscale
summary(lmE)
(1500-5808)/3644
(2500-5808)/3644
summary(lmO)
(3000-7460)/6893
(4000-7460)/6893

#OK! So, if we hold the Exports constant at 1500, what's the difference between 3,000 and 4000 CFS outflow?



newdata2 = data.frame(Exscale = rep(-1.18, 5), Outscale = rep(-.502, 5), 
                      Tempscale = filter(SoDeltasum, Yearf == '2021')$Tempscale,
                      Secchs = filter(SoDeltasum, Yearf == '2021')$Secchs,
                      day = c(165,190, 220, 252, 293), Yearf = "2021", Scenario = "4000 CFS")

newdata3 = data.frame(Exscale = rep(-1.18, 5), Outscale = rep(-.647, 5), 
                      Tempscale = filter(SoDeltasum, Yearf == '2021')$Tempscale,
                      Secchs = filter(SoDeltasum, Yearf == '2021')$Secchs,
                      day = c(165,190, 220, 252, 293), Yearf = "2021", Scenario = "3000 CFS")
allnew = bind_rows(newdata2, newdata3)


prede = as.data.frame(predict(M5.3, newdata = allnew)) %>%
  bind_cols(allnew)

Predictions = prede %>%
  pivot_longer(cols = c("P(Y = absent)","P(Y = Low)","P(Y = High)"), names_to = "HABs", values_to = "Probability") %>%
  mutate(HABs = factor(HABs, levels = c("P(Y = absent)","P(Y = Low)","P(Y = High)"), labels = c("Absent", "Low", "High")))


ggplot(filter(Predictions, day < 225),  aes(x = as.factor(day), y = Probability, fill = Scenario)) + 
  geom_col(position = "dodge") +
  facet_wrap(~HABs)+
  scale_fill_discrete(name = "Outflow Scenario")+
  scale_x_discrete(labels = c("June", "July", "August"), name = "Month")+
  theme_bw()

diff = group_by(Predictions, HABs, day) %>%
  summarize(Difference = Probability[1]-Probability[2])


#Now let's do the same thing with exports. Try 1500 CFS versus 3000 CFS


newdata2e = data.frame(Exscale = rep(-1.18, 5), Outscale = rep(-.647, 5), 
                      Tempscale = filter(SoDeltasum, Yearf == '2021')$Tempscale,
                      Secchs = filter(SoDeltasum, Yearf == '2021')$Secchs,
                      day = c(165,190, 220, 252, 293), Yearf = "2021", Scenario = "1500 CFS")

newdata3e = data.frame(Exscale = rep(-.908, 5), Outscale = rep(-.647, 5), 
                      Tempscale = filter(SoDeltasum, Yearf == '2021')$Tempscale,
                      Secchs = filter(SoDeltasum, Yearf == '2021')$Secchs,
                      day = c(165,190, 220, 252, 293), Yearf = "2021", Scenario = "2500 CFS")
allnewe = bind_rows(newdata2e, newdata3e)


prede = as.data.frame(predict(M5.3, newdata = allnewe)) %>%
  bind_cols(allnewe)

Predictionse = prede %>%
  pivot_longer(cols = c("P(Y = absent)","P(Y = Low)","P(Y = High)"), names_to = "HABs", values_to = "Probability") %>%
  mutate(HABs = factor(HABs, levels = c("P(Y = absent)","P(Y = Low)","P(Y = High)"), labels = c("Absent", "Low", "High")))


ggplot(filter(Predictionse, day < 225),  aes(x = as.factor(day), y = Probability, fill = Scenario)) + 
  geom_col(position = "dodge") +
  facet_wrap(~HABs)+
  scale_fill_manual(values = c("grey", "darkgreen"), name = "Export Scenario")+
  scale_x_discrete(labels = c("June", "July", "August"), name = "Month")+
  theme_bw()

diffe = group_by(Predictionse, HABs, day) %>%
  summarize(Difference = Probability[1]-Probability[2])

##########################################################################
#fhab portal

fhab_bloomreport_portal <- read_csv("data/HABs/fhab_bloomreport_portal.csv")

fhabdelta = 