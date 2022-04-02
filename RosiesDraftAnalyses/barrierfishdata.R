###############################################################################
#Barrier analysis for FISH
#Summer Townet from July and August is probably the best dataset
#to start with because it's got good temporal and spatial coverage and
#was in place in both barrier years

#I should also look into salvage data.
library(tidyverse)
require(sf)
require(lubridate)
require(hms)
require(tidyr)
require(purrr)
require(rlang)
require(readr)
require(dtplyr) # To speed things up
require(ggplot2) # Plotting
require(geofacet) # plotting
library(deltamapr)

library(emmeans)
library(lme4)
library(lmerTest)
library(glmmTMB)
library(performance)
library(multcomp)
library(multcompView)
library(DHARMa)
library(brms)
library(pscl)
library(readxl)
library(RColorBrewer)
mypal = c(brewer.pal(8, "Dark2"), brewer.pal(8, "Set2"), brewer.pal(12, "Set3"), "black", "grey")
yeartypes = read_csv("data/yearassignments.csv")


#Grab the latest TNS datat from the webs
TNURL = "https://filelib.wildlife.ca.gov/Public/TownetFallMidwaterTrawl/TNS%20MS%20Access%20Data/TNS%20data/CatchPerStation1959-2021.csv"
Townet = read_csv(TNURL) %>%
  mutate(StationCode = as.character(`Station Code`))
 # rename(SampDate = `Sample Date`, VolumeOfAllTows = `Volume of All Tows`)

#station GPS
stations = read_csv("data/AllIEPstations_20200220.csv") %>%
  filter(Survey == "TNS")

#upload regional mpas
regions = st_read("C:/Users/rhartman/OneDrive - California Department of Water Resources/Drought/Barrier/BarrierRegions/shpExport.shp")
waterways = WW_Delta
regions = st_transform(regions, crs = st_crs(waterways)) %>%
  st_make_valid()

stas = st_as_sf(stations, coords = c("Longitude","Latitude"), crs = st_crs(waterways))

#make a quick plot so we know we did it right
ggplot() +
  geom_sf(data = regions, aes(fill = Regions)) +
  geom_sf(data = waterways)+geom_sf(data = stas) 

Townet = rename(Townet, StationCode = `Station Code`)

#join regions to stations to get which station is in which region
stas1 = st_join(stas, regions, join=st_intersects)%>% # Add subregions
  filter(!is.na(Regions))%>% # Remove any data outside our regions of interest
  st_drop_geometry() %>% # Drop sf geometry column since it's no longer needed
  dplyr::select(StationCode, Regions)
  
#get rid of things we don't need
TNS2 = left_join(stas1, Townet)  %>%
  dplyr::select(-`Tows Completed`,-`Temperature Top`, -`Temperature Bottom`,          
                -`Conductivity Top`, -`Conductivity Bottom`, -`Tide Code`,                   
                -`Depth Bottom`, -`Cable Out`, -`Tow Direction`,               
                -`Wind Direction`,-`Turbidity Top`, -StartLatDegrees,            
                -StartLatMinutes, -StartLatSeconds, -StartLongDegrees,           
                -StartLongMinutes, -StartLongSeconds, -EndLatDegrees,              
                -EndLatMinutes, -EndLatSeconds, -EndLongDegrees,             
                -EndLongMinutes, -EndLongSeconds) %>%
  rename(SampDate = `Sample Date`, VolumeOfAllTows = `Total Tow Volume`)
  

#Now just data from 2012 to present. From wide to long

TNSd = filter(TNS2, Year >2013) %>%
   pivot_longer(cols = `American Shad`:last_col(), names_to = "Species", values_to = "Catch") %>%
  mutate(CPUE = Catch/VolumeOfAllTows, CPUE = case_when(is.na(CPUE) ~ 0,
                                                        TRUE ~ CPUE)) 

#Calculate which species make up less than 5% of the total catch and drop them.
totcatch = sum(TNSd$Catch)
species = group_by(TNSd, Species) %>%
  summarize(tot = sum(Catch, na.rm = T), Perc = tot/totcatch)

TNSd = filter(TNSd, !Species %in% filter(species, tot == 0)$Species) %>%
  mutate(Species = case_when(
    Species %in% filter(species, tot == 1)$Species ~ "Other",
    Species %in% c("Unknown Damaged (UNID)", "Unknown Fish (UNID)", "Unknown Invertebrate (UNID)", "Trid_SB0 (UNID)") ~ "Unknown",
    TRUE ~ Species
  )) %>%
  mutate(Species = str_replace(Species, "(UNID)",""), Species = str_replace(Species, "(Unid)",""), Species = str_replace(Species, "()","")) %>%
  group_by(StationCode, Regions, Year, Survey, SampDate, Species, VolumeOfAllTows) %>%
  summarize(Catch = sum(Catch), CPUE = sum(CPUE))


write.csv(TNSd, "BarrierTNS.csv")

#what's going on with the gobies?
TNSd %>%
  filter(Species == "Tridentiger spp")%>%
ggplot(aes(x = Regions, y = log(CPUE+1))) +
  geom_boxplot() 


#check out prawns
TNSd %>%
  filter(Species == "Siberian prawn")%>%
  ggplot(aes(x = Regions, y = CPUE)) +
  geom_boxplot() + facet_wrap(~Year) +
  coord_cartesian(ylim = c(0, 0.005))

#now let's look at total catch of everything
TNsum = group_by(TNSd, Year, Regions, StationCode, VolumeOfAllTows) %>%
  summarize(CPUE = sum(CPUE), Catch = sum(Catch))

ggplot(TNsum, aes(x = Regions, y= Catch)) + geom_boxplot()

#average total catch by year and region
TNmean = group_by(TNsum, Year, Regions) %>%
  summarize(CPUE = mean(Catch, na.rm = T), SD = sd(Catch, na.rm = T), SE = SD/sqrt(n())) %>%
  left_join(yeartypes)
  

########PLOT FOR REPORT
ggplot(TNmean, aes(x = Regions, y= CPUE, group = Year)) + geom_col(aes(fill = ShortTerm))+
  geom_errorbar(aes(ymin = CPUE-SE, ymax = CPUE+SE))+
  facet_wrap(~Year)+ylab("Mean total Fish/1000m3")+
  scale_fill_manual(name = "Year Type", values = c("darkorange", "tan", "skyblue"))+
  theme_bw()


#Means by species

TNmeanS = group_by(TNSd, Year, Regions, Species) %>%
  summarize(CPUE = mean(Catch, na.rm = T), SD = sd(Catch, na.rm = T), SE = SD/sqrt(n())) %>%
  left_join(yeartypes)

ggplot(TNmeanS, aes(x = Regions, y= CPUE, group = Year)) + geom_col(aes(fill = Species))+
  facet_wrap(~Year)+ylab("Mean total Fish/1000m3")+
  scale_fill_manual(values = c(mypal, "red", "green"))+
  theme_bw()+
  theme(legend.position = "bottom")


#Huh, i guess the increased catch was driven mainly by Maeoteus. Probalby take the invebrates out. 
######################################################################################################
#fish only
TNfishonly = filter(TNSd, !Species %in% c("Unknown Invertebrate (UNID)", "Palaemon", "Shrimp (UNID)", "Siberian prawn", "Maeotias", 
                                          "Blackfordia virginica", "Crangon"))


TNsumfo = group_by(TNfishonly, Year, Regions, StationCode, VolumeOfAllTows) %>%
  summarize(CPUE = sum(CPUE), Catch = sum(Catch))

ggplot(TNsumfo, aes(x = Regions, y= Catch)) + geom_boxplot()

TNmeanfo = group_by(TNsumfo, Year, Regions) %>%
  summarize(CPUE = mean(Catch, na.rm = T), SD = sd(Catch, na.rm = T), SE = SD/sqrt(n())) %>%
  left_join(yeartypes)


########PLOT FOR REPORT
ggplot(TNmeanfo, aes(x = Regions, y= CPUE, group = Year)) + geom_col(aes(fill = ShortTerm))+
  geom_errorbar(aes(ymin = CPUE-SE, ymax = CPUE+SE))+
  facet_wrap(~Year)+ylab("Mean total Fish/1000m3")+ theme_bw()


TNmeanSf = group_by(TNfishonly, Year, Regions, Species) %>%
  summarize(CPUE = mean(Catch, na.rm = T), SD = sd(Catch, na.rm = T), SE = SD/sqrt(n())) %>%
  left_join(yeartypes)

ggplot(TNmeanSf, aes(x = Regions, y= CPUE, group = Year)) + geom_col(aes(fill = Species))+
  facet_wrap(~Year)+ylab("Mean total Fish/1000m3")+
  scale_fill_manual(values = c(mypal, "red", "green"))

#Actually, that looks pretty similar tot the version with the inverts

####################################################################
#inverts only
TNshrimp = filter(TNSd, Species %in% c("Unknown Invertebrate (UNID)", "Palaemon", "Shrimp (UNID)", "Siberian prawn", "Maeotias", 
                                          "Blackfordia virginica", "Crangon"))


TNsumi = group_by(TNshrimp, Year, Regions, StationCode, VolumeOfAllTows) %>%
  summarize(CPUE = sum(CPUE), Catch = sum(Catch))

ggplot(TNsumi, aes(x = Regions, y= Catch)) + geom_boxplot()

TNmeani = group_by(TNsumi, Year, Regions) %>%
  summarize(CPUE = mean(Catch, na.rm = T), SD = sd(Catch, na.rm = T), SE = SD/sqrt(n())) %>%
  left_join(yeartypes)


########PLOT FOR REPORT
ggplot(TNmeani, aes(x = Regions, y= CPUE, group = Year)) + geom_col(aes(fill = ShortTerm))+
  geom_errorbar(aes(ymin = CPUE-SE, ymax = CPUE+SE))+
  facet_wrap(~Year)+ylab("Mean total Fish/1000m3")


TNmeanSi = group_by(TNshrimp, Year, Regions, Species) %>%
  summarize(CPUE = mean(Catch, na.rm = T), SD = sd(Catch, na.rm = T), SE = SD/sqrt(n())) %>%
  left_join(yeartypes)

ggplot(TNmeanSi, aes(x = Regions, y= CPUE, group = Year)) + geom_col(aes(fill = Species))+
  facet_wrap(~Year)+ylab("Mean total Fish/1000m3")+
  scale_fill_manual(values = c(mypal, "red", "green"))
#############################################################################################
#now let's try some models. 
#I tried using glmmTMB and some other types of models, and they really don't like the 
#low catch in the South Delta. THings kept breaking

hist(TNsum$Catch, breaks = 50)
hist(log(TNsum$Catch+1), breaks = 50)

  c3 = brm(Catch ~ Regions*yearf, family = zero_inflated_negbinomial(), data = TNsum, iter = 2000, chains = 4)
  
c3
plot(c3)

#with station as a random effect
c3 = brm(Catch ~ Regions*yearf+ (1|StationCode), family = zero_inflated_negbinomial(), 
         data = TNsum, iter = 2000, chains = 4)

c3s = summary(c3)
plot(c3)
conditional_effects(c3)

#export results table
write.csv(c3s$fixed, "TownNetModel.csv")

#I need to check for salmon, smelt, sturgeon
  

#pivot and add in zeros
TNSs = filter(TNS2, Year >2013) %>%
  pivot_longer(cols = `Age-0 Striped Bass`:last_col(), names_to = "Species", values_to = "Catch") %>%
  mutate(CPUE = Catch/VolumeOfAllTows, CPUE = case_when(is.na(CPUE) ~ 0,
                                                        TRUE ~ CPUE)) 
TNSS = filter(TNSs, Species %in% c("Chinook Salmon", "Delta Smelt",
                                   "Longfin Smelt", "Green Sturgeon", "Steelhead"))  %>%
  droplevels()

#plot special status species
ggplot(TNSS, aes(x = Year, y = CPUE))+ geom_point()+
  facet_grid(Species~Regions)


TNmeanSS = group_by(TNSS, Year, Regions, Species) %>%
  summarize(CPUE = mean(Catch, na.rm = T), SD = sd(Catch, na.rm = T), SE = SD/sqrt(n())) %>%
  left_join(yeartypes)

ggplot(TNmeanSS, aes(x = Regions, y= CPUE)) + geom_col(aes(fill = Species), position = "dodge")+
  facet_wrap(~Year)+ylab("Mean total Fish/1000m3")+
  scale_fill_manual(values = c(mypal, "red", "green"))+
  theme_bw()+
  theme(legend.position = "bottom")

#No listed fish in the central Delta region. A few longfin in the sacramento region
###########################################################
#I really want to do an NMDS on this

library(vegan)

TNSd2 = filter(TNSd, Year %in% c(2014, 2015, 2017, 2019, 2020, 2021))

TNMat = pivot_wider(TNSd2, id_cols = c(StationCode, Regions, Year, Survey),
                    names_from = Species, values_from = Catch)  %>%
  mutate(total = sum(`American Shad`:`Yellowfin Goby`),
         Regions = as.factor(Regions), Year = as.factor(Year)) %>%
  filter(total!= 0)
TNMat2 = dplyr::select(ungroup(TNMat), `American Shad`:`Yellowfin Goby`)
TNMat3 = TNMat2/rowSums(TNMat2)

source("PlotNMDS.R")
FNMDS = metaMDS(TNMat2, k=3, trymax = 500)
PlotNMDS(FNMDS, data = TNMat, group = "Year")
PlotNMDS(FNMDS, data = TNMat, group = "Regions")

#CPUE
adonis(TNMat2~ as.factor(Year)*Regions, data = TNMat)
adonis(TNMat2~ as.factor(Year)+Regions, data = TNMat)

#relative abundance
adonis(TNMat3~ as.factor(Year)*Regions, data = TNMat)
adonis(TNMat3~ as.factor(Year)+Regions, data = TNMat)

#so only a very, very small perportion of hte variance. 

#one hypothesis ws there might be an increase in black bas and centrarchids sin the central delta

Centr = filter(TNSd2, Species %in% c("Bluegill", "Centrarchids (Unid)", "Largemouth Bass")) %>%
  group_by(Year, Survey, Station, Regions) %>%
  summarize(CPUE = sum(CPUE), Catch = sum(Catch))
ggplot(Centr, aes(x = Regions, y = Catch)) +
  geom_boxplot() + facet_wrap(~Year)
#soo, no. Maybe use the DJFMP beach seines instead?


############################################################
STN = read_csv("data/HABs/STNCatchPerStation2021.csv")
STN = mutate(STN, Date = mdy(SampDate)) %>%
  pivot_longer(cols = "<>":"Yellowfin Goby", names_to = "Species", values_to = "Catch") %>%
  filter(!is.na(Catch)) %>%
  mutate(Survey2 = factor(Survey, labels = c("June 1", "June 15", "Jul 1", "Jul 15", "Aug 1", "Aug 15")),
         Species2 = case_when(Species %in% c("Bay Goby", "Chameleon Goby", "Cheekspot Goby", 
                                              "Gobies (Unid)", "Jacksmelt", "Largemouth Bass",
                                              "Lepomis (UNID)", "Pacific Lamprey", "Prickly Sculpin",
                                              "Staghorn Sculpin", "Starry Flounder", "Tule Perch") ~ "Other",
                               TRUE ~ Species))
  
  
ggplot(STN, aes(x = Survey2, y = Catch)) + geom_col(aes(fill = Species2)) + 
  facet_wrap(~Species2, scales = "free_y")+
  scale_x_discrete(name = NULL) +
  scale_fill_discrete(guide = NULL)+
  theme(axis.text.x = element_text(angle = 90))


##############################################################
#DJFMP = read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.244.8&entityid=147fd5e2c7db15913b2ffa44410dc7f9")
#DJFMPstas = read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.244.7&entityid=99a038d691f27cd306ff93fdcbc03b77")

DJFMP = read_csv("data/1976-2021_DJFMP_beach_seine_fish_and_water_quality_data.csv")
DJFMPstas = read_csv("data/DJFMP_site_locations.csv")


stasD = st_as_sf(DJFMPstas, coords = c("Longitude","Latitude"), crs = st_crs(waterways))

#make a quick plot so we know we did it right
ggplot() +
  geom_sf(data = regions, aes(fill = Regions)) +
  geom_sf(data = waterways)+geom_sf(data = stasD) 

#join regions to stations to get which station is in which region
stas1D = st_join(stasD, regions, join=st_intersects)%>% # Add subregions
  filter(!is.na(Regions))%>% # Remove any data outside our regions of interest
  st_drop_geometry() %>% # Drop sf geometry column since it's no longer needed
  dplyr::select(StationCode, Regions)

DJFMP = left_join(DJFMP, stas1D) %>%
  filter(!is.na(Regions), GearConditionCode ==1) %>%
  mutate( Year = year(SampleDate), Month = month(SampleDate)) %>%
  filter(Year > 2013, Month %in% c(5,6,7,8,9))

#Put in the zeros
DJFMP2 = pivot_wider(DJFMP, id_cols = c(Regions, StationCode, Month, Year, SampleDate, Volume), 
                     names_from = "CommonName", values_from = Count, values_fn = sum, values_fill = 0) %>%
  pivot_longer(cols = `largemouth bass`:last_col(), values_to = "Count", names_to = "CommonName")

write.csv(DJFMP2, "BarriersDJFMP.csv")

ggplot(DJFMP, aes(x= Regions, y = Count, fill = CommonName)) + geom_boxplot() + 
  facet_wrap(~CommonName, scales = "free_y") + scale_fill_discrete(guide = NULL)

DJFMPsum = group_by(DJFMP2, Regions, StationCode, Month, Year, SampleDate, Volume) %>%
  summarize(Catch = sum(Count)) %>%
  mutate(CPUE = Catch/Volume)


ggplot(DJFMPsum, aes(x= Regions, y = Catch)) + geom_boxplot() +
  facet_wrap(~Year)


ggplot(DJFMPsum, aes(x= Catch)) + geom_histogram()+facet_wrap(~Regions)



DJmean = group_by(DJFMPsum, Year, Regions) %>%
  summarize(CPUE = mean(Catch, na.rm = T), SD = sd(Catch, na.rm = T), SE = SD/sqrt(n()))

ggplot(DJmean, aes(x = Regions, y= CPUE, group = Year)) + geom_col()+
  geom_errorbar(aes(ymin = CPUE-SE, ymax = CPUE+SE))+
  facet_wrap(~Year)

#lump the rare species in "other"
DJspecies = group_by(DJFMP2, CommonName) %>%
  summarize(count = sum(Count), percent = count/sum(DJFMP2$Count, na.rm = T))
DJrare = DJspecies$CommonName[which(DJspecies$count<100)]

DJmean2 =  group_by(DJFMP2, Year, Regions, CommonName) %>%
  summarize(CPUE = mean(Count, na.rm = T))

DJmean2.1 = DJFMP2 %>%
  mutate(CommonName2 = case_when(
    CommonName %in% DJrare ~ "Other",
    TRUE ~ CommonName
  )) %>%
  group_by(Year, Regions, CommonName2) %>%
  summarize(CPUE = mean(Count, na.rm = T))

ggplot(DJmean2.1, aes(x = Regions, y= CPUE, fill = CommonName2)) + 
  geom_col()+
 facet_wrap(~Year)+
  scale_fill_manual(values = mypal, name = "Species")+
  theme_bw()+
  theme(legend.position = "bottom")
  


#special status species

DJS = filter(DJFMP2, CommonName %in% c("Chinook salmon", "Delta smelt",
                                   "longfin smelt", "Green Sturgeon", "Steelhead"))  %>%
  droplevels()

DJS2 = filter(DJmean2, CommonName %in% c("Chinook salmon", "Delta smelt",
                                       "longfin smelt", "Green Sturgeon", "Steelhead"))  %>%
  droplevels()

ggplot(DJS, aes(x = Year, y = Count))+ geom_point()+
  facet_grid(CommonName~Regions)+ coord_cartesian(ylim = c(0,20))

ggplot(DJS2, aes(x = Regions, y = CPUE, fill = CommonName)) + 
  geom_col(position = "dodge")+
  facet_wrap(~Year)+
  theme_bw()+
  theme(legend.position = "bottom")
#A few Chinook in teh San Joaquin and Sacramento The only
#listed fish in 2021 was a chinook in hte Sacramento area

#Now let's try centrarchids


cent = filter(DJmean2, CommonName %in% c("largemouth bass", "redear sunfish",
                                         "bluegill", "smallmouth bass", "black crappie"))  %>%
  droplevels()

ggplot(cent, aes(x = Regions, y = CPUE, fill = CommonName))+ geom_col()+
  facet_wrap(~Year)+
  scale_fill_manual(values = mypal, name = "Species")+
  theme_bw()+
  theme(legend.position = "bottom")

#model of centrarchids
centall = filter(DJFMP2, CommonName %in% c("largemouth bass", "redear sunfish",
                                           "bluegill", "smallmouth bass", "black crappie")) %>%
  group_by(Year, StationCode, Regions, SampleDate) %>%
  summarize(Count = sum(Count))%>%
  mutate(Yearf = factor(Year, levels = c(2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021)))


cent = brm(Count ~ Regions*Yearf+ (1|StationCode), family = zero_inflated_negbinomial(), 
          data = centall, iter = 2000, chains = 4)

cent
summary(cent)
conditional_effects(cent)

###################################
#beach seine models
DJFMPsum = mutate(DJFMPsum, Yearf = factor(Year, levels = c(2014, 2015, 2016, 2017, 2018, 2019, 2020, 2021)))


bs3 = brm(Catch ~ Regions*Yearf+ (1|StationCode), family = zero_inflated_negbinomial(), 
         data = DJFMPsum, iter = 2000, chains = 4)

bs3s = summary(bs3)
plot(bs3)
conditional_effects(bs3)
write.csv(bs3s$fixed, "beachseine.csv")


#######################
#beach seine NMDS

DJMat = DJFMP2 %>%
 # filter(Year %in% c(2014, 2015, 2017, 2019, 2020, 2021)) %>%
  mutate(CommonName2 = case_when(
    CommonName %in% DJrare ~ "Other",
    TRUE ~ CommonName
  )) %>%
  pivot_wider(id_cols = c(StationCode, Regions, Month, Year, SampleDate),
                    names_from = CommonName2, values_from = Count,
              values_fn = sum, values_fill = 0)  %>%
  ungroup() %>%
   mutate(Regions = as.factor(Regions), Year = as.factor(Year)) %>%
  filter(!is.na(`No catch`)) %>%
 dplyr::select(-`No catch`)
DJMat2 = dplyr::select(ungroup(DJMat), `largemouth bass`:`threadfin shad`)
DJMat3 = DJMat2/rowSums(DJMat2)

adonis(DJMat3~ Year*Regions, data = DJMat)

source("PlotNMDS.R")
DJMDS = metaMDS(DJMat2, k=3, trymax = 500)

PlotNMDS(DJMDS, data = DJMat, group = "Year")
PlotNMDS(DJMDS, data = DJMat, group = "Regions")

#Hm. Looks like the central delta might cluster out pretty nicely
#Chinook are weird though, and I have too much data.
#There was one catch in 2017 with 80 chinook that seems to be causing problems.
test = filter(DJMat, Year == 2017, StationCode == "SJ001S")
#Nope. This is just not working

####################################################
#salvaage
library(readxl)
yeartypes = read.csv("data/yearassignments.csv")



salvage = read_csv("data/salvagecatch.txt") %>%
  mutate(OrganismCode = as.character(OrganismCode))
orgcodes = read_excel("data/OrganismsLookUp.xlsx")

salvage = left_join(salvage, orgcodes)

#add in zeros
sal2 = pivot_wider(salvage, id_cols = c(SampleDate, AcreFeet, MinutesPumping, SampleTimeLength,
                                        SampleMethod, SampleTime, BuildingCode), 
                   names_from = CommonName, values_from = Count, values_fill = 0,
                   values_fn = sum) %>%
  pivot_longer(cols = c(Jacksmelt:last_col()), names_to = "Species", values_to = "count") %>%
  mutate(Year = year(SampleDate), Month = month(SampleDate))

write.csv(sal2, "BarrierSalvage.csv", row.names = F)

#just the listed fish 2014-2021

recents = filter(sal2, Year >2010, 
                # Month %in% c(6,7,8,9,10,11),
                 Species %in% c("Chinook Salmon",
                                                  "Delta Smelt", "Longfin Smelt",
                                                  "Green Sturgeon", "Steelhead"))

ggplot(recents, aes(x = as.factor(Year), y = count)) + geom_boxplot()+
  facet_grid(Month~Species, scales = "free_y") 

recentmean = group_by(recents, Year, Month, Species) %>%
  summarize(Catch = sum(count)) %>%
  left_join(yeartypes)

ggplot(recentmean, aes(x = Year, y = Catch, fill = ShortTerm)) + geom_col()+
  facet_wrap(~Species, scales = "free_y")+ ylab('Total Catch')+
  xlab("Year (June-November)")+
  scale_fill_manual(name = "Year Type", values = c("darkorange", "tan", "skyblue"))+
  theme_bw()

#wow, did we really get delt smelt in 2021?

DeltaSmelt = filter(sal2, Year >2013, 
                    Species == "Delta Smelt") %>%
  group_by(Month, Year) %>%
  summarize(Catch = sum(count, na.rm = T))

DS2 = filter(sal2, Year ==2021, 
                    Species == "Delta Smelt") %>%
  group_by(SampleDate) %>%
  summarize(Catch = sum(count, na.rm = T))
