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


#Grab the latest TNS datat from the webs
TNURL = "https://filelib.wildlife.ca.gov/Public/TownetFallMidwaterTrawl/TNS%20MS%20Access%20Data/TNS%20data/STN_CatchPerStation.csv"
Townet = read_csv(TNURL) %>%
  mutate(StationCode = as.character(`Station Code`)) %>%
  rename(SampDate = `Sample Date`, VolumeOfAllTows = `Volume of All Tows`)

#2021 data
STN2021 = read_csv("data/HABs/STNCatchPerStation2021.csv") %>%
  mutate(StationCode = as.character(StationCode)) %>%
  rename(`Tridentiger spp_` = `Tridentiger spp`)

Townet2 = bind_rows(Townet, STN2021)

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

#join regions to stations to get which station is in which region
stas1 = st_join(stas, regions, join=st_intersects)%>% # Add subregions
  filter(!is.na(Regions))%>% # Remove any data outside our regions of interest
  st_drop_geometry() %>% # Drop sf geometry column since it's no longer needed
  dplyr::select(StationCode, Regions)
  
TNS2 = left_join(stas1, Townet2)  %>%
  dplyr::select(-`TowsCompleted`,-`TemperatureTop`, -`TemperatureBottom`,          
                -`ConductivityTop`, -ConductivityBottom, -TideCode,                   
                -DepthBottom, -CableOut, -TowDirection,               
                -WindDirection,-TurbidityTop, -StartLatDegrees,            
                -StartLatMinutes, -StartLatSeconds, -StartLongDegrees,           
                -StartLongMinutes, -StartLongSeconds, -EndLatDegrees,              
                -EndLatMinutes, -EndLatSeconds, -EndLongDegrees,             
                -EndLongMinutes, -EndLongSeconds, -Latitude, -Longitude, -`<>`)

#Now just data from 2012 to present. From wide to long

TNSd = filter(TNS2, Year >2012) %>%
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
    TRUE ~ Species
  )) %>%
  group_by(StationCode, Regions, Year, Survey, SampDate, Species) %>%
  summarize(Catch = sum(Catch), CPUE = sum(CPUE))


TNSd %>%
  filter(Species == "Tridentiger spp_")%>%
ggplot(aes(x = Regions, y = log(CPUE+1))) +
  geom_boxplot() + facet_wrap(~Year)

TNSd %>%
  filter(Species == "Siberian prawn")%>%
  ggplot(aes(x = Regions, y = CPUE)) +
  geom_boxplot() + facet_wrap(~Year) +
  coord_cartesian(ylim = c(0, 0.005))

TNsum = group_by(TNSd, Year, Regions) %>%
  summarize(Catch = sum(CPUE))

ggplot(TNsum, aes(x = Regions, y= Catch)) + geom_boxplot()

#OK, there are really no fih in the central delta.

#I really want to do an NMDS on this

library(vegan)

TNSd2 = filter(TNSd, Year %in% c(2014, 2015, 2017, 2019, 2020, 2021))

TNMat = pivot_wider(TNSd2, id_cols = c(StationCode, Regions, Year, Survey),
                    names_from = Species, values_from = Catch)
TNMat2 = dplyr::select(ungroup(TNMat), `American Shad`:`Yellowfin Goby`) %>%
  dplyr::select( !Splittail, !`Tule Perch`, !Wakasagi, !`Bigscale Logperch`, !`Prickly Sculpin`)


FNMDS = metaMDS(TNMat2, k=3, trymax = 500)

adonis(TNMat2~ as.factor(Year) + Regions, data = TNMat)
#so only a very, very small perportion of hte variance. 

#one hypothesis ws there might be an increase in black bas sin the central delta

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
DJFMP = read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.244.7&entityid=a3e94e8f0cf6f675d716151c1ca17b4f")
DJFMPstas = read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.244.7&entityid=99a038d691f27cd306ff93fdcbc03b77")


stasD = st_as_sf(DJFMPstas, coords = c("Longitude_location","Latitude_location"), crs = st_crs(waterways))

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
  filter(!is.na(Regions)) %>%
  mutate(Date = mdy(SampleDate), Year = year(Date), Month = month(Date)) %>%
  filter(Year > 2013, Month %in% c(5,6,7,8,9))

#Put in the zeros
DJFMP2 = pivot_wider(DJFMP, id_cols = c(Regions, StationCode, Month, Year, Date, Volume), 
                     names_from = "CommonName", values_from = Count, values_fn = sum, values_fill = 0) %>%
  pivot_longer(cols = `largemouth bass`:last_col(), values_to = "Count", names_to = "CommonName")

ggplot(DJFMP, aes(x= Regions, y = Count, fill = CommonName)) + geom_boxplot() + 
  facet_wrap(~CommonName, scales = "free_y") + scale_fill_discrete(guide = NULL)

DJFMPsum = group_by(DJFMP2, Regions, StationCode, Month, Year, Date, Volume) %>%
  summarize(Catch = sum(Count)) %>%
  mutate(CPUE = Catch/Volume)


ggplot(DJFMPsum, aes(x= Regions, y = Catch)) + geom_boxplot()


ggplot(DJFMPsum, aes(x= Catch)) + geom_histogram()+facet_wrap(~Regions)

       