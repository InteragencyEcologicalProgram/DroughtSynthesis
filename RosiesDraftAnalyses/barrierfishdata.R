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
  mutate(Station = as.character(`Station Code`))

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
rename(Station = StationCode) %>%
  select(Station, Regions)
  
TNS2 = left_join(stas1, Townet)

#Now just data from 2012 to present. From wide to long

TNSd = filter(TNS2, Year >2012) %>%
  mutate(Stripers = sum(`Age-0 Striped Bass`, `Age-1 Striped Bass`, `Age-2 Striped Bass`),
         `Age-0 Striped Bass` = NULL, `Age-1 Striped Bass` = NULL, `Age-2 Striped Bass` = NULL) %>%
   pivot_longer(cols = `American Shad`:Stripers, names_to = "Species", values_to = "Catch") %>%
  mutate(CPUE = Catch/`Volume of All Tows`)

#Calculate which species make up less than 5% of the total catch and drop them.
totcatch = sum(TNSd$Catch)
species = group_by(TNSd, Species) %>%
  summarize(tot = sum(Catch), Perc = tot/totcatch)

TNSd = filter(TNSd, !Species %in% filter(species, tot == 0)$Species) %>%
  mutate(Species = case_when(
    Species %in% filter(species, tot == 1)$Species ~ "Other",
    TRUE ~ Species
  )) %>%
  group_by(Station, Regions, Year, Survey, `Sample Date`, `Tows Completed`,
           `Volume of All Tows`, Species) %>%
  summarize(Catch = sum(Catch), CPUE = sum(CPUE))


TNSd %>%
  filter(Species == "Stripers")%>%
ggplot(aes(x = Regions, y = log(CPUE+1))) +
  geom_boxplot() + facet_wrap(~Year)

#I really want to do an NMDS on this

library(vegan)

TNSd2 = filter(TNSd, Year %in% c(2014, 2015, 2017, 2019, 2020))

TNMat = pivot_wider(TNSd2, id_cols = c(Station, Regions, Year, Survey),
                    names_from = Species, values_from = Catch)
TNMat2 = select(ungroup(TNMat), `American Shad`:`Yellowfin Goby`) %>%
  select( !Splittail, !`Tule Perch`, !Wakasagi, !`Bigscale Logperch`, !`Prickly Sculpin`)


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

