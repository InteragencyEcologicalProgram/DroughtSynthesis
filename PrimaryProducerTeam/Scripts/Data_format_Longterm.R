library(tidyverse)
library(lubridate)
library(deltamapr) # https://github.com/InteragencyEcologicalProgram/deltamapr
library(sf)
source("Scripts/ggplot_themes.R")

## Program Sources of Data
# DOP= Directed Outflows Project (US Bureau of Reclamation)
# EMP= Environmental Monitoring Program
# FMWT= Fall Midwater Trawl
# NCRO= DWR North Central Regional Office
# STN= Summer Townet Survey
# SDO= ?? 
# USBR= US Bureau of Reclamation
# USGS-SFBRMP= US Geologiacl Survey San Francisco Research Monitoring Projct
# USGS-CAWSC= US Geological Survey CA Water Science Center


# EPSG codes
# NAD83 / UTM 10N = 26910, https://spatialreference.org/ref/epsg/nad83-utm-zone-10n/
# WGS84 = 4326
# NAD83 = 4269


#### FUNCTIONS ####
add_DateTime <- function(df){
  df2 <- df %>% 
    mutate(year= year(Date),
           month= month(Date),
           wyear= as.character(smwrBase::waterYear(Date)), #smwrBase a USGS package: https://github.com/USGS-R/smwrBase
           ds_year= ifelse(month == 12, year+1, year), # DS analysis will use modified year with december as the first month of the subsequent year
           Julian= yday(Date),
           DOY= ymd(str_c("1904", month(Date), day(Date), sep= '-')), #1904 was a leap year
           LatLong= str_c(Latitude, Longitude, sep= " , "))
  return(df2)
}




## Load Drought Synthesis (DS) Regions
rosie_regions <- read_csv("Data/Rosies_regions.csv")
DS_regions <- deltamapr::R_EDSM_Subregions_Mahardja_FLOAT %>% #NAD83 / UTM 10N
  filter(unique(.$SubRegion) %in% c(unique(rosie_regions$SubRegion), "Grant Line Canal and Old River")) %>%  # Add GLCAOR to our analysis
  select(-Region) %>% # Remove DeltaMapR Regions
  left_join(select(rosie_regions, SubRegion, Region)) %>%  # Add in Rosie's regions
  mutate(Region= ifelse(SubRegion == "Grant Line Canal and Old River", "SouthCentral", Region),
         Region= recode(Region, North= 'North Delta', SouthCentral= "South-Central Delta")) %>%
  distinct(.)


## Load Delta waterways and filter by DS Regions
DS_waterways <-  deltamapr::WW_Delta %>% # NAD83
  st_transform(., crs= 26910) %>%  # NAD83 / UTM 10N
  st_join(., DS_regions, left= FALSE,
          join= st_overlaps) # filter the Delta Waterways to include only DS regions

## Get WQ data from integrated database (https://github.com/sbashevkin/discretewq)
   # devtools::install_github("sbashevkin/discretewq")
   # library(discretewq)
idb_raw <- discretewq::wq(Sources = c("EMP", "USGS")) # Only EMP and USGS go back to the 1970s

idb_chla <- idb_raw %>%
  filter(!is.na(Chlorophyll)) %>% # remove rows with NA for both chla and mc_rating
  select(Source, Station, Latitude, Longitude, Field_coords, Date, Datetime, Depth, Chlorophyll) %>%
  rename(chla= Chlorophyll) %>%
  mutate(Source= ifelse(Source == "USGS", "USGS-SFBRMP", Source)) %>% #USGS San Francisco Bay Research and Monitoring Project, https://www.usgs.gov/mission-areas/water-resources/science/water-quality-san-francisco-bay-research-and-monitoring?qt-science_center_objects=0#qt-science_center_objects
  filter(str_detect(Station, "EZ") == FALSE) %>% # Remove the EMP stations EZ2, EZ6, EZ2-SJR, and EZ6-SJR (These have variable lat/longs, need to follow up with Ted on what they mean)
  add_DateTime %>% 
  mutate(Season= ifelse(month == 12 | month == 1 | month == 2, "Winter",
                        ifelse(month >= 3 & month <= 5, "Spring",
                               ifelse(month >= 6 & month <= 8, "Summer",
                                      ifelse(month >= 9 & month <= 11, "Fall", NA)))),
         Season= factor(Season, levels= c("Winter", "Spring", "Summer", "Fall"))) %>% 
  distinct(.)
  
  
idb_stations <- select(idb, Source, Station, Latitude, Longitude) %>%
  distinct(.)



## Make Simple Features (sf) object
# https://mattherman.info/blog/point-in-poly/
chla_stationsLT.sf <- idb_chla %>%
  select(Source, Station, Latitude, Longitude) %>%
  distinct() %>%
  filter(complete.cases(.)) %>% 
  st_as_sf(., coords= c("Longitude", "Latitude"), crs= 4269) %>% # NAD83
  st_transform(., crs= 26910) %>% # NAD 83/ UTM10N
  st_join(., DS_regions, left= TRUE,
          join= st_within) %>%
  select(-SQM) %>% 
  filter(!is.na(Region))


## Add Region and Subregion and water year type to data frame
DS_chlaLT <- left_join(idb_chla, st_drop_geometry(chla_stationsLT.sf)) %>%
  left_join(read_tsv('Data/water_year_type.txt')) %>%
  distinct(.) %>% 
  mutate(ds_year= as.character(ds_year),
         ds_year_type= factor(ds_year_type, ordered= TRUE, levels= c("1_Wet", "2_Below_avg", "3_Drought"))) %>%
  filter(!is.na(Region)) # remove data in a Region not included in this analysis
save(chla_stationsLT.sf, DS_chlaLT, DS_regions, DS_waterways,
     file= "Data/DS_LTdataframes.Rdata")

ggplot() +
  geom_sf(data= DS_regions) + 
  geom_sf(data= DS_waterways) +
  geom_sf(data= chla_stationsLT.sf) +
  coord_sf()


