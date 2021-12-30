library(tidyverse)
library(lubridate)
library(deltamapr) # https://github.com/InteragencyEcologicalProgram/deltamapr
library(sf)
source("Scripts/MyFunctionsAndThemes.R")

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


## Load Water Year Types (Based on Sacramento Index)
wy_types <- read_csv("Data/WaterYearAssignments.csv", col_types = "dccccc") %>% 
  select(Year, Yr_type) %>% 
  rename(ds_year= Year, ds_year_type= Yr_type)


## Get 1975-2020 WQ data from integrated database (https://github.com/sbashevkin/discretewq)
# devtools::install_github("sbashevkin/discretewq")
# library(discretewq)
idb_raw <- discretewq::wq(Sources = c("EMP", "USGS")) # Only EMP and USGS go back to the 1970s

emp_stations <- idb_raw %>% 
  filter(Source == "EMP") %>% 
  select(Source, Station, Latitude, Longitude) %>%
  distinct(.)


## Get 2021 EMP data 
## March-Oct 2021. Data was not collected January-February due to COVID
emp_colnames <- c("Station", "StationNum", "Datetime", "Depth", "chla")

emp_2021 <- read_csv("Data/EMP_2021_March_October_Chla.csv") %>%
  rename_with(~emp_colnames) %>% 
  select(Station, Datetime, chla) %>% 
  mutate(Station= str_replace(Station, "\\ -.*$", ""),
         Source= "EMP",
         Datetime= mdy_hm(Datetime),
         Date= as.Date(Datetime),
         chla= as.numeric(chla)) %>% 
  mutate(Station= ifelse(Station == "Sacramento River @ Hood", "C3A", Station),
         Station= ifelse(Station == "NZ068 in Sacramento River", "NZ068", Station),
         chla= ifelse(is.na(chla), 0.25, chla)) %>% 
  filter(!str_detect(Station, "Entrapment")) %>% 
  left_join(., emp_stations)



## Format idb data and combine with EMP 2021 data
idb_chla <- idb_raw %>%
  filter(!is.na(Chlorophyll)) %>% # remove rows with NA for both chla and mc_rating
  select(Source, Station, Latitude, Longitude, Field_coords, Date, Datetime, Depth, Chlorophyll) %>%
  rename(chla= Chlorophyll) %>%
  mutate(Source= ifelse(Source == "USGS", "USGS-SFBRMP", Source)) %>% #USGS San Francisco Bay Research and Monitoring Project, https://www.usgs.gov/mission-areas/water-resources/science/water-quality-san-francisco-bay-research-and-monitoring?qt-science_center_objects=0#qt-science_center_objects
  filter(str_detect(Station, "EZ") == FALSE) %>% # Remove the EMP stations EZ2, EZ6, EZ2-SJR, and EZ6-SJR (These have variable lat/longs, need to follow up with Ted on what they mean)
  full_join(., emp_2021) %>% 
  add_DateTime %>% 
  mutate(Season= ifelse(month == 12 | month == 1 | month == 2, "Winter",
                        ifelse(month >= 3 & month <= 5, "Spring",
                               ifelse(month >= 6 & month <= 8, "Summer",
                                      ifelse(month >= 9 & month <= 11, "Fall", NA)))),
         Season= factor(Season, levels= c("Winter", "Spring", "Summer", "Fall"))) %>% 
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
  left_join(., wy_types) %>% 
  #left_join(read_tsv('Data/water_year_type.txt')) %>%
  mutate(ds_year= as.character(ds_year),
         ds_year_type= factor(ds_year_type, ordered= TRUE, levels= c("Critical", "Dry", "Below Normal", "Above Normal", "Wet"))) %>%
  filter(!is.na(Region)) # remove data in a Region not included in this analysis
save(chla_stationsLT.sf, DS_chlaLT, DS_regions, DS_waterways,
     file= "Data/DS_dataframesLT.Rdata")




