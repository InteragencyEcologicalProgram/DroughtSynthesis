library(tidyverse)
library(lubridate)
library(deltamapr) # https://github.com/InteragencyEcologicalProgram/deltamapr
library(sf)
source("PrimaryProducerTeam/Scripts/MyFunctionsAndThemes.R")

## Program Sources of Data
## discretewq: https://github.com/sbashevkin/discretewq
# DOP= Directed Outflows Project (US Bureau of Reclamation)
# EMP= Environmental Monitoring Program
# FMWT= Fall Midwater Trawl
# NCRO= DWR North Central Regional Office
# STN= Summer Townet Survey
# SDO= Stockton Deep Water Shipping Channel Dissolved Oxygen Monitoring
# USBR= US Bureau of Reclamation
# USGS-SFBRMP= US Geological Survey San Francisco Research Monitoring Projct
# USGS-CAWSC= US Geological Survey CA Water Science Center
  

# EPSG codes
# NAD83 / UTM 10N = 26910, https://spatialreference.org/ref/epsg/nad83-utm-zone-10n/
# WGS84 = 4326
# NAD83 = 4269


## Load Drought Synthesis (DS) Regions
rosie_regions <- read_csv("PrimaryProducerTeam/Data/Rosies_regions.csv")
DS_regions <- deltamapr::R_EDSM_Subregions_Mahardja_FLOAT %>% #NAD83 / UTM 10N
  filter(unique(.$SubRegion) %in% c(unique(rosie_regions$SubRegion), "Grant Line Canal and Old River")) %>%  # Add GLCAOR to our analysis
  dplyr::select(-Region) %>% # Remove DeltaMapR Regions
  left_join(dplyr::select(rosie_regions, SubRegion, Region)) %>%  # Add in Rosie's regions
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
idb_raw <- discretewq::wq(Sources = c("EMP", "STN", "FMWT", "EDSM", "DJFMP",
                       "SDO", "SKT", "SLS", "20mm", "Suisun",
                       "Baystudy", "USBR", "USGS", "YBFMP"))

## Read Integrated Database from EDI, https://portal.edirepository.org/nis/mapbrowse?packageid=edi.731.1
idb <- idb_raw %>%
  #read_csv("PrimaryProducerTeam/Data/Delta_Integrated_WQ.csv", col_types= "ccdddDDdcccdcdddddc") %>% #automatic column parser was failing, so had to manually assign the column types
  filter(!(is.na(Chlorophyll) & is.na(Microcystis))) %>% # remove rows with NA for both chla and mc_rating
  select(Source, Station, Latitude, Longitude, Field_coords, Date, Datetime, Depth, Microcystis, Chlorophyll) %>%
  rename(chla= Chlorophyll, mc_rating= Microcystis) %>%
  mutate(Source= ifelse(Source == "USGS", "USGS-SFBRMP", Source)) %>% #USGS San Francisco Bay Research and Monitoring Project, https://www.usgs.gov/mission-areas/water-resources/science/water-quality-san-francisco-bay-research-and-monitoring?qt-science_center_objects=0#qt-science_center_objects
  filter(str_detect(Station, "EZ") == FALSE) # Remove the EMP stations EZ2, EZ6, EZ2-SJR, and EZ6-SJR (These have variable lat/longs, need to follow up with Ted on what they mean)

idb_stations <- select(idb, Source, Station, Latitude, Longitude) %>%
  distinct(.)



## Read DWR South Delta Monitoring data (North Central Regional Office, NCRO)
dwr_Sdelta_stations <- read_csv('PrimaryProducerTeam/Data/SDelta_Station_lat_long.csv') %>%
  rename(HABstation= `HAB station ID`)


dwr_Sdelta_mc <- read_csv('PrimaryProducerTeam/Data/qry_ObsHabs_SDelta_2017-2021.csv') %>%
  rename(mc_rating = IndexScore, HABstation= StationCode) %>%
  mutate(Date= mdy(FldDate)) %>%
  filter(!is.na(mc_rating)) %>%
  select(-FldObsWaterHabs, -FldDate) %>%
  left_join(., dwr_Sdelta_stations)

dwr_Sdelta <- read_csv("PrimaryProducerTeam/Data/WQDataReport.SDelta_2000-2021_ChlaPheo.csv", n_max = 11946) %>%
  left_join(., dwr_Sdelta_stations) %>%
  filter(Analyte == "Chlorophyll a") %>%
  rename(Station= LongStationName, chla= Result, Datetime= CollectionDate, Latitude= `Latitude (WGS84)`, Longitude = `Longitude (WGS84)`) %>%
  mutate(Datetime= mdy_hm(Datetime),
         Date= ymd(str_c(year(Datetime), month(Datetime), day(Datetime), sep="-")),
         Source= "NCRO") %>%
  left_join(., dwr_Sdelta_mc) %>%
  select(LongStationName, ShortStationName, HABstation, Date, Datetime, Source, chla, SampleType, mc_rating, Latitude, Longitude) %>%
  mutate(chla= as.numeric(ifelse(str_detect(chla, "N\\.S\\.|<|D1"), -88, chla))) # transform below detects to -88 and make column numeric

## Read USGS CA Water Science Center data
## USGS chla codes (column `M chla (ug/L)`:
# 00050 is 0.7um chla
# CHL06 is the 5micron chla
# FL016 - Katy thinks this one is associated with an old 5 micron lab code, I have to research it though because it is associated with chla for 9 samples, 3 of those were not submitted for 5micron.

# usgs_chla_raw <- read_csv('Data/USGS_DiscreteStationDataFinal_20210909_CS.csv')
# 
# 
# usgs_chla <- usgs_chla_raw %>%
#   select(field_ID, dec_lat_va, dec_long_va, sample_strt_dt, `Date format change`, `Chla (ug/L)`, `M Chla (ug/L)`) %>%
#   filter(`M Chla (ug/L)` == "00050") %>% # Only keep 0.7 micron chla values
#   select(-`M Chla (ug/L)`) %>%
#   rename(Station= field_ID, Latitude= dec_lat_va, Longitude= dec_long_va, Date= `Date format change`, 
#          Datetime= sample_strt_dt, 
#          chla= `Chla (ug/L)`) %>%
#   filter(!is.na(chla)) %>%
#   mutate(Date= ymd(Date),
#          Datetime= ymd_hm(Datetime),
#          Source= "USGS-CAWSC")
# 



## Read in additional HABs Data from Fall Midwater Trawl, Directed Outflows Project, and EMP
habs_add <- read_csv("PrimaryProducerTeam/Data/Microcystis_4NOV2021.csv") %>%
  select(Source, Station, Date, Microcystis, Chlorophyll, Latitude, Longitude) %>%
  rename(mc_rating= Microcystis, chla= Chlorophyll) %>%
  mutate(Station= ifelse(Station == "72" | Station == "73", str_pad(Station, pad= "0", width= 3), Station),
         Date= mdy(Date)) %>%
  filter(str_detect(Station, "EZ") == FALSE) %>% # Remove the EMP stations EZ2, EZ6, EZ2-SJR, and EZ6-SJR (These have variable lat/longs, need to follow up with Ted on what they mean)
  left_join(., idb_stations) %>% 
  add_DateTime() #%>%
  #left_join(., DOP_stations)

habs_add %>%
  group_by(Source) %>%
  summarize(totNA= sum(is.na(Latitude)))

#habs_add_stations %in% idb_stations

## Get 2021 EMP data 
## March-Oct 2021. Data was not collected January-February due to COVID
emp_colnames <- c("Station", "StationNum", "Datetime", "Depth", "chla")

# Get 2021 MC-rating values from EMP from the habs_add data frame
emp_2021_mc <- habs_add %>% 
  filter(ds_year == 2021 & Source == "EMP") %>% 
  select(Source, Station, Date, mc_rating)


emp_2021 <- read_csv("PrimaryProducerTeam/Data/EMP_2021_March_October_Chla.csv") %>%
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
  left_join(., idb_stations) %>% 
  left_join(., emp_2021_mc)




## Combine data and filter to the Short Term Synthesis time period 2011-present
DS_data_noRegions <- full_join(idb, dwr_Sdelta) %>%
  #full_join(., usgs_chla) %>%
 # full_join(., filter(habs_add, !(year == 2021 & Source == "EMP"))) %>%
  full_join(., emp_2021) %>% 
  filter(Longitude > -122.145 & Latitude > 37.7) %>% # FILTER BY the regions of interest for Drought Synthesis
  mutate(Station= ifelse(is.na(Station), ShortStationName, Station)) %>%
  add_DateTime() %>% 
  filter(ds_year >= 2011) %>% ## Filter data for the Short Term Synthesis
  # SEASON
  mutate(Season= ifelse(month == 12 | month == 1 | month == 2, "Winter",
                        ifelse(month >= 3 & month <= 5, "Spring",
                               ifelse(month >= 6 & month <= 8, "Summer",
                                      ifelse(month >= 9 & month <= 11, "Fall", NA)))),
         Season= factor(Season, levels= c("Winter", "Spring", "Summer", "Fall"))) %>% 
  select(-Depth, -LongStationName, -ShortStationName, -HABstation, -SampleType, -Field_coords) %>% 
  distinct(.)


#unique(DS_data_noRegions$Source)
#unique(habs_add$Source)
## CODE TO IDENTIFY WHAT LAT/LONG TO FILTER DS_data to include DS sub regions
# test <- DS_data %>%
#   filter(Longitude > -122.145 & Latitude > 37.6) %>%
#   select(Source, Station, Latitude, Longitude) %>%
#   distinct() %>%
#   st_as_sf(coords= c("Longitude", "Latitude"), crs= 4269) %>% #NAD83
#   st_transform(., crs= 26910)
#
# ggplot() +
#   geom_sf(data= DS_regions, aes(fill= SubRegion), alpha= 0.3) +
#   geom_sf(data= DS_waterways, fill= "skyblue3", color= "black") +
#   geom_sf(data= test, color= "red") +
#   coord_sf()


## Make Simple Features (sf) object
# https://mattherman.info/blog/point-in-poly/
chla_stations <- DS_data_noRegions %>%
  select(Source, Station, Latitude, Longitude) %>%
  distinct() %>%
  filter(complete.cases(.))

chla_stations.sf <- st_as_sf(chla_stations, coords= c("Longitude", "Latitude"), crs= 4269) %>% #NAD83
  st_transform(., crs= 26910) %>% # NAD 83/ UTM10N
  st_join(., DS_regions, left= TRUE,
          join= st_within) %>%
  select(-SQM)


## Load Water Year Types (Based on Sacramento Index)
wy_types <- read_csv("Data/WaterYearAssignments.csv", col_types = "dccccc") %>% 
  select(Year, Yr_type) %>% 
  rename(ds_year= Year, DWR_YrType= Yr_type)

## Add Region and Subregion and water year type to data frame
DS_dataST <- left_join(DS_data_noRegions, st_drop_geometry(chla_stations.sf)) %>%
  left_join(read_tsv('Data/water_year_type.txt')) %>%
  left_join(., wy_types) %>% 
  distinct(.) %>% 
  mutate(ds_year= as.character(ds_year),
         ds_year_type= factor(ds_year_type, ordered= TRUE, levels= c("Drought", "Neutral", "Wet")),
         DWR_YrType= factor(DWR_YrType, ordered= TRUE, levels= c("Critical", "Dry", "Below Normal", "Above Normal", "Wet"))) %>%
  filter(!is.na(Region)) # remove data in a Region not included in this analysis
save(chla_stations.sf, chla_stations, DS_dataST, DS_regions, DS_waterways,
     file= "Data/DS_dataframesST.Rdata")

#write_tsv(DS_data, "Data/DS_Chla_DataCombined_Rexport.tsv")



## Remove objects that are no longer necessary
rm(usgs_chla_raw, DS_data_noRegions, chla_stations, dwr_Sdelta_stations, rosie_regions)

