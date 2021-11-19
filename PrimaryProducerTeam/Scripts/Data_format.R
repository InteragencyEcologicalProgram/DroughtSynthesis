library(tidyverse)
library(lubridate)
library(deltamapr) # https://github.com/InteragencyEcologicalProgram/deltamapr
library(sf)
source("Scripts/ggplot_themes.R")

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
  mutate(Region= ifelse(SubRegion == "Grant Line Canal and Old River", "SouthCentral", Region)) %>%
  distinct(.)

## Load Delta waterways and filter by DS Regions
DS_waterways <-  deltamapr::WW_Delta %>% # NAD83
  st_transform(., crs= 26910) %>%  # NAD83 / UTM 10N
  st_join(., DS_regions, left= FALSE,
        join= st_overlaps) # filter the Delta Waterways to include only DS regions

## Get WQ data from integrated database (https://github.com/sbashevkin/discretewq)
#devtools::install_github("sbashevkin/discretewq")
#library(discretewq)
idb_raw <- discretewq::wq(Sources = c("EMP", "STN", "FMWT", "EDSM", "DJFMP",
                       "SDO", "SKT", "SLS", "20mm", "Suisun",
                       "Baystudy", "USBR", "USGS", "YBFMP"))

## Read Integrated Database from EDI, https://portal.edirepository.org/nis/mapbrowse?packageid=edi.731.1
idb <- idb_raw %>%
  #read_csv("Data/Delta_Integrated_WQ.csv", col_types= "ccdddDDdcccdcdddddc") %>% #automatic column parser was failing, so had to manually assign the column types
  filter(!(is.na(Chlorophyll) & is.na(Microcystis))) %>% # remove rows with NA for both chla and mc_rating
  select(Source, Station, Latitude, Longitude, Field_coords, Date, Datetime, Depth, Microcystis, Chlorophyll) %>%
  rename(chla= Chlorophyll, mc_rating= Microcystis) %>%
  mutate(Source= ifelse(Source == "USGS", "USGS-SFBRMP", Source)) %>% #USGS San Francisco Bay Research and Monitoring Project, https://www.usgs.gov/mission-areas/water-resources/science/water-quality-san-francisco-bay-research-and-monitoring?qt-science_center_objects=0#qt-science_center_objects
  filter(str_detect(Station, "EZ") == FALSE) # Remove the EMP stations EZ2, EZ6, EZ2-SJR, and EZ6-SJR (These have variable lat/longs, need to follow up with Ted on what they mean)

idb_stations <- select(idb, Source, Station, Latitude, Longitude) %>%
  distinct(.)

## Read DWR South Delta Monitoring data
dwr_Sdelta_stations <- read_csv('Data/SDelta_Station_lat_long.csv') %>%
  rename(HABstation= `HAB station ID`)


dwr_Sdelta_mc <- read_csv('Data/qry_ObsHabs_SDelta_2017-2021.csv') %>%
  rename(mc_rating = IndexScore, HABstation= StationCode) %>%
  mutate(Date= mdy(FldDate)) %>%
  filter(!is.na(mc_rating)) %>%
  select(-FldObsWaterHabs, -FldDate) %>%
  left_join(., dwr_Sdelta_stations)

dwr_Sdelta <- read_csv("Data/WQDataReport.SDelta_2000-2021_ChlaPheo.csv", n_max = 11946) %>%
  left_join(., dwr_Sdelta_stations) %>%
  filter(Analyte == "Chlorophyll a") %>%
  rename(Station= LongStationName, chla= Result, Datetime= CollectionDate, Latitude= `Latitude (WGS84)`, Longitude = `Longitude (WGS84)`) %>%
  mutate(Datetime= mdy_hm(Datetime),
         Date= ymd(str_c(year(Datetime), month(Datetime), day(Datetime), sep="-")),
         Source= "DWR S. Delta") %>%
  left_join(., dwr_Sdelta_mc) %>%
  select(LongStationName, ShortStationName, HABstation, Date, Datetime, Source, chla, SampleType, mc_rating, Latitude, Longitude) %>%
  mutate(chla= as.numeric(ifelse(str_detect(chla, "N\\.S\\.|<|D1"), -88, chla))) # transform below detects to -88 and make column numeric

## Read USGS CA Water Science Center data
## USGS chla codes (column `M chla (ug/L)`:
# 00050 is 0.7um chla
# CHL06 is the 5micron chla
# FL016 - Katy thinks this one is associated with an old 5 micron lab code, I have to research it though because it is associated with chla for 9 samples, 3 of those were not submitted for 5micron.

usgs_chla_raw <- read_csv('Data/USGS_DiscreteStationDataFinal_20210909_CS.csv')


usgs_chla <- usgs_chla_raw %>%
  select(field_ID, dec_lat_va, dec_long_va, sample_strt_dt, `Date format change`, `Chla (µg/L)`, `M Chla (µg/L)`) %>%
  #filter(`M Chla (Âµg/L)` != "00050") %>% # Remove all the 5 micron chla values
  select(-`M Chla (µg/L)`) %>%
  rename(Station= field_ID, Latitude= dec_lat_va, Longitude= dec_long_va, Date= `Date format change`, Datetime= sample_strt_dt, chla= `Chla (µg/L)`) %>%
  filter(!is.na(chla)) %>%
  mutate(Date= ymd(Date),
         Datetime= ymd_hm(Datetime),
         Source= "USGS-CAWSC")

## Read in additional HABs Data from Fall Midwater Trawl, Directed Outflows Project, and EMP
#read_csv("Data/HABs2021.csv")
#Microcystis_4NOV2021
# habs_add <- read_csv("Data/HABs2021.csv") %>%
#   select(Source, Station, Datex, Microcystis) %>%
#   rename(mc_rating= Microcystis, Date= Datex) %>%
#   mutate(Station= ifelse(Station == "72" | Station == "73", str_pad(Station, pad= "0", width= 3), Station),
#          Station= str_c(Source, Station, sep= " ")) %>%
#   left_join(., idb_stations)

# habs_add <- readxl::read_xlsx("Data/WQ_HABs_w2021.xlsx", sheet= "in", n_max= 18434) %>%
#   select(Source, Station, Date, Microcystis, Chlorophyll) %>%
#   rename(mc_rating= Microcystis) %>%
#   mutate(Station= ifelse(Station == "72" | Station == "73", str_pad(Station, pad= "0", width= 3), Station)) %>%
#          #Station= str_c(Source, Station, sep= " ")) %>%
#   left_join(., idb_stations)

## Directed Outflow Project stations
# DOP_stations <- readxl::read_xlsx("Data/DOP_WQ_ 2019_2021_11-2-2021.xlsx") %>%
#   select(site_id, start_latitude, start_longitude) %>%
#   distinct(.) %>%
#   rename(Station= site_id, Latitude= start_latitude, Longitude= start_longitude)


habs_add <- read_csv("Data/Microcystis_4NOV2021.csv") %>%
  select(Source, Station, Date, Microcystis, Chlorophyll, Latitude, Longitude) %>%
  rename(mc_rating= Microcystis, chla= Chlorophyll) %>%
  mutate(Station= ifelse(Station == "72" | Station == "73", str_pad(Station, pad= "0", width= 3), Station),
         Date= mdy(Date)) %>%
  filter(str_detect(Station, "EZ") == FALSE) %>% # Remove the EMP stations EZ2, EZ6, EZ2-SJR, and EZ6-SJR (These have variable lat/longs, need to follow up with Ted on what they mean)
  left_join(., idb_stations) #%>%
  #left_join(., DOP_stations)


habs_add %>%
  group_by(Source) %>%
  summarize(totNA= sum(is.na(Latitude)))


#habs_add_stations %in% idb_stations

## Combine data and filter to the Short Term Synthesis time period 2011-present
DS_data_noRegions <- full_join(idb, dwr_Sdelta) %>%
  full_join(., usgs_chla) %>%
  full_join(., habs_add) %>%
  filter(Longitude > -122.145 & Latitude > 37.7) %>% # FILTER BY the regions of interest for Drought Synthesis
  mutate(Station= ifelse(is.na(Station), ShortStationName, Station)) %>%
  mutate(year= year(Date),
         month= month(Date),
         wyear= as.character(smwrBase::waterYear(Date)), #smwrBase a USGS package: https://github.com/USGS-R/smwrBase
         ds_year= ifelse(month == 12, year+1, year), # DS analysis will use modified year with december as the first month of the subsequent year
         Julian= yday(Date),
         DOY= ymd(str_c("1904", month(Date), day(Date), sep= '-')), #1904 was a leap year
         LatLong= str_c(Latitude, Longitude, sep= " , ")) %>%
  filter(ds_year >= 2011) %>% ## Filter data for the Short Term Synthesis
  # SEASON
  mutate(Season= ifelse(month == 12 | month == 1 | month == 2, "Winter",
                        ifelse(month >= 3 & month <= 5, "Spring",
                               ifelse(month >= 6 & month <= 8, "Summer",
                                      ifelse(month >= 9 & month <= 11, "Fall", NA)))),
         Season= factor(Season, levels= c("Winter", "Spring", "Summer", "Fall"))) %>% 
  select(-Depth, -LongStationName, -ShortStationName, -HABstation, -SampleType, -Field_coords) %>% 
  distinct(.)

names(DS_data_noRegions)



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


## Add Region and Subregion and water year type to data frame
DS_data <- left_join(DS_data_noRegions, st_drop_geometry(chla_stations.sf)) %>%
  left_join(read_tsv('Data/water_year_type.txt')) %>%
  distinct(.) %>% 
  mutate(ds_year= as.character(ds_year),
         ds_year_type= factor(ds_year_type, ordered= TRUE, levels= c("1_Wet", "2_Below_avg", "3_Drought"))) %>%
  filter(!is.na(Region)) # remove data in a Region not included in this analysis
save(chla_stations.sf, chla_stations, DS_data, DS_regions, DS_waterways,
     file= "Data/DS_dataframes.Rdata")

#write_tsv(DS_data, "Data/DS_Chla_DataCombined_Rexport.tsv")



## Remove objects that are no longer necessary
rm(usgs_chla_raw, DS_data_noRegions, chla_stations, dwr_Sdelta_stations, rosie_regions)

