#Habs, from visual assessments

library(tidyverse)
library(discretewq)
library(sf)
library(emmeans)
library(lubridate)
library(ggmap)
library(readxl)

#I had Jeff Galef add teh 2021 , summer townet, and FMWT data to the 
#water quality data set that Sam put together. He did it in python
#and I don't have the code

HABs = read_csv("data/HABs/WQ_data_integrated4.csv")
str(HABs)
HABs = mutate(HABs,  Datex = as.Date(Date2, origin = "1899-12-30"),
              MonthYear = NULL, Datetime = NULL, Month = month(Datex), 
              Year = year(Datex), Date = Datex, Datex = NULL, Date2 = NULL)


##############################################################################################
#Reorganize NCRO data

NCRO <- read_excel("data/HABs/WQES HAB Observations and Field Readings.xlsx", 
                   col_types = c("text", "date", "numeric", 
                                 "numeric", "text", "numeric"))

#For once the data is in "long" format and we want it in "wide" format
NCRO2 = pivot_wider(NCRO, id_cols = c(StationCode, Date, `Secchi (m)`, `Microcystis`), 
                    names_from = Parameter, values_from = `Field Sonde Value`, values_fn = first)

#read in GPS and attach it
stas = read_excel("data/HABs/Station_Metadata_Coords.xlsx") %>%
  select(`WQES Code`, `Latitude (WGS84)`, `Longitude (WGS84)`) %>%
  rename(StationCode = `WQES Code`, Latitude = `Latitude (WGS84)`, Longitude = `Longitude (WGS84)`)

#join with GPS and rename columns so they are the same as the integrated data set
NCRO3 = left_join(NCRO2, stas) %>%
  mutate(Source = "NCRO") %>%
  rename(Chlorophyll = `Chlorophyll_ug/L`, Temperature = Temp_C, 
         Turbidity = Turbidity_FNU, Salinity = Salinity_ppt, Conductivity = `SpCond_uS/cm`, Station = StationCode) %>%
  mutate(Secchi = `Secchi (m)`/100, Month = month(Date), Year = year(Date)) %>%
  select(Source, Station, Date, Secchi, Microcystis, Chlorophyll, Salinity, Conductivity, Temperature,
         Turbidity, Latitude, Longitude, Year)
names(NCRO3)
names(HABs)

#add to the rest of the data
HABs1 = bind_rows(HABs, NCRO3)
###########################################################################
#Now get the DOP data in there
DOP = read_excel("data/HABs/DOP water quality 2019-2021.xlsx", na = "NA")

#remove the "deep" samples (with no seperate HAB score) and rename columns
DOP = filter(DOP, !is.na(hab_score)) %>%
  mutate(Source = "DOP", Month = month(date), Year = year(date),
  ) %>%
  rename(Station = site_id,
         Latitude = start_latitude,
         Longitude = start_longitude,
         Date = date,
         Secchi = secchi_depth,
         Salinity = salinity,
         Conductivity = specific_conductivity,
         Microcystis = hab_score,
         Turbidity = turbidity,
         Temperature = temperature,
         Chlorophyll = chlorophyll_a
  )

HABs = bind_rows(HABs1, DOP)


#turn it into a spatial object

HABssf = filter(HABs, !is.na(Longitude), !is.na(Latitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = st_crs(4326))

#attach regional assignments for the drought synthesis report

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

write.csv(Habs2, "WQ_HABs_w2021.csv")