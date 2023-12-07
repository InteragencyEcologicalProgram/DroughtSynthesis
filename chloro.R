#combining chloro data for drought mast clam/jelly manuscript
#will pull from the discrete water quality datasetz, and maybe the National Estuarine 
#Research Reserve System, which samples in SF Bay but need to explore

#need to install and download the discrete water quality package from github
#install.packages("devtools")
#devtools::install_github("InteragencyEcologicalProgram/discretewq")

library(tidyverse)
library(discretewq)
library(dplyr)
library(sf) #this package helps with dealing with spatial data for the regions and such
library(lubridate)
library(readxl)

#Discrete WQ data Package---------

water_qual <- wq(
  Sources = c(
    "EMP", 
    "NCRO",
    "USGS_SFBS")
)

str(water_qual)

#Other Sources-------

#grabbed data from ICF and also the UCD Suisun survey

##UCD Suisun Chloro------- 

#load data from two different UCD suisun surveys

ucd_arc = read.csv("~/Data/Drought MAST/Invert Team/Jellies/Jellies and Clams Manuscript/Chlorophyll for Jellies/Raw Data/UCD Suisun Marsh Chloro/UCD Suisun ArcProject_DV_MZ_2013_2014.csv") %>% 
  mutate("Source" = "UCD ArcProject", 
         Date = mdy(Date)) 

ucd_blacklock = read.csv("~/Data/Drought MAST/Invert Team/Jellies/Jellies and Clams Manuscript/Chlorophyll for Jellies/Raw Data/UCD Suisun Marsh Chloro/UCD Suisun Blacklock_WQ_2013_2015.csv") %>% 
  mutate("Source" = "UCD Blacklock", 
         Date = mdy(Date), 
         year= year(Date), 
         month = month(Date)) 

#load the site coordinates.... was given a KMZ (google earth) file and needed to save as KML, open in excel as XML and then convert the coordinates from text to column since the coords for lat and long were in one column

ucd_coords = read_xlsx("~/Data/Drought MAST/Invert Team/Jellies/Jellies and Clams Manuscript/Chlorophyll for Jellies/Raw Data/UCD Suisun Marsh Chloro/UCD Suisun Chloro Site Coords.xlsx")

ucd_chloro = full_join(ucd_arc, ucd_blacklock) %>% #merge two ucd datasets
  merge(., ucd_coords, by = "Site") %>% # add in the coordinates
  mutate(., "Station" = Site, 
         "Year" = year, # the rename function wasn't working to rename these to merge them so just created a new column
         "Month" = month, 
         "Chlorophyll" = Chlorophyll.ppb) %>% 
  select(Source, Station, Latitude, Longitude, Chlorophyll, Year, Month)
  
  
  
##ICF------

icf_wq = read.csv("~/Data/Drought MAST/Invert Team/Jellies/Jellies and Clams Manuscript/Chlorophyll for Jellies/Raw Data/DOP_ICF_TowData_2017-2022.csv") 

icf_wq$Date = ymd((icf_wq$Date)) #change date column from character to date
  
icf_wq = icf_wq %>% 
  mutate("Source" = "ICF", #add column for data source to merge later
         "Year" = year(Date), 
         "Month" = month(Date),
         "Station" = Station_Code, 
         "Chlorophyll" = Chl_a) %>% 
  select(Source, Station, Latitude, Longitude, Chlorophyll, Year, Month)


#Discrete WQ Chloro----


#filter out data and trim down file to just the main columns and just for chlorophyll
# also only need from 2000 only

chloro = select(water_qual, Source:Longitude, Chlorophyll, Year, Month) %>% 
  filter(Year >= 2000)

#merge icf and other water quality data into one data frame

all_chloro = full_join(ucd_chloro, icf_wq) %>% 
  full_join(., chloro)


#Drought Regions----

#add in the drought regions file so I can section out the data
#got the drought regions from the github here:
#https://github.com/InteragencyEcologicalProgram/DroughtSynthesis/blob/main/DroughtRegions.RData
#RData file so just need to load it

load("C:/Users/cburdi/Documents/Data/Drought MAST/Invert Team/Jellies/Jellies and Clams Manuscript/Chlorophyll for Jellies/DroughtRegions.RData")

Stations = all_chloro %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) %>%
  st_transform(crs = st_crs(Regions)) %>%
  st_join(Regions) %>%
  st_drop_geometry()

#error and won't do the code because there are some NA values in the coords. Find these with subset, but the following command doesn't work, prolly because it is counting it as a character

na_coords = all_chloro %>% 
  subset(all_chloro$Latitude=='NA')

#try again with the is_na function

na_coords = all_chloro %>% 
  subset(is.na(all_chloro$Latitude))

#all the coords that are NA are from the EZ stations. filter out those

chloro_no_na = filter(all_chloro, !is.na(all_chloro$Latitude))

#now redo the region stuff with the file with no nas for the coordinates

Stations = chloro_no_na %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326, remove = FALSE) %>%
  st_transform(crs = st_crs(Regions)) %>% 
  st_join(Regions) %>%
  st_drop_geometry() %>% 
  filter(Region != "NA") %>% #remove any stations that are represented by the drought regions
  filter(Chlorophyll != "NA") #and any missing chloro values


#NERRS Data----

#look at the nerrs data from here: https://cdmo.baruch.sc.edu/data/available-data/
#looks like the collect both discrete and continuous in SF bay. requested a download of their data from the SF bay stations for their monitoring. checking out data to see if we can use it


nerrs= read.csv("~/Data/Drought MAST/Invert Team/Jellies/Jellies and Clams Manuscript/Chlorophyll for Jellies/Raw Data/NERRS Data/NERRS Discrete SF Bay Chloro.csv") %>% 
  filter(ChlFluor != "NA")

str(nerrs)

#no records with the discrete data for chloro. its not a required parameter for the discrete sampling. don't want to use contiuous

#Chloro Averages--------

chloro_mean = Stations %>% 
  group_by(Year, Month, Region) %>% 
  summarise(avg_chloro = mean(Chlorophyll), 
            n= length(Chlorophyll),
            sd = sd(Chlorophyll),
            se = (sd/ (sqrt(n))))

write.csv(chloro_mean, "Combined Average Chloro for Jellies.csv", row.names = FALSE)





















