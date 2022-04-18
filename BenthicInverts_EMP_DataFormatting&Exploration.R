#Drought Synthesis
#Special Studies Project
#Environmental Monitoring Program data
#Benthic Invertebrate Survey data
#1975 - October 2020
#Catch per unit effort (CPUE)
#Organisms/m2 = (sum per visit /# of grabs per visit)/(0.052 grabs/meter squared)

#to do list
#add water year type to abundance data (note that invert data categorized by calendar year)
#create map showing all stations, active and historic

#required packages
library(tidyverse)
library(janitor)
library(hms)
library(readxl) #importing data from excel files
library(waterYearType) #lists all water year types 1901 - 2017

#read in data from EDI-----------------------

#station data
stn <- read_csv("https://portal.edirepository.org/nis/dataviewer?packageid=edi.1036.1&entityid=4e6948186ad756dc2b6de4de41b601f3")

#benthic invert CPUE


#read in data from sharepoint--------------------

#Define path on SharePoint site for data
sharepoint_path <- normalizePath(
  file.path(
    Sys.getenv("USERPROFILE"),
    "California Department of Water Resources/Drought Synthesis - Component data sets"
  )
)  

#read in data from excel file
#separate the taxonomy in the headers from the abundance data
#eventually combine these into one data frame

#first the abundances
abundance <- read_excel(path = paste0(sharepoint_path,"/BenthicInverts_EMP_CPUE_1975-Oct2020_20210511.xlsx")
                        #specify sheet and cell range
                        , range = "75-20 CPUE m2!A8:PD4534"
                        , col_names = T
                        )
#glimpse(abundance) #looks like columns were categorized correctly when imported


#then the taxonomy
taxonomy <- read_excel(path = paste0(sharepoint_path,"/BenthicInverts_EMP_CPUE_1975-Oct2020_20210511.xlsx")
                        #specify sheet and cell range
                       , range = "75-20 CPUE m2!E2:PD8"
                       , col_names = F
                       )
#glimpse(taxonomy)

#then station data
stations <- read_excel(path = paste0(sharepoint_path,"/BenthicInverts_EMP_CPUE_1975-Oct2020_20210511.xlsx")
                       #specify sheet 
                       , sheet = "75-19 station locations"
                       , col_names = T
)
#glimpse(stations) #column types look good
           
#Water year type from waterYearType package
#the only function in package brings in water year type data frame
water_year <- water_year_indices
#glimpse(water_year) #looks like column types are correct

#format and combine data sets----------

#convert abundance data frame from wide to long
#names(abundance)
abund <- abundance %>% 
  pivot_longer(cols = "2970":"1093"
               ,names_to = "sp_code"
               , values_to = "cpue") %>% 
  clean_names() #from janitor package
#glimpse(abund) #column types look correct

#convert taxonomy data frame from wide to long 
#simple approach is probably just a transpose
taxon <- data.frame(t(taxonomy)) %>% 
  rename(phylum = X1
         ,class = X2
         ,order = X3
         ,family = X4
         ,genus = X5
         ,species = X6
         ,sp_code = X7
  )
#glimpse(taxon) #column types look correct

#join abundance and taxonomy data
sp_abund <-left_join(abund,taxon)

#explore station data---------------
#almost no stations that cover full time period starting in 1975 to present

stns <- stations %>% 
  #simplify names
  clean_names() %>% #from janitor package
  #some more column name changes
  rename(station_code = site_code #to match column in abundance df
         ,start = "period_of_record_from"
         ,end = "period_of_record_to")

#replace "Present" with current year
stns$end2<-str_replace_all(stns$end, "Present", "2020")
#need to decide how to categorize stations in a useful way
#mostly should focus on active stations 
#but there are some historical stations with many years of data



#explore abundance data---------------

#range of dates
range(sp_abund$date) #"1975-05-19 UTC" "2020-10-20 UTC"

#look at stations
unique(sp_abund$station_code) #53 stations

#look at histograms of frequencies by species

#look at histogram of total abundances by species

#look at which species are most abundant in wet years vs Critical years
#also look at Rosie's proposed definitions for multi-year periods of drought vs. wet
#also look at Betsy's manuscript

#create definition for rare taxa and exclude them from data set




















