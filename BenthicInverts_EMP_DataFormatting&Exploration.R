#Drought Synthesis
#Special Studies Project
#Environmental Monitoring Program data
#Benthic Invertebrate Survey
#1975 - October 2020
#Catch per unit effort (CPUE)
#Organisms/m2 = (sum per visit /# of grabs per visit)/(0.052 grabs/meter squared)

#required packages
library(tidyverse)
library(janitor)
library(hms)
library(readxl) #importing data from excel files
library(waterYearType) #lists all water year types 1901 - 2017

#read in data--------------------

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
           
#Water year type from waterYearType package
#the only function in package brings in water year type data frame
water_year <- water_year_indices
#glimpse(water_year) #looks like column types are correct


#convert abundance data frame from wide to long

#add water year type to abundance data from

#convert taxonomy data frame from wide to long and combine with abundance data frame

#look at histograms of frequencies by species

#look at histogram of total abundances by species

#look at which species are most abundant in wet years vs Critical years
#also look at Rosie's proposed definitions for multi-year periods of drought vs. wet
#also look at Betsy's manuscript

#create definition for rare taxa and exclude them from data set




















