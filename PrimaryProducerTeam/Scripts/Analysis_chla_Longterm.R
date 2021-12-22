## Long term drought synthesis analysis code for Chlorophyll A

## Libraries
library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
source("Scripts/MyFunctionsAndThemes.R")

## Load data frames from Data_format.R
load("Data/DS_dataframesLT.Rdata")



#### DATA FILTERING ####
## Filter data based on
# Minimum samples per year per site
# Minimum number of years sampled at a site
# Regions to exclude from the analysis
# Seasons to include in the analysis
# Minimum result threshold
chla_data_filt_list <- filter_chla_data(data= DS_chlaLT,
                                        min_samps_yr = 12,
                                        min_yrs= 10,
                                        excluded_regions = c("Far West"),
                                        seasons= c("Summer", "Fall", "Spring", "Winter"),
                                        min_result = 0)


chla_data_filt <- chla_data_filt_list$data
chla_stations_filt.sf <- chla_data_filt_list$stations














ggplot() +
  geom_sf(data= DS_regions) + 
  geom_sf(data= DS_waterways) +
  geom_sf(data= chla_stationsLT.sf) +
  coord_sf()
