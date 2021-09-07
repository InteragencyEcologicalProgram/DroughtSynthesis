#2021 Drought synthesis
#Short term analysis data set
#Submerged aquatic vegetation

#start looking at drought vs non-drought years

#Nick Rasmussen, nicholas.rasmussen@water.ca.gov

#challenges to this data set:
#only seven years of data during 2011-2021
#only four of those years have data for entire delta
#data are only collected once per year
#time of year of data collection varies a bit too

#packages
library(tidyverse)
library(janitor)
library(deltamapr)
library(sf)

#Define path on SharePoint site for data
sharepoint_path <- normalizePath(
  file.path(
    Sys.getenv("USERPROFILE"),
    "California Department of Water Resources/Drought Synthesis - Component data sets"
  )
) 

#read in the data from sharepoint
savdata<-read_csv(file = paste0(sharepoint_path,"./SAV_2014-2020_ByRegion.csv")) %>% 
  clean_names() #use janitor package to improve header formatting
#looks like column types are fine 

#look at number of years with all regions
reg_count<-savdata %>% 
  group_by(year) %>% 
  summarize(count = n()) 
#years with all regions: 2014, 2015, 2019, 2020

#focus on subset of regions with data across all years
#most years, only portions of north and central delta were imaged
#are there cases where area imaged only covered part of an EDSM region?
#ie, artifically reducing SAV area because part of region missing

#make table summarizing sample sizes by sub_region
#look for sub_regions with data for all 7 years
samp_count<-savdata %>% 
  group_by(sub_region) %>% 
  summarize(count = n()) 
#11 of 28 sub_regions have data for all years

#combine sample sizes with main data frame
savdatsz <- left_join(savdata,samp_count) %>% 
#then remove sub_regions without data for all years
filter(count==7) %>% 
#reduce data frame to just needed columns
select(
  "year"
  ,"sub_region"
  ,"region"
  , "sav_hect"
  ,"water_hect"
  ,"sav_perc_wa" 
)

#plot %SAV coverage by subregion
#again just those subregions with data for all years
(plot_sav_subr <-ggplot(savdatsz, aes(x=year, y=sav_perc_wa))+ 
    geom_line() + 
    geom_point()+
    facet_wrap(~sub_region, nrow=3)
)

#look at regions from deltamapr

ggplot(R_EDSM_Subregions_Mahardja)+
  geom_sf(aes(fill=SubRegion))+
  theme_bw()+
  theme(legend.position="none")

#data analysis ----------------

#water year type categories
#Wet years: 2011, 2017, 2019
#Drought years, no barrier: 2013, 2014, 2020
#Drought years, w/barrier: 2015, 2021
#“in between” years (below normal, may not include in the analysis): 2012, 2016, 2018

#We don’t have enough replication to use year-type as a predictor in the model, 
#so we will instead look to see whether conditions are significantly different one year at a time.
#For example, if 2015 and 2021 are not significantly different from each other, 
#but both are different from 2020, it may be due to the presence of the Drought barrier. 

#suggested analysis formula
#Metric ~ factor(year) + month + region + (1|Subregion)
#aquatic veg has one value per year so no month
#only seven years of data, and data for full system only for four years

#look at regions in data set
unique(savdata$region)
#only three regions: "North" "South" "West"
                  


