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
#note: there is no data for partial regions. shruti already excluded those

#Don't worry about regions. Just plot the annual data for SAV and FAV 
#color bars by water year type

#packages
library(tidyverse)
library(janitor)
library(deltamapr)
library(sf)

#just plot data by year--------

#script with formatting for plots
source("PrimaryProducerTeam/Scripts/MyFunctionsAndThemes.R")

#read in veg data
veg <- read_csv("PrimaryProducerTeam/Data/AquaticVegCoverage_2004-2020.csv")

#read in water year assignments
wyt <- read_csv("PrimaryProducerTeam/Data/WaterYearAssignments.csv")

#reduce water year df to just needed columns
wyts <- wyt %>% 
  select("Year","Yr_type")

#add water year assignments to veg data
vegwy <- left_join(veg,wyts, by=c("year"="Year"))

#convert wide to long
vyl <- vegwy %>% 
  #filter(year>2010) %>% 
  rename(WY = Yr_type) %>% 
  pivot_longer(cols="SAV":"FAV", names_to="veg_type", values_to="area_perc") 

#color scheme
year.colors <- c( "Critical" = "#FDE333", "Dry" = "#53CC67", "Below Normal" = "#009B95","Above Normal" = "#00588B", "Wet" = "#4B0055")

#plot with SAV and FAV for both full delta and north + central delta
ggplot(vyl, aes(x=year, y=area_perc)) +
  geom_bar(stat="identity", color = "black", aes(fill= WY)) +
  facet_rep_wrap(region~veg_type, ncol= 2) +
  labs(x= "Year", y= "Percent water area occupied") +
  scale_fill_manual(values= year.colors
                    #, guide= "none"
                    ) +
  theme_doc +
  theme(legend.position = "top")
#full delta plot looks same as north + central delta plot, so just use north + central data because there are more years of data

#plot with SAV and FAV for just north + central delta
nc <- vyl %>% 
  filter(region=="North + Central Delta")

ggplot(nc, aes(x=year, y=area_perc)) +
  geom_bar(stat="identity", color = "black", aes(fill= WY)) +
  facet_rep_wrap(~veg_type, ncol= 2) +
  labs(x= "Year", y= "Percent water area occupied") +
  scale_fill_manual(values= year.colors
                    #, guide= "none"
  ) +
  theme_doc +
  theme(legend.position = "top")
#ggsave(last_plot(), filename= "AquaticVegTimeSeries.png", width= 6.5, height= 6, dpi= 300,
#       path= "Figures")


#look at data by region-----------------------

#Define path on SharePoint site for data
sharepoint_path <- normalizePath(
  file.path(
    Sys.getenv("USERPROFILE"),
    "California Department of Water Resources/Drought Synthesis - Data"
  )
) 

#read in the SAV data from sharepoint
savdat<-read_csv(file = paste0(sharepoint_path,"./SAV_2014-2020_ByRegion.csv")) %>% 
  clean_names() #use janitor package to improve header formatting
#looks like column types are fine 

#read in the regions data from sharepoint
regdat<-read_csv(file = paste0(sharepoint_path,"./Rosies_regions.csv")) %>% 
  clean_names() %>%  #use janitor package to improve header formatting
  glimpse()
#looks like column types are fine 

#format the region data set
#reduce to two needed columns and just the unique combos of them
reg_ft <- regdat %>% 
  distinct(region,sub_region)
  
#format the sav data set
#just drop the old region column
savdata <- savdat %>% 
  select(-region)

#combine sav data with new regions
svrg <- left_join(savdata,reg_ft)

#now look at unique combinations of region and subregion in the sav data set
reg_unq <- svrg %>% 
  distinct(region,sub_region)
#two subregions completely missing from my data set
#also noticed four subregions with regions (check for subregion updates)

#look at month range
unique(savdata$month) 

#look at number of years with all regions
reg_count<-savdata %>% 
  group_by(year) %>% 
  summarize(count = n()) 
#years with all regions: 2014, 2015, 2019, 2020

#focus on subset of regions with data across all years
#most years, only portions of north and central delta were imaged

#make table summarizing sample sizes by sub_region
#look for sub_regions with data for all 7 years
samp_count<-savdata %>% 
  group_by(sub_region) %>% 
  summarize(count = n()) 
#11 of 28 sub_regions have data for all years

#reduce data set to just the subset of subregions available for all seven years
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

#data analysis

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

#Report hypotheses
#Distribution shifts upstream (not sure we can look at this)
#Increased total coverage (can look at this a little bit qualitatively)
#Changed Species composition (can't look at this for SAV)
#Increased Herbicide applications (don't have these data)


#look at regions in data set
unique(savdata$region)
#only three regions: "North" "South" "West"
#looks like regions have changed so import new ones and match with subregions
#are any of these regions even complete in our data set?
                  


