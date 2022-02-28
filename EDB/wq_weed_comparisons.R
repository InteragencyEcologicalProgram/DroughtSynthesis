#Emergency Drought Barrier
#look at correlations among species abundances
#also look at correlations between water quality and aquatic weed abundance
#water quality data are from FRK sonde station
#aquatic weed data are from the SePro annual rake surveys

#notes
#keep in mind that number of rake samples varies among year
#usually about 100 per year but as low as 50 and as high as 200

#packages
library(tidyverse)
#library(PerformanceAnalytics) #plotting correlations
library(DEGreport) #adds corr and p to plots

#read in the data-------------------------

#summary stats for each sonde parameter (July - Oct)
wq <- read_csv("EDB/frk_sonde_data_summary.csv")

#aquatic weed rake data

# Define path on SharePoint site for data
# I synced this folder to my OneDrive
dir_path <- normalizePath(
  file.path(
    Sys.getenv("USERPROFILE"),
    "California Department of Water Resources/DWR - DSRS Aquatic Weed Control Action - MasterDataSet_SAV/Clean&Formatted"
  )
)  

veg <- read_csv(file = paste0(dir_path,"./FranksTractManagement_2014-2021_formatted.csv"))

#format veg data----------------

#calculate mean and se for abundances (rake_coverage_ordinal) for each species by year
#note that there is a row for every species for every sample
#meaning there are lots of zeros for abundance scores
#this is good because those absences should be part of the summary stat calculations
#a number of the specie will likely be too rare for evaluating

#standard error function
se <- function(x) sd(x)/sqrt(length(x))

veg_clean <- veg %>% 
  mutate(
    #create a year column
    year = as.integer(year(date))
    ) %>% 
  group_by(species,year) %>% 
  summarize(score_mean = mean(rake_coverage_ordinal)
            ,score_se = se(rake_coverage_ordinal)
  ) %>% 
  glimpse()

#look at correlations between water quality and vegetation abundance---------------

#join water quality and vegetation data frames

#first double check year ranges of both data sets
range(wq$year) #2015 to 2021
range(veg_clean$year) #2014 to 2021, so there's one year (2014) of veg data we can't use

wv <- left_join(wq,veg_clean)

#make plots showing correlations between each species and water quality parameter
#figure out how to create all these panels with one set of code
#check code for AWCA phyto plots where I think I did this once

#subset of data for just specific conductance data
vsc <- wv %>% 
  filter(parameter=="SpC")

#plot year vs SC faceted by species
ggplot(vsc, aes(value_mean,score_mean))+ 
  geom_smooth(method = "lm")  + 
  geom_point() +
  geom_text(aes(label=year)
            , vjust = -0.9
  )+
  geom_cor(method = "pearson")+
  xlab("Mean Specific Conductance")+
  ylab("Mean Vegetation Abundance Score ") +
  facet_wrap(~species,nrow=3) 
#none of corr are significant

#subset of data for just temperature data
vtp <- wv %>% 
  filter(parameter=="WaterTemperature")

#plot year vs temp faceted by species
ggplot(vtp, aes(value_mean,score_mean))+ 
  geom_smooth(method = "lm")  + 
  geom_point() +
  geom_text(aes(label=year)
            , vjust = -0.9
  )+
  geom_cor(method = "pearson")+
  xlab("Mean Water Temperature")+
  ylab("Mean Vegetation Abundance Score ") +
  facet_wrap(~species,nrow=3) 
#none of corr are significant though Egeria is p=0.06

#subset of data for just turbidity data
vtb <- wv %>% 
  filter(parameter=="Turbidity")

#plot year vs turbidity faceted by species
ggplot(vtb, aes(value_mean,score_mean))+ 
  geom_smooth(method = "lm")  + 
  geom_point() +
  geom_text(aes(label=year)
            , vjust = -0.9
  )+
  geom_cor(method = "pearson")+
  xlab("Mean Turbidity")+
  ylab("Mean Vegetation Abundance Score ") +
  facet_wrap(~species,nrow=3) 
#none of corr are significant though coontail is p=0.07

#subset of data for just DO data
vdo <- wv %>% 
  filter(parameter=="DissolvedOxygen")

#plot year vs DO faceted by species
ggplot(vdo, aes(value_mean,score_mean))+ 
  geom_smooth(method = "lm")  + 
  geom_point() +
  geom_text(aes(label=year)
            , vjust = -0.9
  )+
  geom_cor(method = "pearson")+
  xlab("Mean DO")+
  ylab("Mean Vegetation Abundance Score ") +
  facet_wrap(~species,nrow=3) 
#none of corr are significant though milfoil is p=0.07

#subset of data for just ph data
vph <- wv %>% 
  filter(parameter=="pH")

#plot year vs ph faceted by species
ggplot(vph, aes(value_mean,score_mean))+ 
  geom_smooth(method = "lm")  + 
  geom_point() +
  geom_text(aes(label=year)
            , vjust = -0.9
  )+
  geom_cor(method = "pearson")+
  xlab("Mean pH")+
  ylab("Mean Vegetation Abundance Score ") +
  facet_wrap(~species,nrow=3) 
#a few of the rare species have p values just above or below 0.05

#subset of data for just Fluorescence data
vfl <- wv %>% 
  filter(parameter=="Fluorescence")

#plot year vs Fluorescence faceted by species
ggplot(vfl, aes(value_mean,score_mean))+ 
  geom_smooth(method = "lm")  + 
  geom_point() +
  geom_text(aes(label=year)
            , vjust = -0.9
  )+
  geom_cor(method = "pearson")+
  xlab("Mean Fluorescence")+
  ylab("Mean Vegetation Abundance Score ") +
  facet_wrap(~species,nrow=3) 
#a few of the uncommon/rare species have p values just above or below 0.05


















