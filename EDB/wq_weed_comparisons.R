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
library(ggcorrplot) #plotting correlation matrix
library(DEGreport) #adds corr and p to plots
library(lubridate) #formatting dates

#read in the data-------------------------

#summary stats for each sonde parameter (July - Oct)
wq <- read_csv("EDB/frk_sonde_data_summary.csv")

#aquatic weed rake data
veg <- read_csv("https://raw.githubusercontent.com/InteragencyEcologicalProgram/AquaticVegetationPWT/main/MasterDataSet_SAV/Data_Formatted/FranksTractManagement_2014-2021_formatted.csv")

#look at correlations in abundances among species------------------

#reshape the data frame so each row is a sample and each column is a species
#keep station, date, species, score
veg_wide <- veg %>% 
  #first need to drop the handful of visual observations
  #so they don't create duplicates and mess up the pivot_wider
  filter(survey_method!="visual") %>% 
  select(date,station,species,rake_coverage_ordinal) %>% 
  pivot_wider(
    id_cols=c(date,station)
    ,names_from = species
    , values_from=rake_coverage_ordinal
    #was getting warnings about duplicates before removing visual spp
    #,values_fn = list(rake_coverage_ordinal = length)
    ) %>% 
  glimpse()

#create correlation matrix
corr_matrix <- round(cor(veg_wide[3:17]),2)

# Computing correlation matrix with p-values
corrp_matrix <- cor_pmat(veg_wide[3:17])

# Visualizing the correlation matrix
ggcorrplot(corr_matrix, method ="square", type="lower")

# Visualizing the correlation matrix within indication of whether p-values are significant
#and correlation coefficients printed on plot
ggcorrplot(corr_matrix, method ="square", type="lower", p.mat = corrp_matrix, lab=T)
#strongest correlation is 0.24 (POCR vs PONO), indicating that all are very weak correlations (despite some sig. p-values)

#Egeria densa vs Richardson's pondweed
#second strongest correlation of all (-0.21)
#these are two of the most dominant SAV spp
ggplot(veg_wide, aes(Egeria_densa,Potamogeton_richardsonii))+ 
  geom_smooth(method = "lm")  + 
  geom_point() +
  geom_cor(method = "pearson")
  

#format veg data----------------

#calculate mean and se for abundances (rake_coverage_ordinal) for each species by year
#note that there is a row for every species for every sample
#meaning there are lots of zeros for abundance scores
#this is good because those absences should be part of the summary stat calculations
#a number of the specie will likely be too rare for evaluating

#standard error function
se <- function(x) sd(x)/sqrt(length(x))

veg_clean <- veg %>% 
  #remove visual only records
  filter(survey_method!="visual") %>% 
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


















