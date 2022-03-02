#Emergency Drought Barrier
#SePro SAV surveys
#conduct PERMANOVA analysis on SAV community
#look for differences among years

#load packages
library(tidyverse)
library(lubridate)
library(vegan)

#read in data - veg, wq, herbicide use

#sepro sav data
sav <- read_csv("https://raw.githubusercontent.com/InteragencyEcologicalProgram/AquaticVegetationPWT/main/MasterDataSet_SAV/Data_Formatted/FranksTractManagement_2014-2021_formatted.csv")

#PERMANOVA assumes roughly balances design
#if looking at effect of year, then make sure years have similar sample sizes
#I know they're generally 100 rakes per year but vary 50-200
sav_n <- sav %>% 
  distinct(station,date) %>% 
  group_by(date) %>% 
  summarize(n = n())
#all years have 100 samples except for two 
#2016 = 45, 2015 = 200
#should randomly subset 2015
#could exclude 2016 or just let the design be a bit unbalanced

#look at distribution of scores by species using histograms
ggplot(sav, aes(x = rake_coverage_ordinal) )+
         geom_histogram()+
         facet_wrap(.~species)

#look at rare taxa and consider removing them
#Note that these numbers will likely be affected by 2015 subsampling
sav_spp_sum <- sav %>% 
  #drop the visual samples and any abundances of zero
  filter(survey_method=="rake_weighted" & rake_coverage_ordinal!=0) %>% 
  group_by(species) %>% 
  summarize(freq = n())  
  filter(freq < 10)
#let's drop the spp with fewer than ten detections
unique(sav_spp_sum$species)
#five rare taxa removed in code below

#format data set as matrix
sav_wide <- sav %>% 
  #add year column
  mutate(year = as.factor(year(sav$date))) %>% 
  #remove the small number of visual only samples
  filter(survey_method!="visual") %>%
  #remove rare taxa (fewer than 10 detections ever)
  filter(species!="Heteranthera_dubia" & species!="Nitella_sp" & species!="Potamogeton_nodosus"      
         & species!="Potamogeton_pusillus"  & species!= "Potamogeton_zosteriformis") %>% 
  #convert long to wide
  pivot_wider(id_cols=c(station,year,date)
              , names_from = species
              , values_from = rake_coverage_ordinal) 

#create df with just the environmental predictors
sav_env<- sav_wide %>% 
  select(year) %>% 
  glimpse()

#create df with just subset of columns that contain species abundance data
sav_data<-sav_wide %>% 
  select(Egeria_densa:Myriophyllum_spicatum) %>% 
  glimpse()

#square root transform the abundance data
#is there a more appropriate transformation to use for ordinal data?
#also does the function do this automatically? check documentation
sav_data_sqrt<-sqrt(sav_data)
range(sav_data_sqrt) #0.000000 2.236068
#makes sense because raw scores are 0 to 5

#PERMANOVA using adonis function
#this currently isn't working. need to figure out why
permanova_sav<-adonis2(sav_data_sqrt ~ year, data=sav_env
                       #set number of permutations
                       ,permutations=999
                       #each marginal term analyzed in a model with all other variables
                       #,by = "margin"
                       ,method="bray"
                       )
permanova_sav


