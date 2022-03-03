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
sav_n <- sav %>% 
  distinct(station,date) %>% 
  group_by(date) %>% 
  summarize(n = n())
#all years have 100 samples except for two 
#2016 = 45, 2015 = 200
#should randomly subset 2015 to 100 samples
#could exclude 2016 or just let the design be a bit unbalanced

#create random subset of 100 rake samples from the 200 collected in 2015

#start by grabbing the 2015 sampling data
sav15 <- sav %>% 
  #filter data set to just 2015 samples
  filter(date=="2015-10-13") 

#choose which stations to keep using random numbers
sav15r <- sav15 %>% 
  #condense to just the list of unique stations
  distinct(station) %>% 
  #add column of randomly generated numbers
  mutate(random = sample(200, size = nrow(.), replace = F)) %>% 
  #just keep rows assigned to 1-100
  filter(random >0 & random < 101)

#subset the 2015 data based on random numbers
sav15sb <- left_join(sav15r,sav15) %>% 
  select(-random)

#remove the original full set of 2015 data from main data set
sav_no15 <- sav %>% 
  filter(date!="2015-10-13")

#add the subsetted 2015 data to main data set
sav_sub <- bind_rows(sav_no15,sav15sb)

#make sure there are now 100 instead of 200 samples for 2015
sav_n2 <- sav_sub %>% 
  distinct(station,date) %>% 
  group_by(date) %>% 
  summarize(n = n())
#looks like it worked

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
  summarize(freq = n()) %>% 
  arrange(freq)
#let's drop the spp with fewer than ten detections
unique(sav_spp_sum$species)
#five rare taxa removed in code below

#format data set as matrix
sav_wide <- sav %>% 
  #add year column
  mutate(year = as.factor(year(sav$date))) %>% 
  filter(
    #remove the small number of visual only samples
    survey_method!="visual"
    #remove rake samples with no SAV; can't have these in analysis
    & sav_incidence==1
    ) %>%
  #remove rare taxa (fewer than 10 detections ever)
  filter(species!="Heteranthera_dubia" & species!="Nitella_sp" & species!="Potamogeton_nodosus"      
         & species!="Potamogeton_pusillus"  & species!= "Potamogeton_zosteriformis") %>% 
  #filter(species=="Potamogeton_richardsonii" | species== "Ceratophyllum_demersum" | species== "Egeria_densa" | species== "Najas_guadalupensis") %>% 
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
#range(sav_data_sqrt) #0.000000 2.236068
#makes sense because raw scores are 0 to 5

#PERMANOVA using adonis function
#decide if bray is the best method; I know I've also used horn
permanova_sav<-adonis2(sav_data_sqrt ~ year, data=sav_env
                       #set number of permutations
                       ,permutations=999
                       #each marginal term analyzed in a model with all other variables
                       #,by = "margin"
                       ,method="bray"
                       )
permanova_sav #year is significant p < 0.001

#plot the results

#perform analysis of multivariate homogeneity of group dispersions
#to determine if the multivariate scatter is similar among years
#this is an assumption of PERMANOVA
#if not similar among years, this can results in misleading significant p-values
sav_dist <- vegdist(sav_data_sqrt, method="bray")
dispersion <- betadisper(sav_dist, group=sav_env$year)
permutest(dispersion) 
#p<0.001, so we failed the test (ie, dispersions are not homogeneous)
#so we don't know if centroids or dispersions caused the significant year effect

plot(dispersion, hull=FALSE, ellipse=TRUE) ##sd ellipse
