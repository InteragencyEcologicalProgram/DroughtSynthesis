#Emergency Drought Barrier
#look at correlations among species abundances
#also look at correlations between water quality and aquatic weed abundance
#water quality data are from FRK sonde station
#aquatic weed data are from the SePro annual rake surveys

#notes
#will use discrete EMP WQ data but haven't yet incorporated into this code
#instead of FRK sonde which starts 2015 would be better to use Bethal Island sonde 
#or EMP discrete which start earlier
#keep in mind that number of rake samples varies among year
#usually about 100 per year but as low as 50 and as high as 200

#packages
library(tidyverse)
library(ggcorrplot) #plotting correlation matrix
library(DEGreport) #adds corr and p to plots
library(lubridate) #formatting dates

#read in the data-------------------------

#read in discrete wq data and delta outflow (2004-2021)
wqd <- read_csv("EDB/discrete_wq&outflow_data_summary.csv")

#summary stats for each FRK sonde parameter (July - Oct)
#slightly less useful because doesn't start until 2015, one year later than SAV data set
wq <- read_csv("EDB/frk_sonde_data_summary.csv")

#EMP discrete data

#aquatic weed rake data
veg <- read_csv("https://raw.githubusercontent.com/InteragencyEcologicalProgram/AquaticVegetationPWT/main/MasterDataSet_SAV/Data_Formatted/FranksTractManagement_2014-2021_formatted.csv")

#read in fluridone application data (2006-2021)
herb <- read_csv("EDB/franks_tract_fluridone_applications_2006-2021_summary.csv")

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

#look at correlations between discrete water quality, herbicides, delta outflow vs vegetation abundance---------------

#format env driver data set
wqd_clean <- wqd %>% 
  filter(Station=="EMP D19" & year > 2013) %>% 
  glimpse()

herb_format <- herb %>% 
  select(-fl_rate_ppb) %>% 
  glimpse()

#combine wq + outflow with fluridone
drivers <- left_join(wqd_clean,herb_format) %>% 
  select(-c(Salinity,Station))

#combine veg and drivers
vegd <- left_join(veg_clean,drivers) %>% 
  select(-score_se) %>% 
  #make df longer
  pivot_longer(cols=c(Temperature:fl_quantity_kg),names_to = "parameter",values_to = "value") %>% 
  select(species,year,parameter,score_mean,value) %>% 
  arrange(species,parameter) %>% 
  glimpse()

#create nested data frame
#a data frame of species abundances for each driver
#vegd_nest <- vegd %>% 
#  group_by(parameter) %>% 
#  nest() 
  #start looking at some relationships
#  mutate(model = map(data, function(df) cor.test(score_mean ~ Temperature, data = df)))

library(broom)
library(tidyr)
library(dplyr)

#generate a correlation for the abundances of the four most abundant species vs env driver  
#note that data are ordinal so use spearman instead of pearson 
#https://stackoverflow.com/questions/61184133/calculate-bulk-pair-wise-correlation-using-purrr-and-nested-data-frame
output <- vegd %>% 
  filter(species == "Ceratophyllum_demersum" | species == "Egeria_densa" | species== "Potamogeton_richardsonii"| species =="Najas_guadalupensis") %>% 
    group_by(species, parameter) %>% 
    do(tidy(cor.test(.$score_mean,.$value,method="spearman")))

#format results for export
output_ex <- output %>%
  #just keep needed columns
  select(species,parameter, estimate,p.value) %>% 
  #drop fluridone kg predictor
  filter(parameter!="fl_quantity_kg") %>% 
  #make wider
  pivot_wider(id_cols=c(parameter),names_from = species,values_from = c(estimate,p.value)) %>% 
  arrange(parameter)

#reorder columns
outex <- output_ex[c(1,2,6,3,7,4,8,5,9)]
#write_csv(outex,"EDB/sepro_env_driver_corrs.csv")


#now just look at the ones with p<0.05
#this is a lot of test; should I adjust the p-values?
#might be good to reduce the number of predictors and spp (just most common ones)
output_sig <- output %>% 
  filter(p.value< 0.05)
#only six of 120 tests were significant

#do a couple correlations individually to make sure the big batch of analyses worked correctly
vegd_ng <- vegd %>% 
  filter(species == "Najas_guadalupensis")
cor.test(vegd_ng$score_mean,vegd_ng$pH,method="spearman")
cor.test(vegd_ng$score_mean,vegd_ng$Conductivity,method="spearman")
#these results do match with the bulk analyses df results

#create panel of plots of significant correlations-----------
#only four of them

#coontail vs secchi

coon <- vegd %>% 
  filter(species =="Ceratophyllum_demersum" & parameter=="Secchi")

(cs <- ggplot(coon, aes(x=value, y=score_mean))+ 
  geom_smooth(method = "lm")  + 
  geom_point() +
  geom_text(aes(label=year)
            , vjust = -0.9
  )+
  #geom_cor(method = "spearman")+
  xlab("Annual mean Secchi depth (m)")+
  ylab("Mean abundance score")+
    ggtitle("Ceratophyllum demersum")+
    theme_bw()
)

#naiad vs EC and ph

ndc <- vegd %>% 
  filter(species =="Najas_guadalupensis" & parameter=="Conductivity" )

(nc <- ggplot(ndc, aes(x=value, y=score_mean))+ 
    geom_smooth(method = "lm")  + 
    geom_point() +
    geom_text(aes(label=year)
              , vjust = -0.9
    )+
    #geom_cor(method = "spearman")+
    xlab("Annual mean conductivity (uS/cm)")+
    ylab("Mean abundance score")+
    ggtitle("Najas guadalupensis")+
    theme_bw()
)

ndp <- vegd %>% 
  filter(species =="Najas_guadalupensis" & parameter=="pH")

(np <- ggplot(ndp, aes(x=value, y=score_mean))+ 
    geom_smooth(method = "lm")  + 
    geom_point() +
    geom_text(aes(label=year)
              , vjust = -0.9
    )+
    #geom_cor(method = "spearman")+
    xlab("Annual mean pH")+
    ylab("Mean abundance score")+
    ggtitle("Najas guadalupensis")+
    theme_bw()
)

#egeria and ph
edp <- vegd %>% 
  filter(species =="Egeria_densa" & parameter=="pH")

(ep <- ggplot(edp, aes(x=value, y=score_mean))+ 
    geom_smooth(method = "lm")  + 
    geom_point() +
    geom_text(aes(label=year)
              , vjust = -0.9
    )+
    #geom_cor(method = "spearman")+
    xlab("Annual mean pH")+
    ylab("Mean abundance score")+
    ggtitle("Egeria densa")+
    theme_bw()
)

sfigure <- ggarrange(nc, np, cs,ep,
                     #labels = c("A", "B"),
                     ncol = 2, nrow = 2)
#ggsave(plot=sfigure, "EDB/Sepro_Driver_SignCorrPanel.png",type ="cairo-png",width=8, height=8,units="in",dpi=300)



#look at correlations between sonde water quality and vegetation abundance---------------

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
#none of corr are significant; all p> or =0.1

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


















