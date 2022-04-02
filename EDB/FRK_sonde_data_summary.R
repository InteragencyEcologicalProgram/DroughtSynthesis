#Emergency Drought Barrier
#FRK sonde water quality data
#calculate annual means and standard errors
#use to determine if aquatic weed species abundance correlate with water quality
#based on SePro vegetation surveys conducted annually in October

#packages
library(tidyverse) #suite of data science tools
library(lubridate) #formatting dates
library(ggcorrplot) #plotting correlation matrix
library(DEGreport) #adds corr and p to plots

#read and combine all WQ data files
wq_list <- dir(path = "EDB/FRK_data" ,pattern = "\\.csv", full.names = T, recursive=T)
frk <- map_dfr(wq_list, ~read_csv(.x)) %>% 
  glimpse()

#look at date ranges
#includes data from station installation to Nov 2021
range(frk$time) #"2015-07-01 00:00:00 UTC" "2021-11-20 23:45:00 UTC"
#Note that annual means for 2015 and (temporarily) 2021 will be based on fewer data 
#so they will be skewed
#could drop 2015 or just use July 1- October 1 data for all years
#that would be the mean of the four months of data right before the veg surveys
#which probably works fine

#look at data quality categories
unique(frk$qaqc_flag_id)
#"G" "M" "X" "A" "U"
#A = added filler data
#G = Good
#M = Missing
#U = Unchecked
#I don't know what X means

#how many measurements of each type?
qual_ct<-frk %>% 
  group_by(parameter,qaqc_flag_id) %>% 
  summarize(count = n())  
#vast majority are G so I think we can just use those for our purposes

#filter out bad data
frk_clean <- frk %>% 
    mutate(
    #create month column
    month = as.integer(month(time))
    #create a year column
    ,year = as.integer(year(time))
    #make parameter a factor
    ,parameter = as.factor(parameter)
         ) %>% 
  #only keep the data categorized as good and data during months July to Sept
  filter(qaqc_flag_id=="G" & (month > 6 & month < 10)) %>% 
  #only keep needed columns
  select(parameter,year,month,time,value,unit) %>% 
  glimpse()

#plot time series of remaining data by parameter
#this takes a while to run because of the data set size
#ggplot(frk_clean, aes(x=time, y =value))+
 # geom_point()+
  #geom_line()+
  #facet_wrap(~parameter, scales = "free")

#look at correlations among the six parameters---------------
#a work in progress

#start with frk df
#remove the bad data
#convert long to wide (a column for each parameter)
#make corr matrix
#plot corr matrix

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


#calculate annual means and standard errors

#standard error function
se <- function(x) sd(x)/sqrt(length(x))

frk_sum <- frk_clean %>% 
  group_by(parameter,year) %>% 
  summarize(value_mean = mean(value)
            ,value_se = se(value)
            )

#plot the summary data
ggplot(frk_sum, aes(x=year, y =value_mean))+
  geom_errorbar(aes(ymin=value_mean-value_se, ymax=value_mean+value_se), width = 0.2) +
  geom_point()+
  geom_line()+
  facet_wrap(~parameter, scales = "free")

#write summary stats file 
#write_csv(frk_sum,"EDB/frk_sonde_data_summary.csv")

