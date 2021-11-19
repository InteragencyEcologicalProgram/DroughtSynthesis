#Special Studies Drought Synthesis
#Drought Indices

#required packages
library(tidyverse)
library(lubridate) #working with date-time
library(PerformanceAnalytics) #plotting correlations

#to do
#could calculate mean values for the two relevant divisions
#or could just use Sac drainage because it contributes more water to the system

#read in the data----------
#used read.table because it worked better than tidyverse options for dealing with variable 
#numbers of spaces between columns

#palmer drought severity index (05)
pdsi <- read.table(file="https://www.ncei.noaa.gov/pub/data/cirs/climdiv/climdiv-pdsidv-v1.0.0-20211104"
                   ,header=F
                   )

#palmer hydrological drought index (06)
phdi <- read.table(file="https://www.ncei.noaa.gov/pub/data/cirs/climdiv/climdiv-phdidv-v1.0.0-20211104"
                   ,header=F
                   )

#palmer "Z" index (07)
zndx <- read.table(file="https://www.ncei.noaa.gov/pub/data/cirs/climdiv/climdiv-zndxdv-v1.0.0-20211104"
                   ,header=F
                   )

#modified palmer drought severity index (08)
pmdi <- read.table(file="https://www.ncei.noaa.gov/pub/data/cirs/climdiv/climdiv-pmdidv-v1.0.0-20211104"
                   ,header=F
                   )

#format data------------

#combine data sets for all indices
indices <- bind_rows(pdsi,phdi,pmdi,zndx)

#steps to format and filter data set
indices_cleaner <-indices %>% 
  #rename headers
  rename(division_index_year = V1
         ,jan = V2
         ,feb = V3
         ,mar = V4 
         ,apr = V5
         ,may = V6
         ,jun = V7
         ,jul = V8
         ,aug = V9 
         ,sep = V10
         ,oct = V11
         ,nov = V12
         ,dec = V13
         ) %>% 
  #split up info in first column based on character position
  separate(division_index_year,c("division","index","year"),sep=c(3,5)) %>%
  #filter to just the two divisions needed
  #Sacramento drainage is 402 and San Joaquin drainage is 405
  filter(division == "402" | division =="405") %>% 
  #change df from wide to long
  pivot_longer(cols=jan:dec, names_to = "month", values_to = "value") %>% 
  #create a month-year column and format as date type
  mutate(date1 = paste(month, year),
         date = my(date1)
  ) %>% 
  #subset and reorder columns
  select("division", "index", "date", "value" ) %>% 
  #filter out missing data (-99.90 or -99.99)
  filter(value > -99) %>% 
  glimpse()
 
#make a wide version for correlation plots
indices_w <- indices_cleaner %>% 
  pivot_wider(names_from=index
              ,names_prefix = "index_"
              ,values_from=value) 

#make an even wider version for correlation plots
indices_ww <- indices_w %>% 
  pivot_wider(names_from = division
              ,names_prefix = "div_"
              ,values_from = index_05:index_07)
  
#plots-------------

#make faceted plot showing each index for each division
(plot_di <-ggplot(indices_cleaner, aes(x=date, y= value))+
   geom_bar(stat = "identity") + 
   ylab("Index") + xlab("Date") + 
   facet_wrap(index~division,nrow = 4)
)
#should replace numbers with division and index names
#also add horizontal lines showing categories of wet/dry

#plot correlations among indices within districts

#Sacramento
sac_within <- indices_w %>% 
  filter(division == 402) 

#plot
chart.Correlation(sac_within[3:6])

#correlations
cor(sac_within[3:6])

#San Joaquin
sj_within <- indices_w %>% 
  filter(division == 405) 

#plot
chart.Correlation(sj_within[3:6])

#correlations
cor(sj_within[3:6])

#plot correlations of same index between districts

#plots
chart.Correlation(indices_ww[2:9])

#index 05
cor(indices_ww[2:3])  #0.7790742

#index 06
cor(indices_ww[4:5])  #0.8211686

#index 07
cor(indices_ww[8:9]) #0.8188015

#index 08
cor(indices_ww[6:7]) #0.8319748

  
  
  
  
  
  
