#Special Studies Drought Synthesis
#Drought Indices

#required packages
library(tidyverse)

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
  #create a month-year column
  unite('date',month:year,sep="-",remove=F) %>% 
  #format month-year column
  mutate(date,as.Date("%Y-%m-%d")) %>% 
  glimpse()
  
#plots-------------
  
#plot correlations among indices within districts
#plot correlations of same index between districts
  
  
  
  
  
  
  
