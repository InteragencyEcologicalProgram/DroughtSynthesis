#Emergency Drought Barrier
#look at correlations between water quality and aquatic weed abundance
#water quality data are from FRK sonde station
#aquatic weed data are from the SePro annual rake surveys

#packages
library(tidyverse)

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
#need to make a year column first
#note that there is a row for every species for every sample
#meaning there are lots of zeros for abundance scores
#this is good because those absences should be part of the summary stat calculations
#a number of the specie will likely be too rare for evaluating



















