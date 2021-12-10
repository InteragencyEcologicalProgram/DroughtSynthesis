#2021 Emergency Drought Barrier
#2021 CSTARS ground truthing data
#Entire Delta
#Submersed aquatic vegetation point data
#focus on Franks Tract, Big Break, and Clifton Court

#Nick Rasmussen
#nicholas.rasmussen@water.ca.gov

# Packages--------
library(tidyverse) #suite of data science tools
library(sf) #tools for making maps
library(deltamapr) #Sam's package with shapefiles for delta waterways

# Read in the data----------------------------------------------
# Data set is on SharePoint site for the 
# Delta Smelt Resiliency Strategy Aquatic Weed Control Action
# I synced this folder to my OneDrive
sharepoint_path_read <- normalizePath(
  file.path(
    Sys.getenv("USERPROFILE"),
    "California Department of Water Resources/DWR - DSRS Aquatic Weed Control Action - MasterDataSet_SAV/Clean&Formatted"
  )
) 

#read in data
cstars <- read_csv(file = paste0(sharepoint_path_read,"./CSTARS_2021_formatted.csv"))
glimpse(cstars) #looks good. just need to make lat/long into geometry

#format data set
cstars_format <- cstars %>% 
  #specify the crs which is wgs84
  st_as_sf(coords = c(x='longitude_wgs84',y='latitude_wgs84'), 
           crs = 4236) %>%   #EPSG code for WGS84
  glimpse()

#create map showing all Delta SAV data points
(sav_map_all <- ggplot()+
  #plot waterways base layer
  geom_sf(data= WW_Delta, fill= "skyblue3", color= "black") +
  #plot SAV sampling points
  geom_sf(data= cstars_format, fill= "red", color= "black", shape= 22, size= 1.5)+
    coord_sf( 
      xlim =c(-121.870, -121.251),
      ylim = c(38.570, 37.801)
    )+
    theme_bw()

)

#create map showing Franks Tract SAV data points
(sav_map_ft <- ggplot()+
    #plot waterways base layer
    geom_sf(data= WW_Delta, fill= "skyblue3", color= "black") +
    #plot SAV sampling points
    geom_sf(data= cstars_format, fill= "red", color= "black", shape= 22, size= 3.5)+
    coord_sf( 
      xlim =c(-121.677, -121.576),
      ylim = c(38.07, 38.02)
    )+
    theme_bw()+
    ggtitle("Franks Tract")
)

#create map showing Big Break SAV data points
(sav_map_bb <- ggplot()+
    #plot waterways base layer
    geom_sf(data= WW_Delta, fill= "skyblue3", color= "black") +
    #plot SAV sampling points
    geom_sf(data= cstars_format, fill= "red", color= "black", shape= 22, size= 3.5)+
    coord_sf( 
      xlim =c(-121.740, -121.679),
      ylim = c(38.031, 38.005)
    )+
    theme_bw()+
    ggtitle("Big Break")
)

#create map showing Clifton Court SAV data points
#no sampling in Clifton Court
#not too surprising because access is restricted
(sav_map_cc <- ggplot()+
    #plot waterways base layer
    geom_sf(data= WW_Delta, fill= "skyblue3", color= "black") +
    #plot SAV sampling points
    geom_sf(data= cstars_format, fill= "red", color= "black", shape= 22, size= 3.5)+
    coord_sf( 
      xlim =c(-121.605, -121.552),
      ylim = c(37.867, 37.818)
    )+
    theme_bw()+
    ggtitle("Clifton Court")
)












