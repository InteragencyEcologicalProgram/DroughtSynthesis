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

#read in shape file with Franks Tract (and Mildred Island)
sf_franks_mildred <- read_sf("EDB/Spatial_data/Franks_Mildred.shp")

#format data set-----------

#website with EPSG codes for CRS
#https://spatialreference.org/

cstars_format <- cstars %>% 
  #specify the crs which is wgs84
  st_as_sf(coords = c(x='longitude_wgs84',y='latitude_wgs84'), 
           crs = 4326
           ,remove=F #retains original columns
           ) %>%   #EPSG code for WGS84
  glimpse()

#Sampling maps-------------

#Note: NAD83 and WGS84 are highly similar and perhaps indistinguishable
#this explains why transformations between them appear to do nothing
#https://www.esri.com/arcgis-blog/products/arcgis-desktop/mapping/wgs84-vs-nad83/

#look at WW_Delta base map CRS
st_crs(WW_Delta)
#CRS = NAD83, which is different than our sample data points; EPSG: 4269
WW_Delta_4326 <- st_transform(WW_Delta, crs = 4326)


#create map showing all Delta SAV data points
(sav_map_all <- ggplot()+
  #plot waterways base layer
  geom_sf(data= WW_Delta_4326, fill= "skyblue3", color= "black") +
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
    geom_sf(data= WW_Delta_4326, fill= "skyblue3", color= "black") +
    #plot SAV sampling points
    geom_sf(data= cstars_format, fill= "red", color= "black", shape= 22, size= 3.5)+
    #set bounding box for site
    #Box picks up a few unneeded sampling over in Taylor Slough 
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
    geom_sf(data= WW_Delta_4326, fill= "skyblue3", color= "black") +
    #plot SAV sampling points
    geom_sf(data= cstars_format, fill= "red", color= "black", shape= 22, size= 3.5)+
    #set bounding box for site
    #No stray samples from outside sites captured in this box
    coord_sf( 
      xlim =c(-121.740, -121.685),
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
    geom_sf(data= WW_Delta_4326, fill= "skyblue3", color= "black") +
    #plot SAV sampling points
    geom_sf(data= cstars_format, fill= "red", color= "black", shape= 22, size= 3.5)+
    coord_sf( 
      xlim =c(-121.605, -121.552),
      ylim = c(37.867, 37.818)
    )+
    theme_bw()+
    ggtitle("Clifton Court")
)

#create subsets of data sets by site---------

#Look at CRS for Franks Tract/Mildred Island shape file 
st_crs(sf_franks_mildred) 
#already in the right CRS (WGS84), which is EPSG 4326

#plot the shape files
(map_ft_md <- ggplot()+
    geom_sf(data= sf_franks_mildred, fill= "skyblue3", color= "black") 
)

# Create a bounding box of the Franks-Mildred shapefile which will be used to crop the satellite data
# and extend it by 5% on each side
bbox_fr_mil <- st_bbox(sf_franks_mildred)
bbox_fr_mil_xrange <- bbox_fr_mil$xmax - bbox_fr_mil$xmin
bbox_fr_mil_yrange <- bbox_fr_mil$ymax - bbox_fr_mil$ymin


# Prepare HAB satellite data for maps
franks <- cstars_format %>% 
  mutate(
    strs_obj_f =
      # Crop HAB satellite data to bounding box of the Franks-Mildred shapefile
      map(strs_prx_obj, ~st_crop(.x, bbox_fr_mil)
          
          
franks <- cstars_format %>% 
  filter(
    coord_sf( 
    xlim =c(-121.740, -121.685),
    ylim = c(38.031, 38.005)
  )
    )
