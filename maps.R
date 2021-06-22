#maps

library(tidyverse)
library(deltamapr)
library(ggmap)
library(sf)


#Import CDEC stations
cdec = read.csv("CDEC_StationsEC.csv")
cdecsf = st_as_sf(cdec, coords = c("Longitude","Latitude"), crs = 4326)

ggplot()+
  geom_sf(data = WW_Delta)+
  geom_sf(data =R_EDSM_Subregions_Mahardja_FLOAT, 
          aes(fill=SubRegion), alpha = .2)+
  geom_sf(data = cdecsf)+
  theme(legend.position="none")+ guides(fill = FALSE)+
  theme_bw()
