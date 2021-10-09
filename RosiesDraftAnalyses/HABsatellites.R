#HAB barrier analysis
#need to look at satalite data and microcystis visual assessments

library(tidyverse)
library(raster)
library(stars)
library(exactextractr)
library(ggmap)
library(deltamapr)

#first let's get the satilite data from NOAA
#data from here: https://fhab.sfei.org/
SatData1 = read_stars("C:/Users/rhartman/Desktop/OLCI_202107_Mosaic_CIcyano/sentinel-3a.2021210.0729.L3.CA_mosaic.v950V20193_1_2.CIcyano.tif")
estuary = extent(WW_Delta)
Delta = st_transform(WW_Delta, crs = st_crs(SatData1))
#for unknown reasons, they have coded "NA" with numbers. Very dumb.

str(SatData1)
#255 is "No Data"
SatData1[SatData1 == "255"] = NA
#254 is "No Data (invalid)"
SatData1[SatData1 == "254"] = NA
#253 is "No Data (Cloud)"
SatData1[SatData1 == "253"] = NA
#252 is "No Data (Land)
SatData1[SatData1 == "252"] = NA
#251 is "No Data (Adjacency)"
SatData1[SatData1 == "251"] = NA

#
SatDatacrop = st_crop(SatData1, st_bbox(Delta))

SatDattest = SatDatacrop
SatDattest= mutate(SatDattest, across(everything(), as.numeric)) %>%
  mutate(CyanoHABs = sentinel.3a.2021210.0729.L3.CA_mosaic.v950V20193_1_2.CIcyano.tif,
         sentinel.3a.2021209.0728.L3.CA_mosaic.v950V20193_1_2.CIcyano.tif = NULL) %>%
  st_transform(st_crs(4326))

Delta = st_transform(Delta, st_crs(4326))

ggplot() +
  geom_stars(data = SatDattest)+
  scale_fill_viridis_b(name ="Cyanohabs")+ 
  geom_sf(data = Delta, alpha = 0, size = 0.1)+
  coord_sf(xlim = c(-121.8, -121.3), ylim = c(37.8, 38.2))
