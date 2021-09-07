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
SatData1 = read_stars("C:/Users/rhartman/Desktop/OLCI_202105_Mosaic_CIcyano/sentinel-3a.2021121.0501.L3.CA_mosaic.v950V20193_1_2.CIcyano.tif")
estuary = extent(WW_Delta)
Delta = st_transform(WW_Delta, crs = st_crs(SatData1))
#for unknown reasons, they have coded "NA" with numbers. Very dub.

str(SatData1)
SatData1[SatData1 == "255"] = NA
SatData1[SatData1 == "254"] = NA
SatData1[SatData1 == "253"] = NA
SatData1[SatData1 == "252"] = NA
SatData1[SatData1 == "251"] = NA
SatDatacrop = st_crop(SatData1, Delta)

ggplot() + geom_stars(data = SatDatacrop)
