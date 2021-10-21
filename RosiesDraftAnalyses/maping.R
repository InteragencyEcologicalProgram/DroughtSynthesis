library(deltamapr)
library(tidyverse)
library(ggmap)
library(sf)

cdec = read.csv("CDEC_StationsEC.csv")
cdecsf = st_as_sf(cdec, coords = c("Longitude", "Latitude"), crs = 4326)


#import list of regions with enough data for analysis
library(readr)
Rosies_regions <- read_csv("RosiesDraftAnalyses/Rosies_regions.csv")
FLOATlong = left_join(Rosies_regions, R_EDSM_Subregions_Mahardja_FLOAT) %>%
  filter(Long_term == TRUE) %>%
  st_as_sf()

#plot of all the regions and all the cdec stations
ggplot()+
  geom_sf(data = WW_Delta)+
  geom_sf(data = cdecsf)+
  geom_sf(data = R_EDSM_Subregions_Mahardja_FLOAT,
          aes(fill=SubRegion), alpha = 0.3)+
  theme_bw()+
  theme(legend.position="none")+
    coord_sf(xlim = c(-122.2, -121.2), ylim = c(37.6, 38.6))

#plot of all the cdec stations and a subset of the regions
ggplot()+
  geom_sf(data = WW_Delta)+
  geom_sf(data = cdecsf)+
  geom_sf(data = FLOATlong,
          aes(fill=SubRegion), alpha = 0.2)+
  theme_bw()+
  theme(legend.position="none")+
  coord_sf(xlim = c(-122.2, -121.2), ylim = c(37.6, 38.6))

#plot of the regional averages (centroisds)
centroids = st_centroid(FLOATlong)

ggplot()+
  geom_sf(data = WW_Delta)+
  geom_sf(data = centroids, aes(fill = SubRegion), shape = 21, 
          colour = "black", size = 5, stroke = 1)+
  geom_sf(data = FLOATlong,
          aes(fill=SubRegion), alpha = 0.1)+
  theme_bw()+
  theme(legend.position="none")+
  coord_sf(xlim = c(-122.2, -121.2), ylim = c(37.6, 38.6))

#Import LSZ for each year and season
LSZ = read_csv("RosiesDraftAnalyses/Low_salinity_zone.csv")

#make an example (Fall 2000)
Fall2000 = filter(LSZ, Season == "Fall", Year == 2000)

LSZregions = left_join(Fall2000, FLOATlong) %>%
  st_as_sf()
LSZcent = st_centroid(LSZregions)

ggplot()+
  geom_sf(data = WW_Delta)+
  geom_sf(data = LSZcent, aes(fill = SubRegion), shape = 21, 
          colour = "black", size = 5, stroke = 1)+
  geom_sf(data = LSZregions,
          aes(fill=SubRegion), alpha = 0.1)+
  theme_bw()+
  theme(legend.position="none")+
  coord_sf(xlim = c(-122.2, -121.2), ylim = c(37.6, 38.6))

################################################################
#region assignments

stations = read_csv("data/AllIEPstations_20200220.csv") %>%
  filter(!is.na(Latitude))
stas = st_as_sf(stations, coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_transform(crs = st_crs(R_EDSM_Subregions_Mahardja_FLOAT))

join = st_join(stas, R_EDSM_Subregions_Mahardja_FLOAT) %>%
  mutate(Region = NULL) %>%
  st_drop_geometry()

Regs = read_csv("RosiesDraftAnalyses/Rosies_regions2.csv") %>%
  select(Region, SubRegion) %>%
  unique()

staswregs = left_join(join, Regs) %>%
  left_join(stations)
write.csv(staswregs, "AllIEP_wRegions.csv")
