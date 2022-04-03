
library(tidyverse)
library(deltamapr)

EFISH_data <- read_csv("data/2018-2020_USFWS_EFISH_data.csv")

str(EFISH_data)


#where have they sampled?

EFISHstas = group_by(EFISH_data, RegionCode, Subregion, SampleDate, GPS_Start_Latitude, GPS_Start_Longitude) %>%
  summarize(n = n())

EFISHsf = st_as_sf(EFISHstas, coords = c("GPS_Start_Longitude", "GPS_Start_Latitude"), crs = st_crs(4326))
ggplot() +geom_sf(data = WW_Delta)+ geom_sf(data = EFISHsf, aes(color = Subregion))+
  coord_sf(xlim = c(-121.8, -121.3), ylim = c(37.9, 38.5))+
  scale_color_viridis_d()
