#Map for synthesis paper

library(sf)
library(tidyverse)
library(DroughtData)
library(readxl)


library(cowplot)

library(ggsn)
library(deltamapr)
load("DroughtRegions.RData")

#move region labels outside boxes
Regions = mutate(Regions, nudge = c(-.1, .1, -.1, -.09, .09))

#add points for Sacramento, Stockton, Martinez, and the CVP and SWP pumps

Points =read_excel("data/points.xlsx")
Points = st_as_sf(Points, coords = c("Longitude", "Latitude"), crs = 4326)

ggplot()+
  geom_sf(data = WW_Delta)+
  geom_sf(data = Regions,
          aes(fill=Region), alpha = 0.2)+
  geom_sf(data = filter(Points, Type %in% c("POI", "City", "POI2021")), color = "darkred", size = 4)+
  theme_bw()+
  theme(legend.position="none")+
  scale_shape_manual(values = c(15, 16, 17, 8))+
  scalebar( y.min = 37.8, y.max = 38.6, x.min = -122.2, x.max = -121.2, 
            transform = TRUE, dist = 10, st.size = 4,
            dist_unit = "km", model = "WGS84", location = "bottomleft") +
  north(y.min = 37.8, y.max = 38.6, x.min = -122.2, x.max = -121.2,  symbol = 2) +
  theme_bw()+ylab("")+xlab("")+
  scale_fill_discrete(guide = NULL)+
   geom_sf_label(data = Regions, aes(label = Region), 
                 label.size = 0.05,
                 label.padding = unit(0.1, "lines"),
                 fontface = "bold")+
  geom_sf_text(data = filter(Points, Type %in% c("POI", "City", "POI2021")), 
               aes(label = Label), nudge_x = 0.02, nudge_y = -0.02, fontface = "bold")+
  coord_sf(xlim = c(-122.2, -121.2), ylim = c(37.7, 38.6))

ggsave("plots/whitepapermap.pdf", device = "pdf", width = 6, height = 6)
Yolo = st_read("data/Flood_Bypasses/Flood_Bypasses.shp") %>%
  filter(NAME == "Yolo Bypass")

synthmap = ggplot()+
  geom_sf(data = WW_Delta, fill = "lightgrey", color = "lightgrey", alpha = .5)+
  geom_sf(data = Yolo, fill = "darkgrey", alpha = 0.2)+
  geom_sf(data = Regions,
          aes(fill=Region), alpha = 0.2)+
  geom_sf(data = filter(Points, Label != "Barrier", Type != "flow", Type != "Island"), aes(color = Type), size = 4)+
  
  scale_color_manual(values = c("darkred", "darkblue", "darkgreen", "purple", "lightblue"), guide = NULL)+
  theme_bw()+
  theme(legend.position="none")+
  scale_shape_manual(values = c(15, 16, 17, 8))+
  scalebar( y.min = 37.8, y.max = 38.6, x.min = -122.2, x.max = -121.2, 
            transform = TRUE, dist = 10, st.size = 4,
            dist_unit = "km", model = "WGS84", location = "bottomleft") +
  north(y.min = 37.8, y.max = 38.6, x.min = -122.2, x.max = -121.2,  symbol = 2) +
  theme_bw()+ylab("")+xlab("")+
  scale_fill_discrete(guide = NULL)+
  geom_sf_label(data = Regions, aes(label = Region), 
                label.size = 0.05,
                label.padding = unit(0.1, "lines"),
                fontface = "bold")+
  geom_sf_text(data =  filter(Points, Type == "City"), aes(label = Label), nudge_x = 0.02, nudge_y = -0.02, fontface = "bold", size = 5)+
  geom_sf_text(data =  filter(Points, Type == "POI"), aes(label = Label), nudge_x = 0.01, nudge_y = -0.01, size = 3)+
  geom_sf_text(data =  filter(Points, Type == "Island"), aes(label = Label), nudge_x = 0.01, nudge_y = -0.01, fontface = "italic", size = 3)+
  
  coord_sf(xlim = c(-122.2, -121.2), ylim = c(37.7, 38.6))
synthmap
ggsave("plots/synthmap.tiff", device = "tiff", width = 6, height = 6)
ggsave("plots/synthmap.pdf", device = "pdf", width = 7, height = 7)

####################################################################
#Map for fish paper.
FMWT = read_csv("data/FMWT_Station_Locations.csv") %>%
  filter(IndexYN == "Y") %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

DJFMP = read_csv("data/DJFMP_Site_Locations.csv") %>%
  filter(Location %in% c("Chipps Island", "Sherwood Harbor"), StationCode %in% c("SR055M", "SB018M"))%>%
st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

ggplot()+
  geom_sf(data = WW_Delta, fill = "lightskyblue")+
  theme_bw()+
  geom_sf(data = FMWT, aes(shape = "FMWT"))+
  geom_sf(data = DJFMP, size = 3, shape = 24, aes(fill = Location))+
  scale_fill_manual(values = c("yellow", "green"), name = NULL)+
  scale_shape_manual(values = 19, name = NULL)+
  theme(legend.position="none")+
  scalebar( y.min = 37.9, y.max = 38.6, x.min = -122.2, x.max = -121.2, 
            transform = TRUE, dist = 10, st.size = 4,
            dist_unit = "km", model = "WGS84", location = "bottomleft") +
  north(y.min = 37.8, y.max = 38.4, x.min = -122.5, x.max = -121.2,  symbol = 2) +
  theme_bw()+ylab("")+xlab("")+

  coord_sf(xlim = c(-122.5, -121.2), ylim = c(37.9, 38.6))

ggsave("plots/Fishmap.tiff", device = "tiff", width = 10, height = 5)

ggsave("plots/Fishmap.pdf", device = "pdf", width = 10, height = 5)

######################################################################
#map for water quality paper

library(DroughtData)
library(deltamapr)
library(dataRetrieval)

stas = raw_wq_1975_2021 %>%
  group_by(Source, Station, Latitude, Longitude) %>%
  summarize(n = n()) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>%
  filter(!Station %in% c("EZ2", "EZ6", "EZ2-SJR", "EZ6-SJR"))


siteNumbers <- c(
  'USGS-11455350', # CACHE SLOUGH A RYER ISLAND (inactive)
  'USGS-11455385', # CACHE SLOUGH AB RYER ISLAND FERRY NR RIO VISTA CA (active)
  'USGS-11337190', # SAN JOAQUIN R A JERSEY POINT CA
  'USGS-11313405', # OLD R A BACON ISLAND CA
  'USGS-11312676' # MIDDLE R AT MIDDLE RIVER CA
)

vel_coords <- dataRetrieval::whatWQPsites(siteid = siteNumbers)%>%
  select(MonitoringLocationIdentifier, LatitudeMeasure, LongitudeMeasure) 
vels = st_as_sf(vel_coords, coords = c("LongitudeMeasure", "LatitudeMeasure"), crs = 4326)


wqmap = ggplot()+
  geom_sf(data = WW_Delta, fill = "lightskyblue", color = "grey")+
  geom_sf(data = Regions,
          aes(fill=Region), alpha = 0.2)+
  geom_sf_label(data = Regions,
          aes(label = Region), position = position_nudge(y=Regions$nudge))+
  theme_bw()+
  geom_sf(data = stas, aes(shape = Source))+
  scale_shape_manual(values = c(15,16,17,18,22,23,24,25,11,1, 2), name = "Discrete \nSamples")+    
  geom_sf(data = vels, shape = 16, size = 4,aes(color = "Continous\nVelocity"))+
  scale_color_discrete(name = NULL)+
   theme(legend.position="none")+

scale_fill_viridis_d(option = "H", guide = NULL)+
  scalebar( y.min = 37.8, y.max = 38.6, x.min = -122.2, x.max = -121.2, 
            transform = TRUE, dist = 10, st.size = 4,
            dist_unit = "km", model = "WGS84", location = "bottomleft") +
  north(y.min = 37.8, y.max = 38.4, x.min = -122.2, x.max = -121.2,  symbol = 2) +
  theme_bw()+ylab("")+xlab("")+
  
  coord_sf(xlim = c(-122.2, -121.2), ylim = c(37.8, 38.6))

wqmap


########################################################################
#map of california for inset

library(tigris)
us_states = states(cb = FALSE, class = "sf")
Cal = filter(us_states, NAME == "California")

#bounding box of area of interest
deltabb = st_as_sfc(st_bbox(Regions))

ggm3 = ggplot() + 
  geom_sf(data = Cal, fill = "white", size = 0.2) + 
  geom_sf(data = WW_Watershed, fill = "white", color = "grey")+
  geom_sf(data = deltabb, fill = NA, color = "blue", size = 1.2) +
  theme(axis.title = element_blank(), plot.background = element_rect(fill = "white"),
        axis.ticks = element_blank(), axis.text = element_blank())

ggm3

wqmapinste= ggdraw() +
  draw_plot(wqmap) +
  draw_plot(ggm3, x = 0.1, y = 0.65, width = 0.2, height = 0.2)

wqmapinste
ggsave("plots/WQmap.tiff", device = "tiff", width = 8, height = 8)

synthinste= ggdraw() +
  draw_plot(synthmap) +
  draw_plot(ggm3, x = 0.2, y = 0.65, width = 0.2, height = 0.3)
synthinste

ggsave("plots/synmap2.pdf", device = "pdf", width = 8, height = 8)

################################################################
#blank map for concenptual model



conmap= ggplot()+
  geom_sf(data = WW_Delta, fill = "lightskyblue", color = "grey")+
  geom_sf(data = Regions,
          aes(fill=Region), alpha = 0.2)+
  geom_sf_label(data = Regions,
                aes(label = Region), position = position_nudge(y=Regions$nudge))+
  theme_bw()+
  theme(legend.position="none")+
  
  scale_fill_viridis_d(option = "H", guide = NULL)+
  theme_void()+ylab("")+xlab("")+
  
  coord_sf(xlim = c(-122.2, -121.2), ylim = c(37.8, 38.6))
conmap
