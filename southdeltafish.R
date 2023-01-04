#Are there fish in the South Delta?

library(tidyverse)
library(lubridate)
library(deltafish)

#data manipulation libraries
library(tidyverse)
library(readxl)
library(lubridate)

#GIS libraries
library(ggmap)
library(deltamapr)
library(sf)

#these ones for animated plots
library(gganimate)
library(gifski)
library(transformr)
library(patchwork)

fishes = create_fish_db()
fish = open_fish()
surv = open_survey()
survsub = filter(surv, Source %in% c("FMWT", "STN")) %>%
  left_join(fish) %>%
  collect()

notfish = c("Aequorea", "Aurelia aurita", "Chrysaora fuscescens", "Cnidaria", "Crangon", "Decapoda", "Maeotias marginata",
            "Palaemon", "Palaemon kadiakensis", "Palaemon modestus", "Polyorchis penicillatus", "Scrippsia pacifica", "Blackfordia virginica",
            "Cancer magister", "Exopalaemon modestus", "Palaemon macrodactylus")

#total count by taxa (get rid of zeros for now)
FMTN = group_by(survsub, Station, Latitude, Longitude, Date, SampleID, Source, Taxa) %>%
  filter(Count != 0, !Taxa %in% notfish) %>%
  summarize(count = sum(Count))

#total catch (just fish) by trawl
FMTN2 = filter(survsub, !Taxa %in% notfish) %>%
  group_by(Station, Latitude, Longitude, Date, SampleID, Source) %>%
  summarize(count = sum(Count))

FMTNsf = st_as_sf(filter(FMTN2, !is.na(Latitude)), coords = c("Longitude", "Latitude"), crs = 4326)

#Annimated plot
timePlot = ggplot(data = FMTNsf)+
  geom_sf(data = WW_Delta, color = "grey", fill = "lightblue")+ 
  geom_sf( aes(color = count))+
  scale_color_viridis_b(option = "B")+
  theme_bw()+
  labs(title = 'Date: {frame_time}') +
  scale_x_continuous(limits = c(-122.4, -121.2)) +
  scale_y_continuous( limits = c(37.65, 38.4))+
  shadow_wake(wake_length = 0.1)+
  transition_time(Date)

animate(timePlot, height = 500, width = 800, fps = 20, duration = 100,
        end_pause = 60, res = 100)
anim_save("HABs.gif")



##########################################################
#now let's just do average catch per station for the past 20 years

fishave = filter(FMTN2, Date > as.Date("2000-01-01")) %>%
  group_by(Station, Latitude, Longitude) %>%
  summarize(Count = mean(count)) %>%
  filter(!is.na(Latitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

ggplot(data = fishave)+
  geom_sf(data = WW_Delta, color = "grey", fill = "lightblue")+ 
  geom_sf( aes(size = Count, color = Count))+
  scale_color_viridis_b(option = "B")+
  theme_bw()+
  scale_x_continuous(limits = c(-122.4, -121.2)) +
  scale_y_continuous( limits = c(37.65, 38.4))
  
#Now just taxa from our our paper

test = filter(survsub, Taxa %in% c("Morone saxatilis", "Oncorhynchus tshawytscha",
                                                  "Dorosoma petenense", "Alosa sapidissima",
                                                  "Hypomesus transpacificus", "Spirinchus thaleichthys"),
                          Date > as.Date("2000-01-01"))
PODave =  group_by(test, Station, Latitude, Longitude, Date, SampleID, Source, Taxa) %>%
                   summarize(count = sum(Count)) %>%
  group_by(Station, Latitude, Longitude, Taxa) %>%
  summarize(Count = mean(count)) %>%
  filter(!is.na(Latitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

ggplot(data = PODave)+
  geom_sf(data = WW_Delta, color = "grey", fill = "lightblue")+ 
  geom_sf( aes(size = Count, color = Count))+
  scale_color_viridis_b(option = "B")+
  scale_size_binned(breaks = c(0, 5, 10, 50, 120), range = c(0, 10))+
  theme_bw()+
  scale_x_continuous(limits = c(-122.4, -121.2)) +
  scale_y_continuous( limits = c(37.65, 38.4)) +
  facet_wrap(~Taxa)

#I'd like to be able to varry the size scale by species

fishplot = function(Species, df) {
  df2 = filter(df, Taxa == Species)
  
  ggplot(data = df2)+
    geom_sf(data = WW_Delta, color = "grey", fill = "lightblue")+ 
    geom_sf( aes(size = Count, color = Count))+
    scale_color_viridis_b(option = "B")+
    theme_bw()+
    scale_x_continuous(limits = c(-122.4, -121.2)) +
    scale_y_continuous( limits = c(37.65, 38.4)) +
    facet_wrap(~Taxa)
}

MS = fishplot("Morone saxatilis", PODave)
OT = fishplot("Oncorhynchus tshawytscha", PODave)
DP = fishplot("Dorosoma petenense", PODave)
AS = fishplot("Alosa sapidissima", PODave)
HT = fishplot("Hypomesus transpacificus", PODave)
ST = fishplot("Spirinchus thaleichthys", PODave)

MS+OT+DP+AS+HT+ST
