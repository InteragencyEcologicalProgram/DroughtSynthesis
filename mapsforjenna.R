#Maps of HAB sites for JEnna

#Rosemary Harmtna
#4/8/2022

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

#If you don't have 'deltamapr', run these lines:
# install.packages("devtools")
#devtools::install_github("InteragencyEcologicalProgram/deltamapr")

#Import file of incident reports
incidents = read_excel("data/HABs/Legal Delta HAB incidents 2016-2021.xlsx")

#turn the data points into a geographic shape file
#also turn the "advisory level" into a factor
incsf = st_as_sf(incidents, coords = c("Longitude", "Latitude"), crs = 4326) %>%
  rename(Advisory = `Initial Advisory Level`) %>%
  dplyr::filter(Advisory != "No Advisory") %>%
  mutate(Advisory = factor(Advisory, levels = c("Caution",  "Warning","Danger"), labels = c("Caution",  "Warning","Danger")),
         Year = year(`Incident date`))

#now make a quick map
ggplot()+
  geom_sf(data = WW_Delta, color = "grey", fill = "lightblue")+ 
  geom_sf(data = incsf, aes(fill = Advisory), shape = 21, color = "black", size = 3)+
  scale_fill_manual(values = c("yellow",  "orange", "red"), labels = c("Caution", "Warning","Danger"))+
  theme_bw()+
  scale_x_continuous(limits = c(-121.9, -121.2)) +
  scale_y_continuous( limits = c(37.65, 38.4))+
  facet_wrap(~Year)  

#This data was all the public reports, and some of the regular monitoring surveys started
#also additing to it in 2021, so it's not great for cross-year comparisons.

###################################################################################
#Now let's add teh warnings derrived from actual toxin data

load("Alltoxindata.RData")


#add warning levels from toxin samples we have

levels = filter(Alltoxsf, Analyte == "Microcystins") %>%
  mutate(Advisory = case_when(result > 0.8 & result < 6 ~ "Caution",
                              result >= 6 & result < 20 ~ "Warning",
                              result >= 20 ~ "Danger"),
         Advisory2 = case_when(result > 0.8 & result < 6 ~ 1,
                               result >= 6 & result < 20 ~ 2,
                               result >= 20 ~ 3)) %>%
  filter(!is.na(Advisory)) %>%
  group_by(Station) %>%
  mutate(Max = max(Advisory2), Year = 2021) %>%
  dplyr::select(Max, Station, Year) %>%
  distinct()%>%
  mutate(Advisory = factor(Max, levels = c(1,  3), labels = c("Caution",
                                                              "Danger")))

#add them to the public-contributed data (limit to just 2021)
incsf2 = bind_rows(incsf, levels) %>%
  filter(Year == 2021)

ggplot()+
  geom_sf(data = WW_Delta, color = "grey", fill = "lightblue")+ 
  geom_sf(data = incsf2, aes(fill = Advisory), shape = 21, color = "black", size = 3)+
  scale_fill_manual(values = c("yellow",  "red"), labels = c("Caution", "Danger"))+
  theme_bw()+
  scale_x_continuous(limits = c(-121.9, -121.2)) +
  scale_y_continuous( limits = c(37.65, 38.4))
  
#####################################################################################

#Map of HAB sampling stations

#Load all of the visual index data
load("HABs.RData")

#Turn it into a spatial data frame
HABssf   = HABs %>%
  filter(!is.na(Longitude), !is.na(Date)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

#plot it

test = filter(HABssf, Microcystis >1)

timePlot = ggplot(data = HABssf)+
  geom_sf(data = WW_Delta, color = "grey", fill = "lightblue")+ 
  geom_sf( aes(color = Microcystis))+
  scale_color_viridis_b(option = "B")+
  theme_bw()+
  labs(title = 'Date: {frame_time}') +
  scale_x_continuous(limits = c(-122.4, -121.2)) +
  scale_y_continuous( limits = c(37.65, 38.4))+
  shadow_wake(wake_length = 0.1)+
  transition_time(Date)

animate(timePlot, height = 500, width = 800, fps = 10, duration = 20,
        end_pause = 60, res = 100)
anim_save("HABs.gif")

ggplot(data = filter(HABssf, Date < as.Date("2017-02-01")))+
  geom_sf(data = WW_Delta, color = "grey", fill = "lightblue")+ 
  geom_sf( aes(color = Microcystis))+
  scale_color_viridis_b(option = "B")+
  theme_bw()+
  labs(title = 'Date: {frame_time}') +
  scale_x_continuous(limits = c(-122.4, -121.2)) +
  scale_y_continuous( limits = c(37.65, 38.4))
