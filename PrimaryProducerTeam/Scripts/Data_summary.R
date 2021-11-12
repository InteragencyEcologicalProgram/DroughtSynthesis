## Summarize data to help determine what stations and samples we can use


library(tidyverse)
source("Scripts/Data_format.R")
source("Scripts/ggplot_themes.R")

chla_station_list.sf


#### DATA SUMMARY ####

## Number of stations per region
chla_station_subregion_summary <- count(st_drop_geometry(chla_station_list.sf), SubRegion) %>%
  full_join(., st_drop_geometry(DS_regions)[, c("SubRegion", "Region")]) %>% # Add in regions without any stations
  mutate(n= ifelse(is.na(n), 0, n)) %>% # Give regions without stations a count (n) = 0
  arrange(-n)

## Summarize chla data
chla_station_summary <- DS_data %>%
  group_by(Source, Station, year) %>%
  summarize(count_chla= length(chla)) %>%
  ungroup() %>%
  group_by(Source, Station) %>%
  mutate(start_year= min(year),
         end_year= max(year),
         n_years= length(year))

## Wide format
chla_station_summary.w <- chla_station_summary %>%
  pivot_wider(names_from = year, values_from= count_chla) %>%
  mutate(across(`2011`:`2019`, ~ ifelse(is.na(.x), 0, .x))) # change NAs to zeros

#### DATA FILTERING ####
min_samples_per_year <- 12
regions_to_remove <- c("Far West")

chla_stations_filt <- chla_station_summary %>%
  filter(count_chla >= min_samples_per_year)

DS_data_filt <- DS_data %>%
  filter(Station %in% chla_stations_filt$Station) %>%
  filter(!(Region %in% regions_to_remove))

chla_stations_filt.sf <- chla_stations.sf %>%
  filter(Station %in% chla_stations_filt$Station)


## Number of stations per region
chla_station_filt_subregion_summary <- count(st_drop_geometry(chla_stations_filt.sf), SubRegion) %>%
  full_join(., st_drop_geometry(DS_regions)[, c("SubRegion", "Region")]) %>% # Add in regions without any stations
  mutate(n= ifelse(is.na(n), 0, n)) %>% # Give regions without stations a count (n) = 0
  arrange(-n)

chla_station_filt_region_summary <- count(st_drop_geometry(chla_stations_filt.sf), Region) %>%
  full_join(., st_drop_geometry(DS_regions)[, c( "Region")]) %>% # Add in regions without any stations
  mutate(n= ifelse(is.na(n), 0, n)) %>% # Give regions without stations a count (n) = 0
  arrange(-n)



#### FIGURES ####

## Stations per subregion
ggplot(chla_station_subregion_summary, aes(x= n)) +
  geom_histogram(binwidth= 1, fill= "black", color= "gray70") +
  labs(x= "Number of stations in a subregion", y= "Number of subregions") +
  scale_y_continuous(expand= c(0, 0)) +
  theme_ppt
ggsave(last_plot(), filename= "data_summary_chla_subregion_stations.png", width= 8, height= 6.5, dpi= 300,
       path= "Figures")

## Number of stations per program
program_station_summary <- count(st_drop_geometry(chla_station_list.sf), Source)

ggplot(program_station_summary, aes(x= Source, y= n)) +
  geom_col() +
  labs(x= "Data source", y= "Number of stations") +
  theme_ppt
ggsave(last_plot(), filename= "data_summary_chla_program_stations.png", width= 8, height= 6.5, dpi= 300,
       path= "Figures")




## Number of years per station
chla_station_summary %>%
  select(Source, Station, n_years) %>%
  distinct() %>%
  group_by(n_years) %>%
  count(n_years) %>%
  ggplot(data= ., aes(x= n_years, y= n)) +
  geom_col(fill= "black") +
  labs(x= "Number of years of data", y= "Number of stations") +
  scale_x_continuous(breaks= seq(0, 12)) +
  theme_ppt
ggsave(last_plot(), filename= "data_summary_chla_YearsOfDataPerStation.png", width= 8, height= 6.5, dpi= 300,
       path= "Figures")




## Number of samples per year
chla_station_summary %>%
  group_by(count_chla) %>%
  count(count_chla) %>%
  ggplot(data= ., aes(x= count_chla, y= n)) +
  geom_col(fill= "black") +
  labs(x= "Number of samples per year", y= "Number of years") +
  scale_x_continuous(breaks= seq(0, 40, by= 4)) +
  theme_ppt
ggsave(last_plot(), filename= "data_summary_chla_SamplesPerYear.png", width= 8, height= 6.5, dpi= 300,
       path= "Figures")


## Distribution of chla values

DS_data %>%
  filter(!is.na(chla) & chla > 0 ) %>%
  ggplot(data= ., aes(x= chla)) +
  geom_histogram(binwidth= 1) +
  labs(x= expression(paste("Chl-a (", mu, "g/L)")), y= "Number of samples",
       caption= "Note: x-axis trimmed to 50 ug/L, though ~127 samples exceed this cutoff") +
  scale_x_continuous(limits= c(0, 50)) +
  facet_rep_wrap(~Source, nrow= 6, scales= "free_y") +
  theme_ppt
ggsave(last_plot(), filename= "data_summary_chla_ConcHistogram.png", width= 8, height= 8, dpi= 300,
       path= "Figures")


DS_data_filt %>%
  filter(!is.na(chla) & chla > 0 ) %>%
  ggplot(data= ., aes(x= chla)) +
  geom_histogram(binwidth= 1) +
  labs(x= expression(paste("Chl-a (", mu, "g/L)")), y= "Number of samples",
       caption= "Note: x-axis trimmed to 50 ug/L, though some samples exceed this cutoff") +
  scale_x_continuous(limits= c(0, 50)) +
  facet_rep_wrap(~Source, nrow= 6, scales= "free_y") +
  theme_ppt
#ggsave(last_plot(), filename= "data_summary_chla_ConcHistogram.png", width= 8, height= 8, dpi= 300,
#       path= "Figures")


#### MAPS #####

## Map of stations
ggplot() +
  geom_sf(data= DS_regions, aes(fill= SubRegion), alpha= 0.5) +
  geom_sf(data= DS_waterways, fill= "skyblue3", color= "black") +
  geom_sf(data= chla_station_list.sf, fill= "white", color= "black", shape= 21, size= 2) +
  labs(caption= "All stations") +
  scale_fill_discrete(guide= "none") +
  scale_color_discrete(guide= "none")  +
  scale_x_continuous(breaks= seq(-122, -121, by= 0.5)) +
  scale_y_continuous(breaks= seq(37.6, 38.6, by= 0.5)) +
  coord_sf() +
  facet_wrap(~Source, nrow= 2) +
  theme_map
ggsave(last_plot(), filename= "data_summary_StationMap.png", width= 10, height= 8, dpi= 600,
       path= "Figures")


ggplot() +
  geom_sf(data= DS_regions, aes(fill= Region), alpha= 0.5) +
  #geom_sf(data= DS_regions, aes(color= Region), fill= "transparent", size= 3) +
  geom_sf(data= DS_waterways, fill= "skyblue3", color= "black") +
  geom_sf(data= chla_stations_filt.sf, fill= "white", color= "black", shape= 21, size= 2) +
  labs(caption= ">=12 samples per year") +
  #scale_fill_discrete(guide= "none") +
  #scale_color_discrete(guide= "none")  +
  scale_x_continuous(breaks= seq(-122, -121, by= 0.5)) +
  scale_y_continuous(breaks= seq(37.6, 38.6, by= 0.5)) +
  coord_sf() +
  facet_wrap(~Source, nrow= 1) +
  theme_map
ggsave(last_plot(), filename= "StationMap_filtered.png", width= 10, height= 8, dpi= 600,
       path= "Figures")




