library(ggplot2)
library(lemon)
library(ggsci)
library(cowplot)
library(colorspace)

#### CUSTOM FUNCTIONS ####
filter_chla_data <- function(data, min_samps_yr, min_yrs, excluded_regions, seasons, min_result){
  require(tidyverse)
  require(sf)
  
  chla_station_summary <- data %>%
    group_by(Source, Station, year) %>%
    summarize(count_chla= length(chla)) %>%
    ungroup() %>%
    group_by(Source, Station) %>%
    mutate(start_year= min(year),
           end_year= max(year),
           n_years= length(year))
  
  chla_stations_filt <- chla_station_summary %>%
    filter(count_chla >= min_samps_yr) %>% 
    filter(n_years >= min_yrs) %>% 
    mutate(SourceStation= str_c(Source, Station, sep= "-"))
  
  data_filt <- data %>%
    mutate(SourceStation= str_c(Source, Station, sep= "-")) %>% 
    filter(SourceStation %in% chla_stations_filt$SourceStation) %>%
    #filter(Station %in% chla_stations_filt$Station) %>%
    filter(!(Region %in% excluded_regions)) %>%
    filter(Season %in% seasons) %>%
    filter(chla > min_result)
  
  ## Filter stations again by average data per year > 6
  chla_stations_filt2 <- data_filt %>% 
    select(Source, Station, SourceStation, ds_year, chla) %>% 
    group_by(Source, Station, SourceStation, ds_year) %>% 
    summarize(count_chla= length(chla)) %>% 
    group_by(Source, Station, SourceStation) %>% 
    mutate(mean_counts= mean(count_chla)) %>% 
    filter(mean_counts > 6) %>% # Average amount of data per year
    select(SourceStation) %>% 
    distinct(.)
  
  
  data_filt2 <- data_filt %>% 
    filter(SourceStation %in% chla_stations_filt2$SourceStation)
  
  ## Unique station list with Lat/Longs
  stations_filt.sf <- data_filt2 %>% 
    select(Source, Station, Region, Latitude, Longitude) %>% 
    distinct(.) %>%
    sf::st_as_sf(., coords= c("Longitude", "Latitude"), crs= 4269) %>% #NAD83
    sf::st_transform(., crs= 26910) %>% # NAD 83/ UTM10N
    distinct(.)
  
  return(list(data= data_filt2, stations= stations_filt.sf))
  
}

add_DateTime <- function(df){
  library(lubridate)
  df2 <- df %>% 
    mutate(year= year(Date),
           month= month(Date),
           wyear= as.character(smwrBase::waterYear(Date)), #smwrBase a USGS package: https://github.com/USGS-R/smwrBase
           ds_year= ifelse(month == 12, year+1, year), # DS analysis will use modified year with december as the first month of the subsequent year
           Julian= yday(Date),
           DOY= ymd(str_c("1904", month(Date), day(Date), sep= '-'))) #1904 was a leap year
  return(df2)
}

#### GGPLOT THEMES ############################
theme_ppt <- theme(panel.grid = element_blank(),
                   plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"),
                   text = element_text(size= 16),
                   plot.background = element_rect(fill = "transparent", color= "transparent"), # bg of the plot
                   panel.background = element_rect(fill= "transparent", color= "transparent"),
                   panel.border= element_rect(fill= NA, color= NA, linetype= "solid", size= 1),
                   #panel.ontop = TRUE,
                   axis.line = element_line(color= "black", size= 0.25),
                   axis.text = element_text(colour="black", size= 14),
                   axis.title.x = element_text(vjust= -0.75),
                   axis.title.y = element_text(vjust= 1.5),
                   legend.background = element_rect(size= 0.25, color="black", fill= "transparent"),
                   legend.key = element_blank(),
                   strip.background=element_rect(fill="transparent", color="transparent"),
                   strip.text = element_text(size= 20)
)
#axis.text.x = element_text(angle= 45, hjust= 1))


theme_doc <- theme(panel.grid = element_blank(),
                   plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"),
                   text = element_text(size= 12),
                   plot.background = element_rect(fill = "transparent", color= "transparent"), # bg of the plot
                   panel.background = element_rect(fill= "transparent", color= "transparent"),
                   panel.border= element_rect(fill= NA, color= NA, linetype= "solid", size= 1),
                   #panel.ontop = TRUE,
                   axis.line = element_line(color= "black", size= 0.25),
                   axis.text = element_text(colour="black", size= 10),
                   axis.title.x = element_text(vjust= -0.75),
                   axis.title.y = element_text(vjust= 1.5),
                   legend.background = element_rect(size= 0.25, color="black", fill= "transparent"),
                   legend.key = element_blank(),
                   strip.background=element_rect(fill="transparent", color="transparent"),
                   strip.text = element_text(size= 14)
)
#axis.text.x = element_text(angle= 45, hjust= 1))

theme_map <- theme(panel.grid = element_line(color= "grey92", size= 1),
                   plot.margin = unit(c(0.2, 0.2, 0.2, 0.2), "cm"),
                   text = element_text(size= 12),
                   plot.background = element_rect(fill = "transparent", color= "transparent"), # bg of the plot
                   panel.background = element_rect(fill= "transparent", color= NA),
                   panel.border= element_rect(fill= NA, color= "gray60", linetype= "solid", size= 2),
                   panel.ontop = FALSE,
                   #axis.line = element_line(color= "black", size= 0.25),
                   axis.line = element_blank(),
                   axis.text = element_text(colour="black", size= 10),
                   axis.title.x = element_text(vjust= -0.75),
                   axis.title.y = element_text(vjust= 1.5),
                   legend.background = element_rect(size= 0.25, color="black", fill= "transparent"),
                   legend.key = element_blank(),
                   strip.background=element_rect(fill="transparent", color="transparent"),
                   strip.text = element_text(size= 12)
                   )
