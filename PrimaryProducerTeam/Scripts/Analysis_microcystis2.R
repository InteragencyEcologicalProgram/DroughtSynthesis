## Script to analyse Microcystis Rating data


## Libraries
library(tidyverse)
library(brms)
#source("Scripts/Data_format.R")
source("Scripts/ggplot_themes.R")

## Load data frames from Data_format.R
load("Data/DS_dataframes.Rdata") 

## Transform original 1-5 scale to None, Low, High
mc_data <- DS_data %>%
  filter(!is.na(mc_rating)) %>%
  mutate(mc_mod= ifelse(mc_rating == 1, "none",
                        ifelse(mc_rating > 1 & mc_rating < 4, "low", "high")),
         mc_factor= factor(mc_mod, ordered= TRUE, levels= c("none", "low", "high")), 
         mc_binom= ifelse(mc_rating <= 4, "0", "1")) %>% 
  filter(Source != "DOP") #Remove Directed Outflows Project (DOP) data because it only starts in 2019

#### DATA FILTERING ####
filter_mc_data <- function(data, min_samps_yr, excluded_regions, seasons){
  require(tidyverse)
  require(sf)
  
  ## Get number of samples per year per station  
  mc_station_summary <- data %>%
    group_by(Source, Station, year, Latitude, Longitude) %>%
    summarize(count_mc= length(mc_mod)) %>%
    ungroup() %>%
    group_by(Source, Station) %>%
    mutate(start_year= min(year),
           end_year= max(year),
           n_years= length(year))
  
  ## Filter stations by minimum samples per year
  mc_stations_filt <- mc_station_summary %>%
    filter(count_mc >= min_samps_yr) %>% 
    select(Source, Station, Latitude, Longitude) %>% 
    distinct(.) %>% 
    mutate(SourceStation= str_c(Source, Station, sep= "-"))

  
  ## Filter by station, excluded region, and season
  data_filt <- data %>%
    mutate(SourceStation= str_c(Source, Station, sep= "-")) %>% 
    filter(SourceStation %in% mc_stations_filt$SourceStation) %>%
    #filter(Station %in% mc_stations_filt$Station) %>%
    filter(!(Region %in% excluded_regions)) %>%
    filter(Season %in% seasons) %>%
    distinct(.)
  
  ## Filter stations again by average data per year > 3
  mc_stations_filt2 <- data_filt %>% 
    select(Source, Station, SourceStation, ds_year, mc_factor) %>% 
    group_by(Source, Station, SourceStation, ds_year) %>% 
    summarize(count_mc= length(mc_factor)) %>% 
    group_by(Source, Station, SourceStation) %>% 
    mutate(mean_counts= mean(count_mc)) %>% 
    filter(mean_counts > 3) %>% # Average amount of data per year
    select(SourceStation) %>% 
    distinct(.)
  
  data_filt2 <- data_filt %>% 
    filter(SourceStation %in% mc_stations_filt2$SourceStation)
  
  ## Unique station list with Lat/Longs
  stations_filt.sf <- data_filt2 %>% 
    select(Source, Station, Latitude, Longitude) %>% 
    distinct(.) %>%
    sf::st_as_sf(., coords= c("Longitude", "Latitude"), crs= 4269) %>% #NAD83
    sf::st_transform(., crs= 26910) %>% # NAD 83/ UTM10N
    distinct(.)
  
  return(list(data= data_filt2, stations= stations_filt.sf))
}

mc_data_filt_list <- filter_mc_data(data= mc_data,
                       min_samps_yr <- 6,
                       excluded_regions = c("Far West"),
                       seasons= c("Summer", "Fall"))

mc_data_filt <- mc_data_filt_list$data
mc_stations_filt.sf <- mc_data_filt_list$stations


## Calculate maximum mc_rating value per month
mc_data_stats <- mc_data_filt %>% 
  group_by(Source, ds_year, ds_year_type, Region, Season, month, Station) %>% 
  summarize(mc_max= max(mc_factor),
            mc_min= min(mc_factor)) %>% 
  ungroup()

## WRITE CSV FILES
# data %>% 
#   select(-chla,-Depth, -SampleType, -`M Chla (µg/L)`, -Field_coords, -LongStationName, -ShortStationName, -HABstation) %>% 
#   write_csv(., "Data/mcRating_data.csv")

# mc_data_filt %>% 
#   select(-mc_factor, -chla,-Depth, -SampleType, -`M Chla (µg/L)`, -Field_coords, -LongStationName, -ShortStationName, -HABstation) %>% 
#   write_csv(., "Data/mcRating_data_filtered.csv")
# 


## SUMMARIZE DATA
year_summary <- mc_data_stats %>%
  group_by(Source, ds_year, Region) %>%
  count(Region)

year_summary_all <- mc_data_filt %>%
  group_by(Source, ds_year) %>%
  count(Source)

months_per_year <- mc_data_stats %>% 
  select(Source, Station, ds_year, mc_max) %>% 
  group_by(Source, Station, ds_year) %>% 
  summarize(count_mc= length(mc_max)) %>% 
  group_by(Source, Station) %>% 
  mutate(mean_counts= mean(count_mc),
         SourceStation= str_c(Source, Station, sep= "-"))

#### DATA FIGURES ####

## Station map
ggplot() +
  geom_sf(data= DS_regions, aes(fill= Region), alpha= 0.5) +
  geom_sf(data= DS_waterways, fill= "skyblue3", color= "black") +
  geom_sf(data= mc_stations_filt.sf, fill= "white", color= "black", shape= 21, size= 4) +
  scale_x_continuous(breaks= seq(-122, -121, by= 0.5)) +
  scale_y_continuous(breaks= seq(37.6, 38.6, by= 0.5)) +
  coord_sf() +
  facet_wrap(~Source, nrow= 2) +
  theme_map +
  theme(legend.position = c(0.8, 0.2))
ggsave(last_plot(), filename= "mc_station_map_filtered.png", width= 10, height= 10, dpi= 600,
       path= "Figures")

## Data per year
ggplot(year_summary, aes(x= ds_year, y= n)) +
  geom_col(aes(fill= Source)) +
  facet_rep_wrap(~Region, ncol= 1) +
  labs(x= "Data per year", y= "Count") +
  scale_y_continuous(expand= c(0, 0), breaks= seq(0, 500, by= 100), labels= c("0", "", "200", "", "400", "")) +
  theme_doc
ggsave(last_plot(), filename= "mc_year_sample_summary.png", width= 8, height= 6, dpi= 300,
       path= "Figures")

## MC-rating by Region
ggplot(mc_data_stats, aes(x= ds_year_type)) +
  geom_bar(aes(fill= mc_max), position= "dodge") +
  labs(x= "Year type", y= "Number of observations") +
  scale_y_continuous(expand= c(0, 0)) +
  scale_x_discrete(labels= c("Wet", "Below\nAvg", "Drought")) +
  scale_fill_manual(values= c("Gray70", "seagreen4", "seagreen1"),
                    name= expression(paste(italic("Microcystis "), "Rating")),
                    labels= c("None (1)", "Low (2-3)", "High (4-5)")) +
  facet_rep_grid(.~Region) +
  theme_doc +
  theme(legend.position = "top")
ggsave(last_plot(), filename= "MCrating_Region.png", width= 12, height= 6, dpi= 300,
       path= "Figures")


## MC-rating by Source and Region
ggplot(mc_data_stats, aes(x= ds_year_type)) +
  geom_bar(aes(fill= mc_max), position= "dodge") +
  labs(x= "Year type") +
  scale_y_continuous(expand= c(0, 0)) +
  scale_x_discrete(labels= c("Wet", "Below\nAvg", "Drought")) +
  scale_fill_manual(values= c("Gray70", "seagreen4", "seagreen1"),
                    name= "Rating",
                    labels= c("None (1)", "Low (2-3)", "High (4-5)")) +
  facet_rep_grid(Source~Region) +
  theme_doc
ggsave(last_plot(), filename= "MCrating_Source.png", width= 10, height= 8, dpi= 300,
       path= "Figures")



#### STATISTICS ####

fit_mc1 <- brm(
  formula = mc_factor ~ 1 + cs(ds_year_type) + Season + Region + (1|Station),
  data = mc_data_filt,
  family = acat("probit"),
  chains= 4,
  iter= 5000,
  warmup= 1000,
  cores= 4,
  control = list(adapt_delta = 0.99)
)

save(fit_ac3c, file= "Data/fit_ac3c.Rdata")
load("Data/fit_ac3c.Rdata")
summary(fit_ac3c)
plot(fit_ac3c)
unique(mc_data_filt$Region)

conditions.df <- make_conditions(fit_ac3c, c("Region", "Season"))

#conditional_effects(fit_ac3, "ds_year_type", condition= conditions.df, categorical= TRUE)
term_yt2 <- conditional_effects(fit_ac3c, "ds_year_type", condition= conditions.df, categorical= TRUE)$`ds_year_type`

term_yt <- conditional_effects(fit_ac3, categorical= TRUE)$`ds_year_type`
names(term_yt2)
filter(term_yt2, Season == "Summer")

unique(mc_data_stats$Region)
labeller()


ggplot(term_yt2, aes(x= cats__, y= estimate__, group= ds_year_type)) +
  #geom_point(aes(color= ds_year_type), position= position_dodge(width= 0.3), size= 3) +
  geom_col(aes(fill= ds_year_type), color= "black", position= position_dodge()) +
  geom_errorbar(aes(ymin= lower__, ymax= upper__), width= 0.5, position= position_dodge(0.9)) +
  scale_fill_manual(values= c("skyblue3", "mistyrose2", "tomato"), 
                    name= "Water year type", labels= c("Wet", "Below Avg.", "Drought")) +
  labs(x= "", y= expression(paste("Probability of ", italic("Microcystis"), " detection"))) +
  scale_y_continuous(expand= c(0, 0)) +
  #scale_x_discrete(labels= c("No\nMicrocystis", "Low\nMicrocystis", "High\nMicrocystis")) +
  scale_x_discrete(limits= c("b_low", "c_high"), labels= c(expression(paste("Low ", italic("Microcystis"))),
                                                           expression(paste("High ", italic("Microcystis"))))) +
  #facet_rep_grid(Season ~ Region, nrow= 5, repeat.tick.labels = TRUE) +
  facet_rep_grid(Region ~ Season, repeat.tick.labels = TRUE) +
  theme_doc +
  theme(legend.position= "bottom")
ggsave(last_plot(), filename= "MCrating_probs_LowHigh_Season.png", width= 6.5, height= 8.5, dpi= 300,
       path= "Figures")


ggplot(term_yt2, aes(x= cats__, y= estimate__, group= ds_year_type)) +
  #geom_point(aes(color= ds_year_type), position= position_dodge(width= 0.3), size= 3) +
  geom_col(aes(fill= ds_year_type), color= "black", position= position_dodge()) +
  geom_errorbar(aes(ymin= lower__, ymax= upper__), width= 0.5, position= position_dodge(0.9)) +
  scale_fill_manual(values= c("skyblue3", "mistyrose2", "tomato"), 
                    name= "Water year type", labels= c("Wet", "Below Avg.", "Drought")) +
  labs(x= "", y= expression(paste("Probability of ", italic("Microcystis"), " detection"))) +
  scale_y_continuous(expand= c(0, 0)) +
  #scale_x_discrete(labels= c("No\nMicrocystis", "Low\nMicrocystis", "High\nMicrocystis")) +
  scale_x_discrete(limits= c("b_low", "c_high"), labels= c(expression(paste("Low ", italic("Microcystis"))),
                                                           expression(paste("High ", italic("Microcystis"))))) +
  facet_rep_wrap(~ Region, nrow= 5) +
  theme_doc +
  theme(legend.position= c(0.76, 0.13))
ggsave(last_plot(), filename= "MCrating_probs_LowHigh.png", width= 6.5, height= 6, dpi= 300,
       path= "Figures")


ggplot(term_yt2, aes(x= cats__, y= estimate__, group= ds_year_type)) +
  #geom_point(aes(color= ds_year_type), position= position_dodge(width= 0.3), size= 3) +
  geom_col(aes(fill= ds_year_type), color= "black", position= position_dodge()) +
  geom_errorbar(aes(ymin= lower__, ymax= upper__), width= 0.5, position= position_dodge(0.9)) +
  scale_fill_manual(values= c("skyblue3", "mistyrose2", "tomato"), 
                    name= "Water year type", labels= c("Wet", "Below Avg.", "Drought")) +
  labs(x= "", y= "Probability of rating value") +
  scale_y_continuous(expand= c(0, 0)) +
  scale_x_discrete(labels= c("No\nMicrocystis", "Low\nMicrocystis", "High\nMicrocystis")) +
  #scale_x_discrete(limits= c("b_low", "c_high"), labels= c("Low\nMicrocystis", "High\nMicrocystis")) +
  facet_rep_wrap(~ Region, nrow= 3) +
  theme_doc +
  theme(legend.position= c(0.76, 0.13))
ggsave(last_plot(), filename= "MCrating_probs.png", width= 6.5, height= 6, dpi= 300,
       path= "Figures")



