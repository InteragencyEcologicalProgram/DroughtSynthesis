## Script to analyse Microcystis Rating data


## Libraries
library(tidyverse)
library(brms)
#source("Scripts/Data_format.R")
source("Scripts/MyFunctionsAndThemes.R")

## Load data frames from Data_format.R
load("Data/DS_dataframesST.Rdata") 

## Transform original 1-5 scale to None, Low, High
mc_data <- DS_dataST %>%
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
length(unique(mc_stations_filt.sf$Station))

## Calculate maximum mc_rating value per month
mc_data_stats <- mc_data_filt %>% 
  group_by(Source, ds_year, ds_year_type, Region, Season, month, Station) %>% 
  summarize(mc_max= max(mc_factor),
            mc_min= min(mc_factor)) %>% 
  ungroup()

## WRITE CSV FILES
mc_data_stats %>% 
   write_csv(., "Data/mcRating_data_stats.csv")
 


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

Station_Region_count <- mc_data_stats %>%
  select(Source, Region, Station) %>% 
  distinct(.) %>% 
  group_by(Region) %>%
  count(Region)
sum(Station_Region_count$n)

## Percentage of each MC index level
mc_data_stats %>% 
  group_by(ds_year_type) %>% 
  count(mc_max) %>% 
  mutate(prop= n/sum(n),
         N= sum(n)) %>% 
  write_csv("Data/mc_rating_percentages.csv")

#### DATA FIGURES ####

## Facet labels
region_labels <- c(unique(mc_data_stats$Region)[1:2], "South-Central\nDelta", unique(mc_data_stats$Region)[4:5])
names(region_labels) <-   unique(mc_data_stats$Region)


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
  geom_bar(aes(fill= mc_max), position= position_dodge(preserve = "single")) +
  labs(x= "Water Year", y= "Number of observations") +
  scale_y_continuous(expand= c(0, 0)) +
  scale_fill_manual(values= c("Gray70", "seagreen4", "seagreen1"),
                    name= expression(paste(italic("Microcystis "), "Rating")),
                    labels= c("None (1)", "Low (2-3)", "High (4-5)")) +
  facet_rep_wrap(~Region, nrow= 2, labeller= labeller(Region= as_labeller(region_labels))) +
  theme_bw(base_size= 12) +
  #theme_doc +
  #theme(legend.position = "top")
  theme(legend.position = c(0.85, 0.2))
ggsave(last_plot(), filename= "MCrating_Region.png", width= 6.5, height= 4, dpi= 300,
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
  facet_rep_grid(Source~Region, labeller= labeller(Region= as_labeller(region_labels))) +
  theme_doc
ggsave(last_plot(), filename= "MCrating_Source.png", width= 10, height= 8, dpi= 300,
       path= "Figures")



#### STATISTICS ####

fit_max_mc1 <- brm(
  formula = mc_max ~ 1 + cs(ds_year_type) + Season + Region + (1|Station),
  data = mc_data_stats,
  family = acat("probit"),
  chains= 4,
  iter= 5000,
  warmup= 1000,
  cores= 4,
  control = list(adapt_delta = 0.99)
)

save(fit_max_mc1, file= "Data/fit_max_mc1b.Rdata")
load("Data/fit_max_mc1b.Rdata")
summary(fit_max_mc1)
#plot(fit_max_mc1)


## Extract marginal effects
max_mc1_conditions <- make_conditions(fit_max_mc1, c("Region", "Season"))
max_mc1_effects <- conditional_effects(fit_max_mc1, "ds_year_type", condition= max_mc1_conditions, categorical= TRUE)$`ds_year_type`

## Low & High Microcystis Ratings
ggplot(max_mc1_effects, aes(x= cats__, y= estimate__, group= ds_year_type)) +
  #geom_point(aes(color= ds_year_type), position= position_dodge(width= 0.3), size= 3) +
  geom_col(aes(fill= ds_year_type), color= "black", position= position_dodge()) +
  geom_errorbar(aes(ymin= lower__, ymax= upper__), width= 0.5, position= position_dodge(0.9)) +
  scale_fill_manual(values= year.colors, 
                    name= "Water Year") +
  labs(x= expression(paste(italic("Microcystis"), " Rating Level")), y= "Probability") +
  scale_y_continuous(expand= c(0, 0), limits= c(0, 1)) +
  scale_x_discrete(limits= c("low", "high"), labels= c("Low", "High")) +
  facet_rep_grid(Region ~ Season, repeat.tick.labels = TRUE, labeller= labeller(Region= as_labeller(region_labels))) +
  theme_doc +
  theme(legend.position= "top")
ggsave(last_plot(), filename= "MCrating_probs_LowHigh_Season.png", width= 6.5, height= 8.5, dpi= 300,
       path= "Figures")


## None, Low, & High Microcystis Ratings
ggplot(max_mc1_effects, aes(x= cats__, y= estimate__, group= ds_year_type)) +
  #geom_point(aes(color= ds_year_type), position= position_dodge(width= 0.3), size= 3) +
  geom_col(aes(fill= ds_year_type), color= "black", position= position_dodge()) +
  geom_errorbar(aes(ymin= lower__, ymax= upper__), width= 0.5, position= position_dodge(0.9)) +
  scale_fill_manual(values= year.colors, 
                    name= "Water Year") +
  labs(x= expression(paste(italic("Microcystis"), " Rating Level")), y= "Probability") +
  scale_y_continuous(expand= c(0, 0), limits= c(0, 1)) +
  scale_x_discrete(limits= c("none", "low", "high"), labels= c("None", "Low", "High")) +
  facet_rep_grid(Region ~ Season, repeat.tick.labels = TRUE, labeller= labeller(Region= as_labeller(region_labels))) +
##  theme_doc +
  theme_bw(base_size= 12) +
  theme(legend.position= "top")
ggsave(last_plot(), filename= "MCrating_probs_NoneLowHigh_Season.png", width= 6.5, height= 8.5, dpi= 300,
       path= "Figures")


## Summer only: Low & High Microcystis Ratings
#summer_signif <- rep(c("*", "", ""), 15)

ggplot(filter(max_mc1_effects, Season == "Summer"), aes(x= cats__, y= estimate__, group= ds_year_type)) +
  #geom_point(aes(color= ds_year_type), position= position_dodge(width= 0.3), size= 3) +
  geom_col(aes(fill= ds_year_type), color= "black", position= position_dodge()) +
  geom_errorbar(aes(ymin= lower__, ymax= upper__), width= 0.5, position= position_dodge(0.9)) +
  #geom_text(aes(x= cats__, y= upper__ + 0.1, group= ds_year_type), 
  #          label= summer_signif, size= 8, position= position_dodge(0.9)) +
  scale_fill_manual(values= year.colors, 
                    name= "Water year type", labels= c("Wet", "Below Avg.", "Drought")) +
  labs(x= expression(paste(italic("Microcystis"), " Rating Level")), y= "Probability") +
  scale_y_continuous(expand= c(0, 0), limits= c(0, 1)) +
  scale_x_discrete(limits= c("low", "high"), labels= c("Low", "High")) +
  #scale_x_discrete(labels= c("None", "Low", "High")) +
  facet_rep_wrap(~Region, ncol=1, scales= "free_x", repeat.tick.labels = FALSE, labeller= labeller(Region= as_labeller(region_labels))) +
  theme_doc +
  theme(legend.position= "top")
ggsave(last_plot(), filename= "MCrating_probs_LowHigh_Summer.png", width= 6.5, height= 8.5, dpi= 300,
       path= "Figures")


fit_max_mc2 <- brm(
  formula = mc_max ~ 1 + cs(ds_year_type) + (1|Region) + (1|Station),
  data = mc_data_stats,
  family = acat("probit"),
  chains= 4,
  iter= 5000,
  warmup= 1000,
  cores= 4,
  control = list(adapt_delta = 0.99)
)

#save(fit_max_mc1, file= "Data/fit_max_mc1.Rdata")
load("Data/fit_max_mc1.Rdata")
summary(fit_max_mc1)
plot(fit_max_mc1)


