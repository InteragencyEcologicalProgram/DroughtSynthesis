## Short term drought synthesis analysis code for Chlorophyll A

## Libraries
library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
source("Scripts/ggplot_themes.R")

## Load data frames from Data_format.R
load("Data/DS_dataSTframesST.Rdata")


#### DATA FILTERING ####
## Filter data based on
# Minimum samples per year per site
# Minimum number of years sampled at a site
# Regions to exclude from the analysis
# Seasons to include in the analysis
# Minimum result threshold
chla_data_filt_list <- filter_chla_data(data= DS_dataST,
                                        min_samps_yr = 12,
                                        min_yrs= 6,
                                        excluded_regions = c("Far West"),
                                        seasons= c("Summer", "Fall", "Spring", "Winter"),
                                        min_result = 0)


chla_data_filt <- chla_data_filt_list$data
chla_stations_filt.sf <- chla_data_filt_list$stations


## Get data frame of unique stations in the filtered data set
#chla_stations_filt.sf <- chla_stations.sf %>%
#  filter(Station %in% unique(DS_dataST_filt$Station)) %>% # Remove Suisun Marsh because only have 2-3 EMP stations in that Region
#  distinct(.)


## Transform chla data in preparation for statistical analyses
## Average all chla at each station by month, log transform chla
chla_data_stats <- chla_data_filt %>%
  group_by(ds_year, month, Source, Station, Region, SubRegion, ds_year_type, Date, Season) %>%
  summarize(samples_in_month= n(),
            chlaAvg= mean(chla, na.rm= TRUE),
            chlaAvg_log10= log10(chlaAvg), #log10
            chlaAvg_log= log(chlaAvg), # natural log
            ) %>% 
  ungroup()

## WRITE CSV FILE
#chla_data_stats %>% 
#  select(Date, everything(), -chlaAvg_log) %>% 
#   write_csv(., "Data/chla_data_stats.csv")


## Summary Tables of data
source_summary <- chla_data_stats %>%
  group_by(Source) %>%
  count(Source)
sum(source_summary$n)

SubRegion_month_summary <- chla_data_stats %>%
  group_by(Source, ds_year, month, Region, Season) %>%
  count(SubRegion)

Station_month_summary <- chla_data_stats %>%
  group_by(Source, ds_year, month, Region, SubRegion, Season) %>%
  count(Station)
sum(Station_month_summary$n)

year_summary <- chla_data_stats %>%
  group_by(Source, ds_year, Region) %>%
  count(Region)

sum(year_summary$n)

Station_count <- chla_data_stats %>%
  select(Source, Station) %>% 
  distinct(.) %>% 
  group_by(Source) %>%
  count(Source)
sum(Station_count$n)

Station_Region_count <- chla_data_stats %>%
  select(Source, Region, Station) %>% 
  distinct(.) %>% 
  group_by(Region) %>%
  count(Region)
sum(Station_count$n)

Year_type_summary <- chla_data_stats %>%
  group_by(Source, month, Region, SubRegion) %>%
  count(ds_year_type)



#### FIGURES ####
ggplot(year_summary, aes(x= ds_year, y= n)) +
  geom_col(aes(fill= Source)) +
  facet_rep_wrap(~Region, ncol= 1) +
  labs(x= "Station-months per year", y= "Count") +
  scale_y_continuous(expand= c(0, 0), breaks= seq(0, 600, by= 100), labels= c("0", "", "200", "", "400", "", "600")) +
  theme_doc
ggsave(last_plot(), filename= "chla_year_sample_summary.png", width= 8, height= 6, dpi= 300,
       path= "Figures")


## Boxplots of data
season.colors <- c("burlywood4", "darkslategray3", "chartreuse3", "sienna3")
year.colors <- c("skyblue3", "mistyrose2", "tomato")

## Yeartype only
ggplot(chla_data_stats, aes(x= ds_year_type, y= chlaAvg_log10)) +
  geom_boxplot(aes(fill= ds_year_type)) +
  labs(x= "Year type", y= expression(paste("Chlorophyll-a (", mu, "g/L)"))) +
  scale_y_continuous(breaks= c(-2, -1, 0, 1, 2),
                     labels= c("0.01", "0.1", "1", "10", "100")) +
  scale_x_discrete(labels= c("Wet", "Below\nAvg", "Drought")) +
  scale_fill_manual(values= year.colors, guide= "none") +
  annotation_logticks(side= "l") +
  theme_doc
ggsave(last_plot(), filename= "chla_filtered_YearTypeOnly_log10.png", width= 6.5, height= 8, dpi= 300,
       path= "Figures")

## Yeartype and Regions
ggplot(chla_data_stats, aes(x= ds_year_type, y= chlaAvg_log10)) +
  geom_boxplot(aes(fill= ds_year_type)) +
  labs(x= "Year type", y= expression(paste("Chlorophyll-a (", mu, "g/L)"))) +
  scale_y_continuous(breaks= c(-2, -1, 0, 1, 2),
                     labels= c("0.01", "0.1", "1", "10", "100")) +
  scale_x_discrete(labels= c("Wet", "Below\nAvg", "Drought")) +
  scale_fill_manual(values= year.colors, guide= "none") +
  annotation_logticks(side= "l") +
  facet_rep_wrap(~ Region, ncol= 2, repeat.tick.labels = TRUE) +
  theme_doc +
  theme(legend.position = c(0.77, 0.15), 
        legend.direction = "vertical")
ggsave(last_plot(), filename= "chla_filtered_Region_log10.png", width= 6.5, height= 8, dpi= 300,
       path= "Figures")


## Region and Season
ggplot(chla_data_stats, aes(x= ds_year_type, y= chlaAvg_log10)) +
  geom_boxplot(aes(fill= Season)) +
  labs(x= "Year type", y= expression(paste("Chlorophyll-a (", mu, "g/L)"))) +
  scale_y_continuous(breaks= c(-2, -1, 0, 1, 2),
                labels= c("0.01", "0.1", "1", "10", "100")) +
  scale_x_discrete(labels= c("Wet", "Below\nAvg", "Drought")) +
  scale_fill_manual(values= season.colors) +
  annotation_logticks(side= "l") +
  facet_rep_wrap(~ Region, ncol= 2, repeat.tick.labels = TRUE) +
  theme_doc +
  theme(legend.position = c(0.77, 0.15), 
        legend.direction = "vertical")
ggsave(last_plot(), filename= "chla_filtered_SeasonRegion_log10.png", width= 6.5, height= 8, dpi= 300,
       path= "Figures")

## Year Type and Season
ggplot(chla_data_stats, aes(x= Season, y= chlaAvg_log10)) +
  geom_boxplot(aes(fill= ds_year_type)) +
  labs(x= "Year type", y= expression(paste("Chlorophyll-a (", mu, "g/L)"))) +
  scale_y_continuous(breaks= c(-2, -1, 0, 1, 2),
                     labels= c("0.01", "0.1", "1", "10", "100")) +
  #scale_x_discrete(labels= c("Wet", "Below\nAvg", "Drought")) +
  scale_fill_manual(values= year.colors, name= "Water Year Type", labels= c("Wet", "Below Avg.", "Drought")) +
  annotation_logticks(side= "l") +
  facet_rep_wrap(~ Region, ncol= 2, repeat.tick.labels = TRUE) +
  theme_doc +
  theme(legend.position = c(0.77, 0.15), 
        legend.direction = "vertical")
ggsave(last_plot(), filename= "chla_filtered_SeasonRegion2_log10.png", width= 6.5, height= 8, dpi= 300,
       path= "Figures")



## Regions combined and Season
ggplot(chla_data_stats, aes(x= ds_year_type, y= chlaAvg_log10)) +
  geom_boxplot(aes(fill= Season)) +
  labs(x= "Year type", y= expression(paste("Chlorophyll-a (", mu, "g/L)"))) +
  scale_y_continuous(breaks= c(-2, -1, 0, 1, 2),
                     labels= c("0.01", "0.1", "1", "10", "100")) +
  scale_x_discrete(labels= c("Wet", "Below\nAvg", "Drought")) +
  scale_fill_manual(values= season.colors) +
  annotation_logticks(side= "l") +
  #facet_rep_wrap(~ Region, ncol= 2, repeat.tick.labels = TRUE) +
  theme_doc #+
  theme(legend.position = c(0.77, 0.15), 
        legend.direction = "vertical")
ggsave(last_plot(), filename= "chla_filtered_Season_log10.png", width= 6.5, height= 4, dpi= 300,
       path= "Figures")

## Density plot
ggplot(chla_data_stats, aes(x= chlaAvg_log10)) +
  #geom_histogram(aes(fill= ds_year_type)) +
  geom_density(aes(color= ds_year_type), size= 1.5) +
  labs(x= "Chla (ug/L)", y= "Density") +
  scale_y_continuous(expand= c(0, 0)) +
  scale_x_continuous(breaks= c(0, 1, 2), labels= c(1, 10, 100)) +
  scale_color_manual(values= year.colors, name= "Water Year Type", labels= c("Wet", "Below Avg.", "Drought")) +
  annotation_logticks(side= "b") +
  #facet_rep_grid(. ~ Season, repeat.tick.labels = TRUE) +
  theme_doc +
theme(legend.position = c(0.8, 0.8), 
      legend.direction = "vertical")
ggsave(last_plot(), filename= "chla_filtered_log10_density.png", width= 6.5, height= 4, dpi= 300,
       path= "Figures")


## Month and Region
ggplot(chla_data_stats, aes(x= as.factor(month), y= chlaAvg_log10)) +
  geom_boxplot(aes(fill= ds_year_type)) +
  labs(x= "Month", y= expression(paste("Chla (", mu, "g/L)"))) +
  scale_y_continuous(breaks= c(-2, -1, 0, 1, 2),
                     labels= c("0.01", "0.1", "1", "10", "100")) +
  #scale_x_discrete(labels= c("Wet", "Dry", "Drought")) +
  annotation_logticks(side= "l") +
  scale_fill_manual(name= "Year type",
                      labels= c("Wet", "Dry", "Drought"),
                    values= c("Skyblue3", "darksalmon", "firebrick3")) +
  facet_rep_grid(Region~.) +
  theme_doc
ggsave(last_plot(), filename= "chla_Month_log10.png", width= 10, height= 8, dpi= 300,
       path= "Figures")


## Station map
ggplot() +
  #geom_sf(data= filter(DS_regions, Region != "Suisun Marsh"), aes(fill= Region), alpha= 0.5) +
  geom_sf(data= DS_regions, aes(fill= Region), alpha= 0.5) +
  #geom_sf(data= DS_regions, aes(color= Region), fill= "transparent", size= 3) +
  geom_sf(data= DS_waterways, fill= "skyblue3", color= "black") +
  geom_sf(data= chla_stations_filt.sf, fill= "white", color= "black", shape= 21, size= 4) +
  scale_x_continuous(breaks= seq(-122, -121, by= 0.5)) +
  scale_y_continuous(breaks= seq(37.6, 38.6, by= 0.5)) +
  coord_sf() +
  facet_wrap(~Source, nrow= 2) +
  theme_map +
  theme(legend.position = "right")
  #theme(legend.position = c(0.8, 0.2))
ggsave(last_plot(), filename= "station_map_chla_filtered.png", width= 6.5, height= 5, dpi= 600,
       path= "Figures")



#### STATISTICS ####
fit_log10.1 <- lmer(chlaAvg_log10 ~ ds_year_type + Season + Region + (1|Station),
                   data= chla_data_stats)
summary(fit_log10.1)
plot(fit_log10.1)
anova(fit_log10.1)

emm_year <- emmeans(fit_log10.1, specs= "ds_year_type", pbkrtest.limit = nrow(chla_data_stats))
pairs(emm_year)
emm_Region <- emmeans(fit_log10.1, specs= "Region", pbkrtest.limit = nrow(chla_data_stats))
pairs(emm_Region)
emm_season <- emmeans(fit_log10.1, specs= "Season", pbkrtest.limit = nrow(chla_data_stats))
pairs(emm_season)


