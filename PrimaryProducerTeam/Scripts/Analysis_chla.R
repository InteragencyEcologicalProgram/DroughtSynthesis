## Short term drought synthesis analysis code for Chlorophyll A

## Libraries
library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)

source("Scripts/ggplot_themes.R")


#### DATA FILTERING ####
filter_chla_data <- function(data, min_samps_yr, excluded_regions, seasons, min_result){

chla_station_summary <- data %>%
  group_by(Source, Station, year) %>%
  summarize(count_chla= length(chla)) %>%
  ungroup() %>%
  group_by(Source, Station) %>%
  mutate(start_year= min(year),
         end_year= max(year),
         n_years= length(year))

chla_stations_filt <- chla_station_summary %>%
  filter(count_chla >= min_samps_yr)

data_filt <- data %>%
  filter(Station %in% chla_stations_filt$Station) %>%
  filter(!(Region %in% excluded_regions)) %>%
  filter(Season %in% seasons) %>%
  filter(chla > min_result)

return(data_filt)
}

## # load data frames from Data_format.R
load("Data/DS_dataframes.Rdata")


## Filter data based on
# Minimum samples per year per site
# Regions to exclude from the analysis
# Seasons to include in the analysis
# Minimum result threshold
DS_data_filt <- filter_chla_data(DS_data,
                                 min_samps_yr = 12,
                                 excluded_regions = c("Far West", "Suisun Marsh"),
                                 seasons= c("Summer", "Fall"),
                                 min_result = 0)

## Get data frame of unique stations in the filtered data set
chla_stations_filt.sf <- chla_stations.sf %>%
  filter(Station %in% unique(DS_data_filt$Station)) %>% # Remove Suisun Marsh because only have 2-3 EMP stations in that Region
  distinct(.)


## Transform chla data in preparation for statistical analyses
## Average all chla at each station by month, log transform chla
DS_data_stats <- DS_data_filt %>%
  group_by(ds_year, month, Source, Station, Region, SubRegion, ds_year_type, Date, Season) %>%
  summarize(chlaAvg= mean(chla, na.rm= TRUE),
            count= n()) %>%
  mutate(chla_log10= log10(chlaAvg), #log10
         chla_log= log(chlaAvg), # natural log
         month_pair= ifelse(month == 6 | month == 7, "1J-J",
                            ifelse(month == 8 | month == 9, "2A-S", "3O-N"))) %>%
  ungroup()


## Summary Tables of data
source_summary <- DS_data_stats %>%
  group_by(Source) %>%
  count(Source)


SubRegion_month_summary <- DS_data_stats %>%
  group_by(Source, ds_year, month, Region, Season) %>%
  count(SubRegion)

Station_month_summary <- DS_data_stats %>%
  group_by(Source, ds_year, month, Region, SubRegion, Season) %>%
  count(Station)

year_summary <- DS_data_stats %>%
  group_by(Source, ds_year, Region) %>%
  count(Region)


Year_type_summary <- DS_data_stats %>%
  group_by(Source, month, Region, SubRegion) %>%
  count(ds_year_type)



#### FIGURES ####
ggplot(year_summary, aes(x= ds_year, y= n)) +
  geom_col(aes(fill= Source)) +
  facet_rep_wrap(~Region, ncol= 1) +
  labs(x= "Station-months per year", y= "Count") +
  scale_y_continuous(expand= c(0, 0)) +
  theme_ppt
ggsave(last_plot(), filename= "year_sample_summary.png", width= 8, height= 6, dpi= 300,
       path= "Figures")


## Boxplots of data


ggplot(DS_data_stats, aes(x= ds_year_type, y= chla_log10)) +
  geom_boxplot(aes(fill= Season)) +
  labs(x= "Year type", y= expression(paste("Chlorophyll-a (", mu, "g/L)"))) +
  scale_y_continuous(breaks= c(-2, -1, 0, 1, 2),
                labels= c("0.01", "0.1", "1", "10", "100")) +
  scale_x_discrete(labels= c("Wet", "Below\nAvg", "Drought")) +
  annotation_logticks(side= "l") +
  facet_rep_wrap(~ Region, ncol= 4) +
  theme_doc +
  theme(legend.position = "top")
ggsave(last_plot(), filename= "chla_filtered_Season_log10.png", width= 10, height= 6, dpi= 300,
       path= "Figures")




ggplot(DS_data_stats, aes(x= as.factor(month), y= chla_log10)) +
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
  geom_sf(data= filter(DS_regions, Region != "Suisun Marsh"), aes(fill= Region), alpha= 0.5) +
  #geom_sf(data= DS_regions, aes(color= Region), fill= "transparent", size= 3) +
  geom_sf(data= DS_waterways, fill= "skyblue3", color= "black") +
  geom_sf(data= chla_stations_filt.sf, fill= "white", color= "black", shape= 21, size= 4) +
  scale_x_continuous(breaks= seq(-122, -121, by= 0.5)) +
  scale_y_continuous(breaks= seq(37.6, 38.6, by= 0.5)) +
  coord_sf() +
  facet_wrap(~Source, nrow= 2) +
  theme_map +
  theme(legend.position = c(0.8, 0.2))
ggsave(last_plot(), filename= "station_map_chla_filtered.png", width= 10, height= 10, dpi= 600,
       path= "Figures")



#### STATISTICS ####
fit_log10.1 <- lmer(chla_log10 ~ ds_year_type + Season + Region + (1|Station),
                   data= DS_data_stats)
summary(fit_log10.1)
plot(fit_log10.1)
anova(fit_log10.1)

emm_year <- emmeans(fit_log10.1, specs= "ds_year_type", pbkrtest.limit = 3448)
pairs(emm_year)
emm_Region <- emmeans(fit_log10.1, specs= "Region", pbkrtest.limit = 3448)
pairs(emm_Region)
emm_season <- emmeans(fit_log10.1, specs= "Season", pbkrtest.limit = 3448)
pairs(emm_season)





summary(emm_year)

?emmeans

DS_data_summary_stats



fit_log.1 <- lmer(chla_log ~ ds_year_type + Season + Region + (1|Station), data= DS_data_stats)
summary(fit_log.1)
plot(fit_log.1)
anova(fit_log.1)


fit_log.2 <- lmer(chla_log ~ ds_year_type + month_pair + Region + (1|Station), data= DS_data_stats)
summary(fit_log.2)
plot(fit_log.2)
anova(fit_log.2)


#### OLD CODE ####


## Quantile regression
library(lqmm)

fitq <- lqmm(fixed= chla_log10 ~ ds_year_type + Season + Region, random= ~ 1, group= Station, tau= 0.8,
             data= DS_data_stats, fit= FALSE)
summary(fitq)
plot(fitq)
?lqmm

fitq$control$verbose <- TRUE


fit.lqmm <- do.call("lqmm.fit.gs", args = fitq)

summary(fit.lqmm)
summary(DS_data_stats$chla)


ggplot(SubRegion_month_summary) +
  geom_histogram(aes(x= n), binwidth =1) +
  labs(x= "Samples per SubRegion per month") +
  scale_y_continuous(expand= c(0, 0)) +
  theme_ppt
#ggsave(last_plot(), filename= "subregion_sample_summary.png", width= 8, height= 6, dpi= 300,
#       path= "Figures")


ggplot(Station_month_summary) +
  geom_histogram(aes(x= n), binwidth =1) +
  labs(x= "Samples per station per month") +
  scale_y_continuous(expand= c(0, 0)) +
  theme_ppt
#ggsave(last_plot(), filename= "station_sample_summary.png", width= 8, height= 6, dpi= 300,
#       path= "Figures")


# ggplot(DS_data_stats, aes(x= ds_year_type, y= chla_log10)) +
#   geom_boxplot(aes(fill= month_pair)) +
#   labs(x= "Year type", y= expression(paste("Chla (", mu, "g/L)"))) +
#   scale_y_continuous(breaks= c(-2, -1, 0, 1, 2),
#                      labels= c("0.01", "0.1", "1", "10", "100")) +
#   scale_x_discrete(labels= c("Wet", "Dry", "Drought")) +
#   annotation_logticks(side= "l") +
#   scale_fill_discrete(name= "Months",
#                       labels= c("J-J", "A-S", "O-N")) +
#   facet_rep_wrap(~ Region, ncol= 3) +
#   theme_ppt
# ggsave(last_plot(), filename= "chla_MonthPair_Season_log10.png", width= 10, height= 6, dpi= 300,
#        path= "Figures")
#


# ggplot(DS_data_stats, aes(x= ds_year_type, y= chlaAvg)) +
#   geom_boxplot(aes(fill= Season)) +
#   facet_rep_wrap(~ Region, ncol= 5) +
#   theme_bw() +
#   theme_ppt



# ggplot() +
#   geom_sf(data= DS_regions, aes(fill= SubRegion), alpha= 0.5) +
#   #geom_sf(data= DS_regions, aes(color= Region), fill= "transparent", size= 3) +
#   geom_sf(data= DS_waterways, fill= "skyblue3", color= "black") +
#   geom_sf(data= chla_stations_filt.sf, fill= "white", color= "black", shape= 21, size= 4) +
#   labs(caption= ">=12 samples per year") +
#   scale_fill_discrete(guide= "none") +
#   #scale_color_discrete(guide= "none")  +
#   scale_x_continuous(breaks= seq(-122, -121, by= 0.5)) +
#   scale_y_continuous(breaks= seq(37.6, 38.6, by= 0.5)) +
#   coord_sf() +
#   facet_wrap(~Source, nrow= 2) +
#   theme_map
# ggsave(last_plot(), filename= "station_map_SubRegion_chla_filtered.png", width= 10, height= 10, dpi= 600,
#        path= "Figures")
#
