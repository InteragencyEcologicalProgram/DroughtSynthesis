library(tidyverse)
#library(lme4)
#library(lmerTest)
library(brms)
#source("Scripts/Data_format.R")
source("Scripts/ggplot_themes.R")


load("Data/DS_dataframes.Rdata") # load data frames from Data_format.R


mc_data <- DS_data %>%
  filter(!is.na(mc_rating)) %>%
  mutate(mc_mod= ifelse(mc_rating == 1, "a_none",
                        ifelse(mc_rating > 1 & mc_rating < 4, "b_low", "c_high")),
         mc_binom= ifelse(mc_rating <= 4, "0", "1")) %>% 
  filter(Source != "DOP") #Remove Directed Outflows Project (DOP) data because it only starts in 2019


data= mc_data
min_samps_yr <- 1
excluded_regions = c("Far West")
seasons= c("Summer", "Fall")


#### DATA FILTERING ####
mc_station_summary <- data %>%
  group_by(Source, Station, year, Latitude, Longitude) %>%
  summarize(count_mc= length(mc_mod)) %>%
  ungroup() %>%
  group_by(Source, Station) %>%
  mutate(start_year= min(year),
         end_year= max(year),
         n_years= length(year))

source_counts <- mc_station_summary %>% 
  group_by(Source) %>% 
  count(count_mc)

mc_stations_filt <- mc_station_summary %>%
  filter(count_mc >= min_samps_yr) %>% 
  select(Source, Station, Latitude, Longitude) %>% 
  distinct(.)

mc_stations_filt %>% 
  select(Station) %>% 
  distinct(.)

length(unique(mc_stations_filt$Station))
length(unique(mc_station_summary$Station))
unique(data$Source)


mc_stations_filt.sf <- mc_stations_filt %>%
  st_as_sf(., coords= c("Longitude", "Latitude"), crs= 4269) %>% #NAD83
  st_transform(., crs= 26910) %>% # NAD 83/ UTM10N
  distinct(.)


class(chla_stations.sf)

mc_data_filt <- data %>%
  filter(Station %in% mc_stations_filt$Station) %>%
  filter(!(Region %in% excluded_regions)) %>%
  filter(Season %in% seasons) %>%
  mutate(mc_factor= factor(mc_mod, ordered= TRUE, levels= c("a_none", "b_low", "c_high"))) %>%
  distinct(.)
#filter(chla > min_result) %>%
names(data)


## WRITE CSV FILES
# data %>% 
#   select(-chla,-Depth, -SampleType, -`M Chla (µg/L)`, -Field_coords, -LongStationName, -ShortStationName, -HABstation) %>% 
#   write_csv(., "Data/mcRating_data.csv")

# mc_data_filt %>% 
#   select(-mc_factor, -chla,-Depth, -SampleType, -`M Chla (µg/L)`, -Field_coords, -LongStationName, -ShortStationName, -HABstation) %>% 
#   write_csv(., "Data/mcRating_data_filtered.csv")
# 

## Calculate maximum mc_rating value per month
mc_data_stats <- mc_data_filt %>% 
  group_by(Source, year, ds_year_type, Region, Season, month, Station) %>% 
  summarize(mc_max= max(mc_factor),
            mc_min= min(mc_factor))
mc_data_stats





year_summary <- mc_data_filt %>%
  group_by(Source, ds_year, Region) %>%
  count(Region)

year_summary_all <- mc_data %>%
  group_by(Source, ds_year) %>%
  count(Source)


ggplot(year_summary, aes(x= ds_year, y= n)) +
  geom_col(aes(fill= Source)) +
  facet_rep_wrap(~Region, ncol= 1) +
  labs(x= "Station-months per year", y= "Count") +
  scale_y_continuous(expand= c(0, 0), breaks= seq(0, 500, by= 100), labels= c("0", "", "200", "", "400", "")) +
  
  theme_ppt
ggsave(last_plot(), filename= "mc_year_sample_summary.png", width= 8, height= 6, dpi= 300,
       path= "Figures")

mc_fmwt <- filter(mc_data_filt, Source == "FMWT")
mc_stn <- filter(mc_data_filt, Source == "STN")

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




ggplot(mc_data_filt, aes(x= ds_year_type)) +
  geom_bar(aes(fill= mc_mod), position= "dodge") +
  labs(x= "Year type") +
  scale_y_continuous(expand= c(0, 0)) +
  scale_x_discrete(labels= c("Wet", "Below\nAvg", "Drought")) +
  scale_fill_manual(values= c("Gray70", "seagreen4", "seagreen1"),
                    name= "Rating",
                    labels= c("None (1)", "Low (2-3)", "High (4-5)")) +
  facet_rep_grid(Source~Region) +
  theme_ppt
ggsave(last_plot(), filename= "MCrating_Source.png", width= 10, height= 8, dpi= 300,
       path= "Figures")


ggplot(mc_data_filt, aes(x= ds_year_type)) +
  geom_bar(aes(fill= mc_binom), position= "dodge") +
  labs(x= "Year type") +
  scale_y_continuous(expand= c(0, 0)) +
  scale_x_discrete(labels= c("Wet", "Dry", "Drought")) +
  scale_fill_manual(values= c("Gray70",  "tomato"),
                    name= "Rating",
                    labels= c("None (1)", "Low (2-3)", "High (4-5)")) +
  facet_rep_grid(Source~Region) +
  theme_ppt


## Station map
ggplot() +
  geom_sf(data= DS_regions, aes(fill= Region), alpha= 0.5) +
  #geom_sf(data= DS_regions, aes(color= Region), fill= "transparent", size= 3) +
  geom_sf(data= DS_waterways, fill= "skyblue3", color= "black") +
  geom_sf(data= mc_stations_filt.sf, fill= "white", color= "black", shape= 21, size= 4) +
  #labs(caption= ">=12 samples per year") +
  #scale_fill_discrete(guide= "none") +
  #scale_color_discrete(guide= "none")  +
  scale_x_continuous(breaks= seq(-122, -121, by= 0.5)) +
  scale_y_continuous(breaks= seq(37.6, 38.6, by= 0.5)) +
  coord_sf() +
  facet_wrap(~Source, nrow= 2) +
  theme_map +
  theme(legend.position = c(0.8, 0.2))
ggsave(last_plot(), filename= "mc_station_map_filtered.png", width= 10, height= 10, dpi= 600,
       path= "Figures")

ggplot() +
  geom_sf(data= DS_regions, aes(fill= SubRegion), alpha= 0.5) +
  #geom_sf(data= DS_regions, aes(color= Region), fill= "transparent", size= 3) +
  geom_sf(data= DS_waterways, fill= "skyblue3", color= "black") +
  geom_sf(data= chla_stations_filt.sf, fill= "white", color= "black", shape= 21, size= 4) +
  labs(caption= ">=12 samples per year") +
  scale_fill_discrete(guide= "none") +
  #scale_color_discrete(guide= "none")  +
  scale_x_continuous(breaks= seq(-122, -121, by= 0.5)) +
  scale_y_continuous(breaks= seq(37.6, 38.6, by= 0.5)) +
  coord_sf() +
  facet_wrap(~Source, nrow= 2) +
  theme_map




#### STATISTICS ####


fit_ac3c <- brm(
  formula = mc_factor ~ 1 + cs(ds_year_type) + Season + Region + (1|Station),
  data = mc_data_filt,
  family = acat("probit"),
  chains= 4,
  iter= 5000,
  warmup= 1000,
  cores= 4,
  control = list(adapt_delta = 0.99)
)

save(fit_ac3c, file= "Data/fit_ac3c.data")
load("Data/fit_ac3.Rdata")
summary(fit_ac3c)
plot(fit_ac3c)
unique(mc_data_filt$Region)

conditions.df <- make_conditions(fit_ac3c, c("Region", "Season"))

#conditional_effects(fit_ac3, "ds_year_type", condition= conditions.df, categorical= TRUE)
term_yt2 <- conditional_effects(fit_ac3c, "ds_year_type", condition= conditions.df, categorical= TRUE)$`ds_year_type`

term_yt <- conditional_effects(fit_ac3, categorical= TRUE)$`ds_year_type`
names(term_yt2)
filter(term_yt2, Season == "Summer")

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



