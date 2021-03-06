## Long term drought synthesis analysis code for Chlorophyll A

## Libraries
library(tidyverse)
library(lme4)
library(lmerTest)
library(emmeans)
source("Scripts/MyFunctionsAndThemes.R")

## Load data frames from Data_format.R
load("Data/DS_dataframesLT.Rdata")



#### DATA FILTERING ####
## Filter data based on
# Minimum samples per year per site
# Minimum number of years sampled at a site
# Regions to exclude from the analysis
# Seasons to include in the analysis
# Minimum result threshold
chla_data_filt_list <- filter_chla_data(data= DS_chlaLT,
                                        min_samps_yr = 12,
                                        min_yrs= 10,
                                        excluded_regions = c("Far West", "Suisun Marsh"),
                                        seasons= c("Summer", "Fall", "Spring", "Winter"),
                                        min_result = 0)


chla_data_filt <- chla_data_filt_list$data
chla_stations_filt.sf <- chla_data_filt_list$stations


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

## Year Type mean
mean_YT <- chla_data_stats %>% 
  group_by(ds_year_type) %>% 
  summarize(mean_chla= mean(chlaAvg_log10),
            med_chla= median(chlaAvg_log10))

mean_YT <- chla_data_stats %>% 
  group_by(ds_year_type) %>% 
  summarize(mean_chla= mean(chlaAvg_log10),
            med_chla= median(chlaAvg_log10))

mean_YT_R_S <- chla_data_stats %>% 
  group_by(ds_year_type, Region, Season) %>% 
  summarize(mean_chla= mean(chlaAvg_log10),
            med_chla= median(chlaAvg_log10))


chla_data_stats %>% 
  group_by(ds_year_type) %>% 
  summary(.$chlaAvg_log10)

## WRITE CSV FILE
chla_data_stats %>%
 select(Date, everything(), -chlaAvg_log) %>%
  write_csv(., "Data/chla_data_stats_LT.csv")


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
  select(Source, Region, Source, Station) %>% 
  distinct(.) %>% 
  group_by(Region, Source) %>%
  count(Region)
sum(Station_count$n)

Year_type_summary <- chla_data_stats %>%
  group_by(Source, month, Region, SubRegion) %>%
  count(ds_year_type)


#### STATISTICS ####
#rm(DS_chlaLT, DS_regions, DS_waterways, chla_data_filt_list, chla_data_filt)
options(contrasts = c("contr.sum","contr.poly"))


## Model 1: chla ~ year type
fit_log10.1 <- lmer(chlaAvg_log10 ~ ds_year_type + (1|Station),
                    data= chla_data_stats)

summary(fit_log10.1)
plot(fit_log10.1)
anova(fit_log10.1, type= 2, ddf= "Satterthwaite")
#emm_year1 <- emmeans(fit_log10.1, specs= "ds_year_type", pbkrtest.limit = nrow(chla_data_stats))
#save(emm_year1, file= "Data/emm_year1_LT.Rdata")
load("Data/emm_year1_LT.Rdata")
pairs(emm_year1)
plot(emm_year1, comparisons= TRUE)
pwpm(emm_year1)


# emmeans1 results for boxplots
emm_year_results1 <- tibble(ds_year_type= c("Critical", "Dry", "Below Normal", "Above Normal", "Wet"),
                            chlaAvg_log10= rep(2.8, 5),
                            emm_group= c("a", "ab", "c", "c", "b"))


## Model 2: chla ~ year type + Season + Region
fit_log10.2 <- lmer(chlaAvg_log10 ~ ds_year_type + Season + Region + (1|Station),
                    data= chla_data_stats)
summary(fit_log10.2)
plot(fit_log10.2)
anova(fit_log10.2, type= 2, ddf= "Satterthwaite")
#ref_grid(fit_log10.2)@grid
#emm_year2 <- emmeans(fit_log10.2, specs= "ds_year_type", pbkrtest.limit = nrow(chla_data_stats))
#save(emm_year2, file= "Data/emm_year2_LT.Rdata")
load("Data/emm_year2_LT.Rdata")
pairs(emm_year2)
plot(emm_year2, comparisons= TRUE)
pwpp(emm_year2)

# emmeans1 results for boxplots
emm_year_results2 <- tibble(ds_year_type= c("Critical", "Dry", "Below Normal", "Above Normal", "Wet"),
                            chlaAvg_log10= rep(2.8, 5),
                            emm_group= c("", "a", "b", "b", "a"))




## Region is non-significant in ANOVA

#emm_Region2 <- emmeans(fit_log10.2, specs= "Region", pbkrtest.limit = nrow(chla_data_stats))
#save(emm_Region2, file= "Data/emm_Region2.Rdata")
#load("Data/emm_Region2.Rdata")
pairs(emm_Region2)

#emm_season2 <- emmeans(fit_log10.2, specs= "Season", pbkrtest.limit = nrow(chla_data_stats))
#save(emm_season2, file= "Data/emm_season2.Rdata")
load("Data/emm_season2.Rdata")
pairs(emm_season2)

#emm_YearType_RegionSeason2_LT <- emmeans(fit_log10.2, specs= c("ds_year_type", "Region", "Season"), pbkrtest.limit = nrow(chla_data_stats))
#save(emm_YearType_RegionSeason2_LT, file= "Data/emm_YearType_RegionSeason2_LT.Rdata")
pwpp(emm_YearType_RegionSeason2_LT)
pairs(emm_YearType_RegionSeason2_LT)
plot(emm_YearType_RegionSeason2_LT, comparison = TRUE, CI= FALSE)

emmp_tib <- as_tibble(emm_YearType_RegionSeason2_LT@grid)

pwpp(emm_YearType_RegionSeason2_LT[str_detect(emmp_tib$Region, "South-Central Delta"), ], by= "Season", sort= FALSE)
pwpm(emm_YearType_RegionSeason2_LT[str_detect(emmp_tib$Region, "South-Central Delta"), ], by= "Season")


chla_data_stats %>% 
  filter(Region == "South-Central Delta") %>% 
  group_by(ds_year_type, Region, Season) %>% 
  summarize(mean_chla= mean(chlaAvg_log10))



### FIGURES ####
yr_type_labels <- c("Critical", "Dry", "Below\nNormal", "Above\nNormal", "Wet")

ggplot(year_summary, aes(x= ds_year, y= n)) +
  geom_col(aes(fill= Source)) +
  facet_rep_wrap(~Region, ncol= 1) +
  labs(x= "Station-months per year", y= "Count") +
  scale_y_continuous(expand= c(0, 0), breaks= seq(0, 600, by= 100), labels= c("0", "", "200", "", "400", "", "600")) +
  theme_doc +
  theme(axis.text.x = element_text(angle= 90, vjust= 0.5),
        legend.position = "top")
ggsave(last_plot(), filename= "chla_year_sample_summary_LT.png", width= 6.5, height= 5, dpi= 300,
       path= "Figures")



## Station map
ggplot() +
  geom_sf(data= DS_regions, aes(fill= Region), alpha= 0.5) +
  geom_sf(data= DS_waterways, fill= "skyblue3", color= "black") +
  geom_sf(data= chla_stations_filt.sf, fill= "white", color= "black", shape= 21, size= 3) +
  scale_x_continuous(breaks= seq(-122, -121, by= 0.5)) +
  scale_y_continuous(breaks= seq(37.6, 38.6, by= 0.5)) +
  coord_sf() +
  facet_wrap(~Source, nrow= 1) +
  guides(fill=guide_legend(nrow=2,byrow=TRUE)) +
  theme_map +
  theme(legend.position = "top")

ggsave(last_plot(), filename= "station_map_chla_filt_LT.png", width= 6.5, height= 5, dpi= 600,
       path= "Figures")

## BOXPLOTS BELOW ##

## Yeartype only
ggplot(chla_data_stats, aes(x= ds_year_type, y= chlaAvg_log10)) +
  geom_boxplot(aes(fill= ds_year_type), outlier.size= 1, outlier.color= "gray60") +
  stat_summary(fun=mean, geom="point", shape=18, size=2, color="white") +
  geom_text(data= emm_year_results2, aes(x= ds_year_type, y= chlaAvg_log10, label= emm_group)) +
  labs(x= "Year type", y= expression(paste("Chlorophyll-a (", mu, "g/L)"))) +
  scale_y_continuous(breaks= c(-2, -1, 0, 1, 2),
                     labels= c("0.01", "0.1", "1", "10", "100")) +
  scale_x_discrete(labels= yr_type_labels) +
  scale_fill_manual(values= sacI.colors, name= "Water year", guide= "none") +
  #scale_fill_discrete_diverging(rev= TRUE, name= "Water year", guide= "none") +
  annotation_logticks(side= "l") +
  theme_bw(base_size = 12) +
  #theme_doc +
  theme(legend.position = "top")
ggsave(last_plot(), filename= "chla_filtered_YearTypeOnly_LT.png", width= 6.5, height= 5, dpi= 300,
      path= "Figures")

## Year time series
ggplot(chla_data_stats, aes(x= ds_year, y= chlaAvg_log10)) +
  geom_boxplot(aes(fill= ds_year_type), outlier.size= 1, outlier.color= "gray60") +
  stat_summary(fun=mean, geom="point", shape=18, size=1, color="white") +
  labs(x= "Year", y= expression(paste("Chlorophyll-a (", mu, "g/L)"))) +
  scale_y_continuous(breaks= c(-2, -1, 0, 1, 2),
                     labels= c("0.01", "0.1", "1", "10", "100")) +
  scale_x_discrete(expand= c(0.02, 0)) +
  scale_fill_manual(values= sacI.colors, name= "Water year") +
  annotation_logticks(side= "l") +
  guides(fill=guide_legend(nrow=2, byrow=TRUE)) +
  theme_bw(base_size = 12) +
  #theme_doc +
  theme(axis.text.x = element_text(angle= 90, vjust= 0.5),
        legend.position = "top")
ggsave(last_plot(), filename= "chla_filtered_TimeSeries_LT.png", width= 6.5, height= 5, dpi= 300,
       path= "Figures")


## Yeartype and Regions
ggplot(chla_data_stats, aes(x= ds_year_type, y= chlaAvg_log10)) +
  geom_boxplot(aes(fill= ds_year_type), outlier.size= 1, outlier.color= "gray60") +
  stat_summary(fun=mean, geom="point", shape=18, size=1, color="white") +
  labs(x= "Year type", y= expression(paste("Chlorophyll-a (", mu, "g/L)"))) +
  scale_y_continuous(breaks= c(-2, -1, 0, 1, 2),
                     labels= c("0.01", "0.1", "1", "10", "100")) +
  scale_x_discrete(labels= yr_type_labels) +
  scale_fill_manual(values= sacI.colors, name= "Water year", guide= "none") +
  annotation_logticks(side= "l") +
  facet_rep_wrap(~ Region, ncol= 2, repeat.tick.labels = TRUE) +
  #guides(fill=guide_legend(nrow=2, byrow=TRUE)) +
  theme_bw(base_size = 12) +
  #theme_doc +
  theme(legend.position = "top")
  #theme(legend.position = c(0.77, 0.15), 
  #      legend.direction = "vertical")
ggsave(last_plot(), filename= "chla_filtered_Region_LT.png", width= 6.5, height= 6, dpi= 300,
       path= "Figures")


## Region and Season
ggplot(chla_data_stats, aes(x= ds_year_type, y= chlaAvg_log10)) +
  geom_boxplot(aes(fill= Season), outlier.size= 1, outlier.color= "gray60") +
  labs(x= "Year type", y= expression(paste("Chlorophyll-a (", mu, "g/L)"))) +
  scale_y_continuous(breaks= c(-2, -1, 0, 1, 2),
                     labels= c("0.01", "0.1", "1", "10", "100")) +
  scale_x_discrete(labels= yr_type_labels) +
  scale_fill_manual(values= season.colors) +
  annotation_logticks(side= "l") +
  facet_rep_wrap(~ Region, ncol= 2, repeat.tick.labels = TRUE) +
  theme_bw(base_size = 12) +
  #theme_doc +
  theme(legend.position = "top")
  
ggsave(last_plot(), filename= "chla_filtered_SeasonRegion_LT.png", width= 6.5, height= 6, dpi= 300,
       path= "Figures")

## Year Type and Season
ggplot(chla_data_stats, aes(x= Season, y= chlaAvg_log10)) +
  geom_boxplot(aes(fill= ds_year_type), outlier.size= 1, outlier.color= "gray60") +
  geom_point(data= mean_YT_R_S, aes(x= Season, y= mean_chla, group= ds_year_type), 
             color= "white", shape= 18, size= 0.75, position= position_dodge(width= 0.75)) +
 # stat_summary(fun=mean, geom="point", shape= 8, size= 3, color="red", fill="red") +
  labs(x= "Season", y= expression(paste("Chlorophyll-a (", mu, "g/L)"))) +
  scale_y_continuous(breaks= c(-2, -1, 0, 1, 2),
                     labels= c("0.01", "0.1", "1", "10", "100")) +
  #scale_fill_manual(values= sacI.colors, name= "Water year", guide= "none") +
  annotation_logticks(side= "l") +
  facet_rep_wrap(~ Region, ncol= 2, repeat.tick.labels = TRUE) +
  guides(fill=guide_legend(nrow=2, byrow=TRUE)) +
  theme_bw(base_size = 12) +
  #theme_doc +
  theme(legend.position = "top")
ggsave(last_plot(), filename= "chla_filtered_SeasonRegion2_LT.png", width= 6.5, height= 6, dpi= 300,
       path= "Figures")



## Regions combined and Season
ggplot(chla_data_stats, aes(x= Season, y= chlaAvg_log10)) +
  geom_boxplot(aes(fill= ds_year_type), outlier.size= 1, outlier.color= "gray60") +
  labs(x= "Season", y= expression(paste("Chlorophyll-a (", mu, "g/L)"))) +
  scale_y_continuous(breaks= c(-2, -1, 0, 1, 2),
                     labels= c("0.01", "0.1", "1", "10", "100")) +
  scale_fill_manual(values= sacI.colors, name= "Water year", guide= "none") +
  #scale_fill_discrete_diverging(rev= TRUE, name= "Water year") +
  annotation_logticks(side= "l") +
  guides(fill=guide_legend(nrow=2, byrow=TRUE)) +
  theme_bw(base_size = 12) +
  #theme_doc +
  theme(legend.position = "top")
ggsave(last_plot(), filename= "chla_filtered_Season_LT.png", width= 6.5, height= 5, dpi= 300,
       path= "Figures")

## Density plot
ggplot(chla_data_stats, aes(x= chlaAvg_log10)) +
  geom_density(aes(color= ds_year_type), size= 1.5) +
  labs(x= expression(paste("Chlorophyll-a (", mu, "g/L)")), y= "Probability density", ) +
  scale_y_continuous(expand= c(0.01, 0)) +
  scale_x_continuous(breaks= c(0, 1, 2), labels= c(1, 10, 100)) +
  #scale_color_manual(values= year.colors, name= "Water Year Type", labels= c("Wet", "Below Avg.", "Drought")) +
  scale_color_discrete_diverging(rev= TRUE, name= "Water year") +
  annotation_logticks(side= "b") +
  #facet_rep_grid(. ~ Season, repeat.tick.labels = TRUE) +
  theme_doc +
  theme(legend.position = c(0.8, 0.75), 
        legend.direction = "vertical")
ggsave(last_plot(), filename= "chla_filtered_density_LT.png", width= 6.5, height= 4, dpi= 300,
       path= "Figures")

