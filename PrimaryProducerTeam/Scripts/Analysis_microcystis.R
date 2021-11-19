library(tidyverse)
library(lme4)
library(lmerTest)
library(brms)
#library(tidybayes)
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
mc_data_filt %>%
  count(Region, SubRegion)

## WRITE CSV FILES
data %>%
  #select(-chla,-Depth, -SampleType, -`M Chla (µg/L)`, -Field_coords, -LongStationName, -ShortStationName, -HABstation) %>%
  write_csv(., "Data/mcRating_data.csv")

mc_data_filt %>%
 # select(-mc_factor, -chla,-Depth, -SampleType, -`M Chla (µg/L)`, -Field_coords, -LongStationName, -ShortStationName, -HABstation) %>%
  write_csv(., "Data/mcRating_data_filtered.csv")



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

ggplot(mc_data_filt, aes(x= ds_year_type)) +
  geom_bar(aes(fill= mc_mod), position= "dodge") +
  labs(x= "Year type") +
  scale_y_continuous(expand= c(0, 0)) +
  scale_x_discrete(labels= c("Wet", "Below\nAvg", "Drought")) +
  scale_fill_manual(values= c("Gray70", "seagreen4", "seagreen1"),
                    name= "Rating",
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
unique(mc_data$Region)

library(mclogit)
#library(memisc)
fit.all <- mblogit(mc_factor ~ ds_year_type + Season + Region, random= ~1|Station,
                     data= mc_data_filt)

summary(fit.all)


fit.stn.1 <- mblogit(mc_factor ~ ds_year_type + Region, random= ~1|Station,
                     data= mc_stn, method= "MQL")
summary(fit.stn.1)


fit.fmwt.1 <- mblogit(mc_factor ~ ds_year_type + Region, random= ~1|Station,
                     data= mc_fmwt)

unlist(mclogit.control(maxit= 50))
summary(fit.fmwt.1)

control$trace.inner

args(mblogit)

mclogit.control()[["maxit"]]
class(mclogit.control()[[1]])

mclogit.control(maxit= 50)


my_ellipsis_function <- function(x, ...) {
  print(paste("Class of regular-argument:", class(x)))
  print(paste("Number of ellipsis-arguments:", length(list(...))))
}


my_ellipsis_function(x = "Hello World", mtcars, c(1:10), list("Abc", 123))



### BRMS ####
library(brms)

# 12 minutes to run, model converged
fit_ac1 <- brm(
  formula = mc_factor ~ 1 + cs(ds_year_type) + (1|Station),
  data = mc_data_filt,
  family = acat("probit"),
  chains= 2,
  iter= 1000,
  warmup= 500,
  cores= 4
)
746/60

#save(fit_ac1, file= "Data/fit_ac1.Rdata")
load("Data/fit_ac1.Rdata")
getwd()
summary(fit_ac1)
marginal_effects(fit_ac1, "ds_year_type", categorical= TRUE)
?marginal_effects


fit_ac2 <- brm(
  formula = mc_factor ~ 1 + cs(ds_year_type) + (1|Station) + (1|Source),
  data = mc_data_filt,
  family = acat("probit"),
  chains= 3,
  iter= 1000,
  warmup= 500,
  cores= 6
)

summary(fit_ac2)
marginal_effects(fit_ac2, "ds_year_type", categorical= TRUE)

save(fit_ac2, file= "Data/fit_ac2.Rdata")
load("Data/fit_ac2.Rdata")

summary(fit_ac2)



fit_ac3 <- brm(
  formula = mc_factor ~ 1 + cs(ds_year_type) + Season + Region + (1|Station),
  data = mc_data_filt,
  family = acat("probit"),
  chains= 4,
  iter= 5000,
  warmup= 1000,
  cores= 4,
  control = list(adapt_delta = 0.99)
)

#save(fit_ac3, file= "Data/fit_ac3.Rdata")

summary(fit_ac3)
plot(fit_ac3)
#pp_check(fit_ac3, type= "xyz")
conditions.df <- data.frame(Region= c("Suisun Bay", "Suisun Marsh", "North", "SouthCentral", "Confluence"))



fit_srat1 <- brm(
  formula = mc_factor ~ 1 + cs(ds_year_type) + (1|Station) + (1|Source),
  data = mc_data_filt,
  family = sratio("probit"),
  chains= 2,
  iter= 2000,
  warmup= 1000,
  cores= 6
)

summary(fit_srat1)
marginal_effects(fit_srat1, "ds_year_type", categorical= TRUE)
save(fit_srat1, file= "Data/fit_srat1.Rdata")
load("Data/fit_srat1.Rdata")


fit_srat2 <- brm(
  formula = mc_factor ~ 1 + cs(ds_year_type) + Season + Region + (1|Station) + (1|Source),
  data = mc_data_filt,
  family = sratio("probit"),
  chains= 2,
  iter= 2000,
  warmup= 1000,
  cores= 6
)

summary(fit_srat2)
conditional_effects(fit_srat2, "ds_year_type", categorical= TRUE)
conditional_effects(fit_srat2, "Region", categorical= TRUE)
conditional_effects(fit_srat2, "Season", categorical= TRUE)
save(fit_srat2, file= "Data/fit_srat2.Rdata")
load("Data/fit_srat2.Rdata")
?conditional_effects



fit_cumu1b <- brm(
  formula = mc_factor ~ 1 + cs(ds_year_type) + (1|Station) + (1|Source),
  data = mc_data_filt,
  family = cumulative("probit"),
  chains= 3,
  iter= 2000,
  warmup= 1000,
  cores= 6,
  control = list(adapt_delta = 0.95)
)
summary(fit_cumu1b)
summary(fit_cumu1)
save(fit_cumu1b, file= "Data/fit_cumu1b.Rdata")
#load("Data/fit_cumu1.Rdat")
#conditional_effects(fit_cumu1, "ds_year_type", categorical= TRUE)

str(fit_cumu1b)
cumu1b_df <- ggmcmc::ggs(fit_cumu1b)
unique(cumu1b_df$Parameter)

library(ggplot2)
cumu1b_df %>%
  filter(!str_detect(Parameter, "Source") & !str_detect(Parameter, "Station")) %>%
ggplot(data= ., aes(x= Iteration, y= value)) +
  geom_line(aes(color= as.factor(Chain))) +
  facet_wrap(~ Parameter, scales = "free_y") +
  theme_bw()

# https://m-clark.github.io/posts/2021-02-28-practical-bayes-part-i/#divergent-transitions
mcmc_plot(
  fit_cumu1b,
  #pars = c('b_b11', 'b_b21', 'b_x1'),
  type = 'pairs',
  diag_fun = 'dens',
  off_diag_fun = 'hex',
  fixed = TRUE
)

fit_cumu1c <- brm(
  formula = mc_factor ~ 1 + cs(ds_year_type) + Region + Season + (1|Station),
  data = mc_data_filt,
  family = cumulative("probit"),
  chains= 3,
  iter= 2000,
  warmup= 1000,
  cores= 6,
  control = list(adapt_delta = 0.99)
)
summary(fit_cumu1c)
save(fit_cumu1c, file= "Data/fit_cumu1c.Rdata")
load("Data/fit_cumu1c.Rdata")

marginal_effects(fit_cumu1c, "ds_year_type", categorical= TRUE)


fit_cumu2b <- brm(
  formula = mc_factor ~ 1 + cs(ds_year_type) + Season + Region + (1|Station),
  data = mc_data_filt,
  family = cumulative("probit"),
  chains= 4,
  iter= 3000,
  warmup= 1000,
  cores= 8,
  control = list(adapt_delta = 0.99)
)

fit_cumu2c <- brm(
  formula = mc_factor ~ 1 + cs(ds_year_type) + Season + Region + (1|Station),
  data = mc_data_filt,
  family = cumulative("probit"),
  chains= 4,
  iter= 5000,
  warmup= 1000,
  cores= 8,
  control = list(adapt_delta = 0.99)
)

fit_cumu2d <- brm(
  formula = mc_factor ~ 1 + cs(ds_year_type) + Season + Region + (1|Source) + (1|Station),
  data = mc_data_filt,
  family = cumulative("probit"),
  chains= 4,
  iter= 5000,
  warmup= 1000,
  cores= 4,
  control = list(adapt_delta = 0.99)
)


save(fit_cumu2e, file= "Data/fit_cumu2e.Rdata") 

?brm

save(fit_cumu2d, file= "Data/fit_cumu2d.Rdata") 
load("Data/fit_cumu2d.Rdata")
save(mc_data_filt, file= "Data/mc_data_filt.Rdata") 


pairs(fit_cumu2d, variable=c('b_Intercept[1]', 'b_Intercept[2]', 'sd_Source__Intercept', 'sd_Station__Intercept'))
variables(fit_cumu2d)

summary(fit_cumu2d)
plot(fit_cumu2c)
pp_check(fit_cumu2c, type= "xyz")
conditions.df <- data.frame(Region= c("Suisun Bay", "Suisun Marsh", "North", "SouthCentral", "Confluence"))
conditional_effects(fit_cumu2d, 
                    effect= "ds_year_type", 
                    conditions= conditions.df,
                    categorical= TRUE)$`ds_year_type`
unique(mc_data_filt$Region)
year_effect <- conditional_effects(fit_cumu2c, effect= "ds_year_type", categorical= TRUE)
conditional_effects(fit_cumu2c, 
                    effect= "ds_year_type", 
                    conditions= conditions.df,
                    categorical= TRUE)$`ds_year_type`


year_effect_gg <- plot(year_effect, plot = FALSE)[[1]]

region_effect <- conditional_effects(fit_cumu2b, effect= "Region", categorical= TRUE)


year_effect_gg +
  theme_doc

#hypothesis(fit_cumu2b, "ds_year_type.L[1]  > ds_year_type.L[2]", scope= "standard", group= "Station")


#ggsave(year_effect, filename= "mc_rating_BRMS_yearEffect.png", width= 10, height= 10, dpi= 600,
#       path= "Figures")


term_yt <- conditional_effects(fit_cumu2b, categorical= TRUE)$`ds_year_type`
term_yt <- conditional_effects(fit_cumu2d, 
                    effect= "ds_year_type", 
                    conditions= conditions.df,
                    categorical= TRUE)$`ds_year_type`

term_r <- conditional_effects(fit_cumu2b, categorical= TRUE)$`Region`

term_yt2 <- conditional_effects(fit_cumu2b, effect= "ds_year_type", categorical= TRUE)
dterm_all <- conditional_effects(fit_cumu2b, categorical= TRUE)

ggplot(term_yt, aes(x= cats__, y= estimate__, group= ds_year_type)) +
  #geom_point(aes(color= ds_year_type), position= position_dodge(width= 0.3), size= 3) +
  geom_col(aes(fill= ds_year_type), color= "black", position= position_dodge()) +
  geom_errorbar(aes(ymin= lower__, ymax= upper__), width= 0.5, position= position_dodge(0.9)) +
  scale_fill_manual(values= c("skyblue3", "mistyrose2", "tomato"), 
                    name= "Water year type", labels= c("Wet", "Below Avg.", "Drought")) +
  labs(x= "", y= "Probability of rating value") +
  scale_y_continuous(expand= c(0, 0)) +
  scale_x_discrete(labels= c("No\nMicrocystis", "Low\nMicrocystis", "High\nMicrocystis")) +
  facet_rep_wrap(~ Region, nrow= 3) +
  theme_doc +
  theme(legend.position = "top")
ggsave(last_plot(), filename= "MCrating_probs.png", width= 6.5, height= 6, dpi= 300,
       path= "Figures")


ggplot(term_yt, aes(x= ds_year_type, y= estimate__, group= cats__)) +
  geom_errorbar(aes(ymin= lower__, ymax= upper__), position= "jitter") +
  geom_point(aes(color= cats__), position= "jitter") +
  
  
  theme_doc

               


dterm_all[["ds_year_type"]]
loo(fit_ac1, fit_ac2, fit_srat1)
