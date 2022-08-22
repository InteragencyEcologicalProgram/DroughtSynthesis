# Chlorophyll

library(tidyverse)
library(lubridate)
library(lme4)
library(lmerTest)
library(DHARMa)
library(effects)
library(DroughtData)
library(sf)

#test

#stas = c("P8", "D8", "MD10", "MD10A", "D7", "D6", "D4", "D28A", "D26", "D22", "D19", "D16", "D12", "D10", "C3", "C3A")

#Chlorophyll data from KEith
Chl = read_csv("data/chla_data_stats_LT2.csv")
yrs = read_csv("data/yearassignments.csv")

#Add NCRO
#try more NCRO stuff
NCRO2 = read_csv("data/WQDataReport.SDelta_2000-2021_ChlaPheo.csv")
SDelta_Station_lat_long <- read_csv("PrimaryProducerTeam/Data/SDelta_Station_lat_long.csv")
NCRO2 = mutate(NCRO2, Result = as.numeric(Result),
               Date = mdy_hm(CollectionDate)) %>%
  filter(Analyte == "Chlorophyll a", SampleType == "Normal Sample") %>%
  rename(chlaAvg = Result) %>%
  left_join(SDelta_Station_lat_long) %>%
  filter(!is.na(`Latitude (WGS84)`))

load("Regions.RData")
load("DroughtRegions.RData")
Regions = st_transform(Regions, crs = 4326) %>%
  st_make_valid() 
NCROsf = st_as_sf(NCRO2, coords = c("Longitude (WGS84)", "Latitude (WGS84)"), crs = 4326) %>%
  st_join(Regions) %>%
  st_drop_geometry() %>%
  dplyr::select(ShortStationName, Date, chlaAvg, Region) %>%
  mutate(month = month(Date), Yday = yday(Date), Year = case_when(month == 12 ~ year(Date)+ 1,
                                                                  TRUE ~ year(Date)),
         Season = case_when(month %in% c(3,4,5) ~ "Spring",
                            month %in% c(6,7,8) ~ "Summer",
                            month %in% c(9,10,11) ~ "Fall",
                            month %in% c(12,1,2) ~ "Winter"),
         chlaAvg_log10 = log(chlaAvg, 10)) %>%
  left_join(yrs) %>%
  group_by(ShortStationName, Region, month, Year, Season, Yr_type, Date, Yday) %>%
  summarize(chlaAvg = mean(chlaAvg), chlaAvg_log10 =log(chlaAvg, 10) )

NCRO3 = mutate(NCROsf, Region = "South-Central Delta", Source = "NCRO") %>%
  rename(Station = ShortStationName)

Chl1 = Chl  %>%
  rename(Year = ds_year, Yr_type = ds_year_type) 

Chl1 = Chl1%>%
  left_join(yrs) %>%
  bind_rows(NCRO3) 


#ok, beautiful! do some averaging
Chl2 = Chl1 %>%
  #filter(Station %in% stas) %>% 
  group_by(Region, Season, month, Year) %>%
  summarize(Chla = mean(chlaAvg, na.rm = T)) %>%
  group_by( Region, Season, Year) %>%
  summarize(Chla = mean(Chla)) %>%
  group_by(Season, Year) %>%
  summarize(Chla = mean(Chla), logChl = log(Chla)) %>%
  rename(YearAdj = Year)

Chl2reg = Chl1 %>%
  #filter(Station %in% stas) %>% 
  group_by(Region, Season, month, Year) %>%
  summarize(Chla = mean(chlaAvg, na.rm = T)) %>%
  group_by( Region, Season, Year) %>%
  summarize(Chla = mean(Chla), logChl = log(Chla)) %>%
  rename(Year = Year) %>%
  left_join(yrs) %>%
  mutate(Yr_type = factor(Yr_type, levels = c("Critical", "Dry", "Below Normal", "Above Normal", "Wet")))# %>%
  #filter(Year > 1995)

#plot of chlorophyll by water year type
ggplot(Chl2reg, aes(x = Yr_type, y = logChl, fill = Yr_type))+
  geom_boxplot(alpha = 0.7)+
  facet_grid(Season~Region)+
  drt_color_pal_yrtype()+
  ylab("Chlorophyl ug/L (log-transformed)")+
  xlab("Year Type")+
  theme_bw()+
  scale_x_discrete(labels = c("C", "D", "B", "A", "W")) 

#plot of chlorophyll by water year type
ggplot(Chl2reg, aes(x = Index, y = logChl))+
  geom_point(alpha = 0.7, aes(color =Drought))+
  geom_smooth(method = "lm")+
  facet_grid(Season~Region)+
  drt_color_pal_drought(aes_type = "color")+
  ylab("Chlorophyl ug/L (log-transformed)")+
  xlab("Sac Valley Index")+
  theme_bw()

#plot of chlorophyll by drought
ggplot(Chl2reg, aes(x = Drought, y = logChl, fill = Drought))+
  geom_boxplot(alpha = 0.7)+
  facet_grid(Season~Region)+
 drt_color_pal_drought()+
  ylab("Chlorophyl ug/L (log-transformed)")+
  xlab("Year Type")+
  theme_bw()

#plot of chlorophyll by water year type - just hte more recent years
ggplot(filter(Chl2reg, Year > 1989), aes(x = Yr_type, y = logChl, fill = Yr_type))+
  geom_boxplot(alpha = 0.7)+
  facet_grid(Season~Region)+
  drt_color_pal_yrtype()+
  ylab("Chlorophyl ug/L (log-transformed)")+
  xlab("Year Type")+
  theme_bw()+
  scale_x_discrete(labels = c("C", "D", "B", "A", "W"))
#Bleh, nothing going on there

#plot of chlorophyll by year
ggplot(Chl2reg, aes(x = Year, y = logChl, fill = Yr_type))+
  geom_col(alpha = 0.7)+
  facet_grid(Season~Region)+
  drt_color_pal_yrtype()+
  ylab("Chlorophyl ug/L (log-transformed)")+
  theme_bw()

ggplot(Chl2reg, aes(x = Year, y = logChl, fill = Drought))+
  geom_col(alpha = 0.7)+
  facet_grid(Season~Region)+
  drt_color_pal_drought()+
  ylab("Chlorophyl ug/L (log-transformed)")+
  theme_bw()


#plot of raw chlrophyll data by day of year
Chl = mutate(Chl1, Yday = yday(Date), Yr_type = factor(Yr_type, 
                                                      levels = c("Critical", "Dry", "Below Normal", "Above Normal", "Wet"),
                                                      ordered = TRUE))# %>%
  #filter(Station %in% stas)

ggplot(Chl, aes(x = Yday, y = chlaAvg_log10, color = Yr_type))+
  geom_point(alpha = 0.3)+
  geom_smooth()+
  drt_color_pal_yrtype(aes_type = "color")+
  facet_wrap(~Region)+
  theme_bw()


save(Chl2reg, file = "RegionalCHLaverages.RData")
###############################################################################
#Let's bin the data by >10 versus < 10
Chl = mutate(Chl, bloom = case_when(chlaAvg > 10 ~ TRUE,
                                    TRUE ~ FALSE)) %>%
  filter(Region != "North Delta")


#Binomial model of blooms
#we don't have enough long-term data from the North Delta to include it.

cmod = glmer(bloom ~ Region + Season+Yr_type + (1|Station) + (1|Year), family = "binomial", data = Chl)
summary(cmod)
library(visreg)
visreg(cmod)

cmod2 = glmer(bloom ~ Region + Season+Yr_type + (1|Station) + (1|Year), family = "binomial", data = Chl)
summary(cmod2)
plot(cmod2)
visreg(cmod2)



#Try it another way, I"m not sure how to tdo this right
#get rid of Winter, because it's not interesting, and filter it to post-clam crash.

Chlsum = Chl %>%
  group_by(Region, Season, Drought, Yr_type, month, Year) %>%
  summarize(Presence = length(bloom[which(bloom)]), Absence = length(bloom[which(!bloom)]), N = n()) #%>%
  #filter(Season != "Winter",ds_year >1989)

cmod3 = glmer(cbind(Presence, Absence) ~ Region*Yr_type+ (1|Year), family = "binomial", data = Chlsum)
summary(cmod3)
plot(cmod3)
visreg(cmod3)
visreg(cmod3, xvar = "Yr_type", by = "Region")
visreg(cmod3, xvar = "Region", by = "Yr_type")

res = simulateResiduals(cmod3)
plot(res)

#let's visualize

Chlsumlong = pivot_longer(Chlsum, cols = c(Presence, Absence), names_to = "bloom", values_to = "frequency")
ggplot(Chlsumlong, aes(x = Year, y = frequency, fill = bloom)) + 
  geom_col()+ facet_grid(Season~Region)

Chlsum = mutate(Chlsum, percent = Presence/(Absence + Presence),
                Yr_type = factor(Yr_type, 
                                 levels = c("Critical", "Dry", "Below Normal", "Above Normal", "Wet"),
                                 ordered = TRUE))

ggplot(Chlsum, aes(x = Year, y = percent, fill = Yr_type)) + geom_col()+
  facet_grid(Season~Region) + drt_color_pal_yrtype()+
  ylab("Percent of CHLa samples over 10ug/L")

ggplot(Chlsum, aes(x = Year, y = percent, fill = Drought)) + geom_col()+
  facet_grid(Season~Region) + drt_color_pal_drought()+
  ylab("Percent of CHLa samples over 10ug/L")

ggplot(Chlsum, aes(x = Drought, y = percent, fill = Drought)) + geom_boxplot()+
  facet_grid(Season~Region) + drt_color_pal_drought()+
  ylab("Percent of CHLa samples over 10ug/L")

ggplot(Chlsum, aes(x = Yr_type, y = percent, fill = Yr_type)) + 
  geom_boxplot(alpha = 0.5)+
  facet_grid(Season~Region) + drt_color_pal_yrtype()+
  ylab("Percent of CHLa samples over 10ug/L")+
  theme_bw()

############################################################
#Pre-2000 versus post-2000

Chl2b = mutate(Chl2, Regime = case_when(YearAdj < 2000 ~ "long-term",
                                     YearAdj >= 2000 ~ "Short-Term")) %>%
  rename(Year = YearAdj) %>%
  left_join((yrs))

Chlb = mutate(Chl, Regime = case_when(ds_year < 2000 ~ "long-term",
                                      ds_year >= 2000 ~ "Short-Term"), Date = date(Date)) %>%
  rename(Year = ds_year) %>%
  left_join((yrs)) %>%
  left_join(DF)

Chlbreg = mutate(Chl2reg, Regime = case_when(Year < 2000 ~ "long-term",
                                      Year >= 2000 ~ "Short-Term")) 

ggplot(Chlb, aes(x = log(OUT), y = chlaAvg_log10, color = Regime)) + geom_point()+
  geom_smooth(method = "lm")

ggplot(Chl2b, aes(x = Index, y = logChl, color = Regime)) + geom_point()+
  geom_smooth(method = "lm")+
  facet_grid(~Season)+ ylab("Seasonal average chlorophyll")+
  xlab("Sac Valley Index (annual)")


ggplot(Chlbreg, aes(x = Index, y = logChl, color = Regime)) + geom_point()+
  geom_smooth(method = "lm")+
  facet_grid(Region~Season)+ ylab("Seasonal average chlorophyll")+
  xlab("Sac Valley Index (annual)")
