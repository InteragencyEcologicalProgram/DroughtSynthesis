# Chlorophyll

library(tidyverse)
library(lubridate)
library(lme4)
library(lmerTest)
library(DHARMa)
library(effects)

#Chlorophyll data from KEith
Chl = read_csv("data/chla_data_stats_LT2.csv")
#ok, beautiful! do some averaging
Chl2 = group_by(Chl, Region, Season, month, ds_year) %>%
  summarize(Chla = mean(chlaAvg)) %>%
  group_by( Region, Season, ds_year) %>%
  summarize(Chla = mean(Chla)) %>%
  group_by(Season, ds_year) %>%
  summarize(Chla = mean(Chla), logChl = log(Chla)) %>%
  rename(YearAdj = ds_year)

Chl2reg = group_by(Chl, Region, Season, month, ds_year) %>%
  summarize(Chla = mean(chlaAvg)) %>%
  group_by( Region, Season, ds_year) %>%
  summarize(Chla = mean(Chla), logChl = log(Chla)) %>%
  rename(Year = ds_year) %>%
  left_join(yrs) %>%
  mutate(Yr_type = factor(Yr_type, levels = c("Critical", "Dry", "Below Normal", "Above Normal", "Wet"))) %>%
  filter(Year > 1995)

#plot of chlorophyll by water year type
ggplot(Chl2reg, aes(x = Yr_type, y = Chla, fill = Yr_type))+
  geom_boxplot(alpha = 0.7)+
  facet_grid(Season~Region)+
  drt_color_pal_yrtype()+
  ylab("Chlorophyl ug/L (log-transformed)")+
  xlab("Year Type")+
  theme_bw()+
  scale_x_discrete(labels = c("C", "D", "B", "A", "W")) 

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
Chl = mutate(Chl, Yday = yday(Date), Yr_type = factor(ds_year_type, 
                                                      levels = c("Critical", "Dry", "Below Normal", "Above Normal", "Wet"),
                                                      ordered = TRUE)) %>%
  filter(ds_year > 1989)

ggplot(Chl, aes(x = Yday, y = chlaAvg_log10, color = Yr_type))+
  geom_point(alpha = 0.3)+
  geom_smooth()+
  drt_color_pal_yrtype(aes_type = "color")+
  facet_wrap(~Region)+
  theme_bw()

###############################################################################
#Let's bin the data by >10 versus < 10
Chl = mutate(Chl, bloom = case_when(chlaAvg > 10 ~ TRUE,
                                    TRUE ~ FALSE)) %>%
  filter(Region != "North Delta")


#Binomial model of blooms
#we don't have enough long-term data from the North Delta to include it.

cmod = glmer(bloom ~ Region + Season+Yr_type + (1|Station) + (1|ds_year), family = "binomial", data = Chl)
summary(cmod)
library(visreg)
visreg(cmod)

cmod2 = glmer(bloom ~ Region + Season+Yr_type + (1|Station) + (1|ds_year), family = "binomial", data = Chl)
summary(cmod2)
plot(cmod2)
visreg(cmod2)



#Try it another way, I"m not sure how to tdo this right
#get rid of Winter, because it's not interesting, and filter it to post-clam crash.

Chlsum = Chl %>%
  rename(Year = ds_year) %>%
  left_join(yrs) %>%
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

ggplot(Chlsum, aes(x = ds_year, y = percent, fill = Yr_type)) + geom_col()+
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
