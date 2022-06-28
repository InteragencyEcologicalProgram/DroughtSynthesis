#degree day calculations for the South Delta
#also nutrient plots

library(tidyverse)
library(lubridate)
library(smonitr)
library(readxl)
library(pollen)
library(sf)
library(ggmap)
library(deltamapr)
library(ggsn)


#Now for the degree day calculations
#Ted organized the water quality data for me.

load("data/WQ.daily.RData")
unique(WQ.daily$Site)
library(pollen)

#We just need teh temperature data
TempsD = filter(WQ.daily, Analyte == "Temp")

#use the 'gdd' function in the 'pollen' package to calculate number of degree-days above 19
#Take out Franks from 2015, because we only have half the year.
TempsDDD = mutate(TempsD, Year = year(Date), DOY = yday(Date)) %>%
  group_by(Site, Year) %>%
  mutate(Degreedays = gdd(tmax = Daily.Max, tmin = Daily.Min, tbase = 19, tbase_max = 35),
         MaxDD = max(Degreedays, na.rm = T)) %>%
  filter(!(Site == "FRK" & Year == 2015))

#quick exploritory plot
ggplot(TempsDDD, aes(x =DOY, y = Degreedays, color = as.factor(Year))) + geom_line()+
  geom_hline(aes(yintercept = MaxDD, color = as.factor(Year)), linetype = 2)+
  facet_grid(.~Site, scales = "free_x")+ theme_bw()

#See what the total number of degree days for each year is.
DDs = group_by(TempsDDD, Year, Site) %>%
  summarize(MaxDD = first(MaxDD), firstDay = first(DOY[which(Degreedays > 0)]),
            lastDay = first(DOY[which(Degreedays == MaxDD)]), season = lastDay -firstDay)

#2020 and 2015 were the hottest at all sites
ggplot(DDs, aes(x = as.factor(Year), y = MaxDD))+geom_boxplot()

#2015 also got hot earlier than other years
ggplot(DDs, aes(x = as.factor(Year), y = firstDay))+geom_boxplot()

####################################################

#Average degree days for the south delta by year.

DDyear = mutate(TempsD, Year = year(Date), DOY = yday(Date)) %>%
  filter(!(Site == "FRK")) %>%
  group_by(Year, DOY) %>%
  summarise(Daily.Max = mean(Daily.Max, na.rm = T), Daily.Min = mean(Daily.Min, na.rm = T), WaterMean = mean(Daily.Mean))%>%
  group_by(Year) %>%
  mutate(Degreedays = gdd(tmax = Daily.Max, tmin = Daily.Min, tbase = 19, tbase_max = 35),
         MaxDD = max(Degreedays, na.rm = T)) 

#plot the average degree days for all stations by year.
ggplot(DDyear, aes(x = DOY, y = Degreedays, color = as.factor(Year)))+
  geom_line()+ coord_cartesian(xlim = c(85, 300))+
  geom_hline(aes(yintercept = MaxDD, color = as.factor(Year)), linetype = 2)+
  scale_color_brewer(palette = "Set2", name = NULL)+
  theme_bw()+
  ylab("Degree Days above 19C")+
  scale_x_continuous(breaks = c(91, 152, 213, 274), labels = c("Apr", "Jun", "Aug", "Oct"))

#OK, do air temperature real quick
library(cder)

#unfortunately, we don't have a lot of stations with air temperature
Airtemp = cdec_query(c("RRI", "MSD", "SJR"), 4, "E", start.date = ymd("2015-01-01"), end.date = ymd("2021-12-30"))

#Convert to celcius and calculate the mean, max and min temperature per day. 
AirtempM = Airtemp %>%
  mutate(Year = year(ObsDate), DOY = yday(ObsDate)) %>%
  group_by(Year, DOY, StationID) %>%
  summarise(Temp = mean(Value, na.rm = T), DailyMin = min(Value, na.rm = T), DailyMax = max(Value, na.rm = T)) %>%
  mutate(TempC = (Temp-32)*(5/9), DailyMinC = (DailyMin -32)*(5/9), DailyMaxC = (DailyMax -32)*(5/9))

#average temperatures between stations
AirtempM2 = group_by(AirtempM, Year, DOY) %>%
  summarise(TempC = mean(TempC, na.rm = T), Min = mean(DailyMinC, na.rm = T), Max = mean(DailyMaxC, na.rm = T))

#plot it!
ggplot(AirtempM, aes(x = DOY, y = TempC, color = as.factor(Year)))+
  geom_point(alpha = 0.5, size = 0.5)+
  geom_smooth()+
  ylab("Daily mean air temperture (C)")+
  xlab("Day of Year")+
  scale_color_brewer(palette = "Set2", name = NULL)+
  scale_x_continuous(breaks = c(91, 152, 213, 274), labels = c("Apr", "Jun", "Aug", "Oct"))+
  theme_bw()
#Kinda gross


ggplot(AirtempM2, aes(x = DOY, y = TempC, color = as.factor(Year)))+
  geom_point(alpha = 0.5, size = 0.5)+
  geom_smooth()+
  ylab("Daily mean air temperture (C)")+
  xlab("Day of Year")+
  scale_color_brewer(palette = "Set2", name = NULL)+
  scale_x_continuous(breaks = c(91, 152, 213, 274), labels = c("Apr", "Jun", "Aug", "Oct"))+
  theme_bw()

#Degree days by air temperature

AirDD = AirtempM2 %>%
  group_by( Year) %>%
  mutate(DegreedaysA = gdd(tmax = Max, tmin = Min, tbase = 19, tbase_max = 35),
         MaxDDA = max(DegreedaysA, na.rm = T))


ggplot(AirDD, aes(x = DOY, y = DegreedaysA, color = as.factor(Year)))+
  geom_line()+ coord_cartesian(xlim = c(85, 300))+
  geom_hline(aes(yintercept = MaxDDA, color = as.factor(Year)), linetype = 2)+
  scale_color_brewer(palette = "Set2", name = NULL)+
  theme_bw()+
  ylab("Degree Days above 19C (air temp)")+
  scale_x_continuous(breaks = c(91, 152, 213, 274),
                     labels = c("Apr", "Jun", "Aug", "Oct"))+
  xlab("Day of Year")

#air temperature versus water temperature

alltemp = left_join(DDyear, AirDD)

#plot air temp versus water temp
ggplot(alltemp, aes(x = WaterMean, y = TempC, color = as.factor(Year))) + 
  geom_point()+
  scale_color_brewer(palette = "Set2", name = NULL)+
  geom_smooth(method = "lm")+
  ylab("Air Temperature")+ xlab("Water Temperature")


#switch format for plotting
alltemplong = alltemp %>%
  rename(Water = WaterMean, Air = TempC) %>%
  pivot_longer(cols = c(Water, Air), names_to = "Analyte", values_to = "MeanTemp")

#TEMPERATURE PLOT FOR REPORT
#this is figure 2-20
ggplot(alltemplong, aes(x = DOY, y =`MeanTemp`, color = as.factor(Year))) + 
  geom_point(alpha = 0.1)+
  scale_color_brewer(palette = "Set2", name = NULL)+
  geom_smooth(se = FALSE)+
  coord_cartesian(xlim = c(70, 320), ylim = c(10, 30))+
  ylab("Daily Mean Temp (C)")+
  facet_wrap(~Analyte)+
  theme_bw()+
  scale_x_continuous(breaks = c(91, 152, 213, 274),
                     labels = c("Apr", "Jun", "Aug", "Oct"))+
  xlab("Day of Year")

ggsave("plots/Meantemp.tiff", device = "tiff", width = 6, height = 4)


allDDlong = alltemp %>%
  rename(Water = Degreedays, Air = DegreedaysA) %>%
  pivot_longer(cols = c(Water, Air), names_to = "Analyte", values_to = "DegreeDays")

#Calculate maximum degree days to plot on the graph
Maxes = alltemp %>%
  dplyr::select(MaxDD, MaxDDA, Year) %>%
  rename(Water = MaxDD, Air = MaxDDA) %>%
  pivot_longer(cols = c(Water, Air), names_to = "Analyte", values_to = "MaxDD")%>%
  group_by(Year, Analyte) %>%
  summarize(MaxDD = max(MaxDD, na.rm = T))

#DEGREE DAYS PLOT FOR REPORT
#this is figure 2-21
ggplot(allDDlong, aes(x = DOY, y =`DegreeDays`, color = as.factor(Year))) + 
  coord_cartesian(xlim = c(70, 320))+
  geom_hline(data = Maxes, aes(yintercept = MaxDD, color = as.factor(Year)), linetype = 2, size = 1)+
  scale_color_brewer(palette = "Set2", name = NULL)+
  geom_line(size = 1)+
  facet_wrap(~Analyte)+
  ylab("Degree Days above 19C")+
  theme_bw() +scale_x_continuous(breaks = c(91, 152, 213, 274),
                                labels = c("Apr", "Jun", "Aug", "Oct"))+
  xlab("Day of Year")

write.csv(allDDlong, "outputs/DegreeDays.csv", row.names = FALSE)

#####################################################################
# water quality map
#This is the basis for figure 2-5, but Ted tweaked it a bit in Illustrator
nutsallsf = hab_nutr_chla_mvi %>%
  group_by(Source, Station, Longitude, Latitude) %>%
  summarize(N = n()) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

#continuous stations
WQsta = read_excel("data/continuous stations.xlsx") %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)

ggplot() + geom_sf(data=reg3, aes(fill = Stratum2), alpha = 0.7)+
  scale_fill_manual(values = reg3$colors, guide = NULL)+
  geom_sf(data = WW_Delta, fill = "lightblue")+
  geom_sf(data=nutsallsf, aes(shape = Source))+
  scale_shape(name = "Nutrients")+
  geom_sf(data = WQsta, aes(color = Type), size = 3)+
  scale_color_manual(values = c("blue", "red"), name = "Continuous Stations")+
    geom_sf_label(data = WQsta, aes(label = StationCode), size = 2, label.size  = 0.05, nudge_x = .03)+
    theme_bw()+
  scalebar(dist = 10, dist_unit = "km",
           transform = TRUE, st.dist = .1, x.min = -121.6, x.max = -121.8, y.min = 37.6, y.max = 37.8) +
  
  #there are a number of different optinos for north arrow symbols. ?north
  north(data = reg3, symbol = 2) +
  theme_bw()+ylab("")+xlab("")+
  coord_sf(xlim = c(-121.9, -121.2), ylim = c(37.7, 38.6))+
  xlab(NULL)+ ylab(NULL)

ggsave("plots/WQmap.tiff", device = "tiff", width = 7, height = 9)
ggsave("plots/WQmap.pdf", device = "pdf", width = 7, height = 9)

