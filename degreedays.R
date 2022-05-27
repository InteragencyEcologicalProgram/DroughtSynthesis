#degree day calculations for the South Delta
#also nutrient plots

library(tidyverse)
library(lubridate)
library(smonitr)
library(readxl)
library(pollen)
library(sf)

#quick check of hte nurient data
load("Regions.RData")
hab_nutr_chla_mvi <- read_csv("data/hab_nutr_chla_mvi.csv")

#attach regions
# nutsallsf = st_as_sf(hab_nutr_chla_mvi, coords = c("Longitude", "Latitude"), crs = 4326) %>%
#   st_join(reg3) %>%
#   dplyr::select(-nudge, -Stratum) %>%
#   rename(Region = Stratum2)
# 
# save(nutsallsf, file = "nuts_w_regions.RData")

Franks = filter(hab_nutr_chla_mvi, Station %in% c("D19")) %>%
  mutate(Month = month(Date), Year = year(Date), Chlorophyll = as.numeric(Chlorophyll),
         Nitrate = as.numeric(DissNitrateNitrite)) %>%
  filter(Month %in%c(4, 5, 6,7,8,9))

#replace values below the reporting limits with zeros


ggplot(Franks, aes(x= Month, y = Chlorophyll, color = as.factor(Year)))+geom_point() + geom_line() +
  xlab("month - 2021")+ ylab("chlorophyll ug/L")

ggplot(Franks, aes(x= Month, y = Nitrate, color = as.factor(Year)))+geom_point() + geom_line() +
  xlab("Month of Year")+ ylab("Nitrate mg/L")+ theme_bw() 



ggplot(Franks, aes(x= Month, y = Nitrate, color = Station ))+geom_point() + geom_line() +
  xlab("Month of Year")+ ylab("Nitrate")


#Let's look at other areas in teh Delta

Nuts = mutate(hab_nutr_chla_mvi, Month = month(Date), 
              Year = year(Date),Chlorophyll = as.numeric(Chlorophyll),
              Nitrate = as.numeric(DissNitrateNitrite),
              Phosphorus = as.numeric(DissOrthophos)) %>%
  mutate(Chl = case_when(Chlorophyll_Sign == "<" ~ 0,
                         TRUE ~ Chlorophyll),
         Nitrate = case_when(DissNitrateNitrite_Sign == "<" ~ 0,
                             TRUE ~ Nitrate),
         Phosphorus = case_when(DissOrthophos_Sign == "<" ~ 0,
                                TRUE ~ Phosphorus)) %>%
  dplyr::filter(Year == 2021, Month %in% c(2, 3, 4, 5, 6,7,8,9))


ggplot(Nuts, aes(x= Month, y = Nitrate, color = Station ))+geom_point() + geom_line() +
  xlab("month - 2021")+ ylab("Nitrate")




nutssf = st_as_sf(Nuts, coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_join(reg3) %>%
  st_drop_geometry() %>%
 # filter(!is.na(Region)) %>%
  mutate(Station = case_when(Source == "USGS_CAWSC" ~str_sub(Station, start = 6),
                             TRUE ~ Station),
         Ammonium = as.numeric(DissAmmonia)) %>%
  mutate(Ammonium = case_when(DissAmmonia_Sign == "<" ~ 0,
                         TRUE ~ Ammonium))

library(RColorBrewer)
pal = c(brewer.pal(8, "Set2"), brewer.pal(12, "Set3"), brewer.pal(9, "Set1"), brewer.pal(12, "Paired"), "black", "grey")

nutssf %>%
  droplevels() %>%
  filter(!is.na(Nitrate)) %>%
  split(.$Region) %>%
  map(~ ggplot(., aes(x= Date, y = Nitrate, color = Station ))+geom_point() + geom_line() +
        xlab("Date")+ ylab("Nitrate + Nitrite (mg/L)") +
        scale_color_manual(values = pal)+
        annotate("rect", xmin = min(nutssf$Date), 
                 xmax =  max(nutssf$Date),
                 ymin = 0, ymax = 0.04, alpha = 0.5, fill = "gray")+
        ggtitle(.$Region)+
        theme_bw()+
        theme(legend.margin = margin(0, 0,0,0),
              legend.text = element_text(size = 7),
              legend.title = element_blank(), legend.key.size = unit(1, "line"),
              legend.background = element_rect(fill = "transparent"))+
        guides(color = guide_legend(override.aes = list(size = 1), nrow = 7))) %>%
  cowplot::plot_grid(plotlist = ., nrow = 4)

ggsave(filename = "Nitrate.tiff", device = "tiff", width = 10, height = 12)

nutssf %>%
  droplevels() %>%
  filter(!is.na(Ammonium)) %>%
  split(.$Stratum2) %>%
  map(~ ggplot(., aes(x= Date, y =Ammonium, color = Station ))+
        geom_point() + geom_line() +
        xlab("Date")+ ylab("Ammonium (mg/L)") +
        scale_color_manual(values = pal)+
        annotate("rect", xmin = min(nutssf$Date), 
                 xmax =  max(nutssf$Date),
                 ymin = 0, ymax = 0.05, alpha = 0.5, fill = "gray")+
          
        ggtitle(.$Stratum2)+
        theme_bw()+
        theme(legend.margin = margin(0, 0,0,0),
              legend.text = element_text(size = 7),
              legend.title = element_blank(), legend.key.size = unit(1, "line"),
              legend.background = element_rect(fill = "transparent"))+
        guides(color = guide_legend(override.aes = list(size = 1), nrow = 7))) %>%
  cowplot::plot_grid(plotlist = ., nrow = 4)

ggsave(filename = "Ammonium.tiff", device = "tiff", width = 10, height = 12)



nutssf %>%
  droplevels() %>%
  filter(!is.na(Chlorophyll)) %>%
split(.$Stratum2) %>%
  map(~ ggplot(., aes(x= Date, y = Chl, color = Station ))+geom_point() + geom_line() +
        xlab("Date")+ ylab("Chlorophyll (ug/L)") +
        scale_color_manual(values = pal)+
        ggtitle(.$Stratum2)+
        annotate("rect", xmin = min(nutssf$Date), 
                 xmax =  max(nutssf$Date),
                 ymin = 0, ymax = 0.5, alpha = 0.5, fill = "gray")+
        theme_bw()+
        theme(legend.margin = margin(0, 0,0,0),
            legend.text = element_text(size = 7),
              legend.title = element_blank(), legend.key.size = unit(1, "line"),
              legend.background = element_rect(fill = "transparent"))+

        guides(color = guide_legend(override.aes = list(size = 1), nrow = 7))) %>%
  cowplot::plot_grid(plotlist = ., nrow = 4)

ggsave(filename = "Chlorophyll.tiff", device = "tiff", width = 10, height = 12)



nutssf %>%
  droplevels() %>%
  filter(!is.na(Phosphorus)) %>%
  split(.$Stratum2) %>%
  map(~ ggplot(., aes(x= Date, y = Phosphorus, color = Station ))+geom_point() + geom_line() +
        xlab("Date")+ ylab("Dissolved Orthophosphate (mg/L)") +
        scale_color_manual(values = pal)+
        ggtitle(.$Stratum2)+
        theme_bw()+
        annotate("rect", xmin = min(nutssf$Date), 
                 xmax =  max(nutssf$Date),
                 ymin = 0, ymax = 0.05, alpha = 0.5, fill = "gray")+
        theme(legend.margin = margin(0, 0,0,0),
              legend.text = element_text(size = 7),
              legend.title = element_blank(), legend.key.size = unit(1, "line"),
              legend.background = element_rect(fill = "transparent"))+
        guides(color = guide_legend(override.aes = list(size = 1), nrow = 7))) %>%
  cowplot::plot_grid(plotlist = ., nrow = 4)

ggsave(filename = "Orthophos.tiff", device = "tiff", width = 10, height = 12)

P8 = filter(mutate(hab_nutr_chla_mvi, Month = month(Date), 
                   Year = year(Date),Chlorophyll = as.numeric(Chlorophyll),
                   Nitrate = as.numeric(DissNitrateNitrite)), Station %in% c("P8", "D19", "OSJ", "C9"))
ggplot(P8, aes(x = Date, y = Nitrate, color = Station)) + geom_line()
ggplot(filter(P8, !is.na(DissAmmonia)), aes(x = Date, y = as.numeric(DissAmmonia), color = Station)) + geom_line()


###########################################################
#South Delta Nutrients by season and year
SoNuts = mutate(hab_nutr_chla_mvi, Month = month(Date), 
                Year = year(Date),Chlorophyll = as.numeric(Chlorophyll),
                NitrateNitrite = as.numeric(DissNitrateNitrite),
                Ammonium = as.numeric(DissAmmonia), 
                Orthophos = as.numeric(DissOrthophos)) %>%
  mutate(Chl = case_when(Chlorophyll_Sign == "<" ~ 0,
                         TRUE ~ Chlorophyll),
         Nitrate = case_when(DissNitrateNitrite_Sign == "<" ~ 0,
                             TRUE ~ NitrateNitrite),
         Phosphorus = case_when(DissOrthophos_Sign == "<" ~ 0,
                                TRUE ~ Orthophos),
         Ammonium = case_when(DissAmmonia_Sign == "<" ~ 0,
                                TRUE ~ DissAmmonia))


Sonutssf = st_as_sf(SoNuts, coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_join(reg3) %>%
  st_drop_geometry() %>%
  filter(!is.na(Stratum2)) %>%
  mutate(Station = case_when(Source == "USGS_CAWSC" ~str_sub(Station, start = 6),
                             TRUE ~ Station))%>%
  filter(Stratum2 %in% c("Lower SJ", "South Delta", "Lower Sac")) %>%
  mutate(Month = month(Date), Year = year(Date), Season = case_when(
    Month %in% c(12,1,2) ~ "Winter",
    Month %in% c(3,4,5) ~ "Spring",
    Month %in% c(6,7,8) ~ "Summer",
    Month %in% c(9,10,11) ~ "Fall"
  ))
SoNutsmean = Sonutssf %>%
  pivot_longer(cols = c(Ammonium, Chl, Nitrate, Phosphorus), names_to= "Analyte",
               values_to = "Concentration")  %>%
  group_by(Year, Season, Analyte) %>%
summarize(ConcentrationM = mean(Concentration, na.rm = T), 
          SEc = sd(Concentration, na.rm = T)/sqrt(n())) %>%
  mutate(Season = factor(Season, levels = c("Winter", "Spring", "Summer", "Fall")))

ggplot(SoNutsmean, aes(x=Year, y = ConcentrationM, fill = Season)) + geom_col()+
  geom_errorbar(aes(ymin = ConcentrationM - SEc, ymax = ConcentrationM + SEc ))+
  facet_grid(Analyte~Season, scales = "free_y")+
  scale_fill_brewer(palette = "Set2", guide = NULL)+
  ylab("Concentration")+
  theme_bw()

ggsave(filename = "Nutrients.tiff", device = "tiff", width = 6, height = 5)
##################################3
#nutrient statistics
library(lme4)
library(lmerTest)
library(emmeans)
library(DHARMa)
nit = lmer(log(Nitrate+0.04) ~ as.factor(Year) + Season + (1|Month)+ (1|Station),  data = Sonutssf)
summary(nit)
plot(nit)
nitres = simulateResiduals(nit)
plot(nitres)
#OK, some issues

Amm = lmer(log(Ammonium+0.05) ~ as.factor(Year)+Season + (1|Month)+ (1|Station),  data = Sonutssf)
summary(Amm)
plot(simulateResiduals(Amm))



chl= lmer(log(Chl+0.01) ~ as.factor(Year) + Season + (1|Month)+ (1|Station),  data = Sonutssf)
summary(chl)
plot(simulateResiduals(chl))

Orth= lmer(log(Phosphorus+0.05) ~ as.factor(Year) + Season + (1|Month)+ (1|Station),  data = Sonutssf)
summary(Orth)
plot(Orth)
plot(simulateResiduals(Orth))


#pllot for report##################
ggplot(SoNutsmean, aes(x=Year, y = ConcentrationM, fill = Season)) + geom_col()+
  geom_errorbar(aes(ymin = ConcentrationM - SEc, ymax = ConcentrationM + SEc ))+
  facet_grid(Analyte~Season, scales = "free_y")+
  scale_fill_brewer(palette = "Set2", guide = NULL)+
 # geom_text(data = tuk, aes(x = Year, y = 0, label = group), inherit.aes = FALSE)+
  ylab("Concentration")+
  theme_bw()

#Meh, maybe I don't show the letters?

Amg = plot(emmeans(Amm, specs = "Year", by = "Season"), comparison = T)+
  xlab("Estimated Marginal Mean")+
  ggtitle("Ammonia")

Nitg = plot(emmeans(nit, specs = "Year", by = "Season"), comparison = T)+
  xlab("Estimated Marginal Mean")+
  ggtitle("Nitrate + Nitrite")

CHLg = plot(emmeans(chl, specs = "Year", by = "Season"), comparison = T)+
  xlab("Estimated Marginal Mean")+
  ggtitle("Chlorophyll")

Orthg = plot(emmeans(Orth, specs = "Year", by = "Season"), comparison = T)+
  xlab("Estimated Marginal Mean")+
  ggtitle("Orthophosphate")

cowplot::plot_grid(Amg, CHLg, Nitg, Orthg, nrow = 2)

ggsave(filename = "Nutsemmeans.tiff", device = "tiff", path = "plots/", 
       width = 8, height = 10)

##########################################################################################

#Now for the degree day calculations

#load("data/WQ.RData")

#summary(WQ)
#unique(WQ$Analyte)

#Temps = filter(WQ, Analyte == "Temp")

#ggplot(Temps, aes(x = DateTime, y = Amount)) + geom_point()

load("data/WQ.daily.RData")
unique(WQ.daily$Site)
library(pollen)
TempsD = filter(WQ.daily, Analyte == "Temp")

Degreedays = gdd(tmax = TempsD$Daily.Max, tmin = TempsD$Daily.Min, tbase = 19, tbase_max = 35)

TempsDDD = mutate(TempsD, Year = year(Date), DOY = yday(Date)) %>%
  group_by(Site, Year) %>%
  mutate(Degreedays = gdd(tmax = Daily.Max, tmin = Daily.Min, tbase = 19, tbase_max = 35),
         MaxDD = max(Degreedays, na.rm = T)) %>%
  filter(!(Site == "FRK" & Year == 2015))

ggplot(TempsDDD, aes(x =DOY, y = Degreedays, color = Year)) + geom_line()+
  geom_hline(aes(yintercept = MaxDD, color = Year), linetype = 2)+
  facet_grid(.~Site, scales = "free_x")

ggplot(TempsDDD, aes(x =DOY, y = Degreedays, color = Year)) + geom_line()+
  geom_hline(aes(yintercept = MaxDD, color = Year), linetype = 2)+
  facet_grid(Year~Site, scales = "free_x")


ggplot(TempsDDD, aes(x =DOY, y = Degreedays, color = as.factor(Year))) + geom_line()+
  geom_hline(aes(yintercept = MaxDD, color = as.factor(Year)), linetype = 2)+
  facet_grid(.~Site, scales = "free_x")+ theme_bw()

DDs = group_by(TempsDDD, Year, Site) %>%
  summarize(MaxDD = first(MaxDD), firstDay = first(DOY[which(Degreedays > 0)]),
            lastDay = first(DOY[which(Degreedays == MaxDD)]), season = lastDay -firstDay)

ggplot(DDs, aes(x = as.factor(Year), y = MaxDD))+geom_boxplot()
ggplot(DDs, aes(x = as.factor(Year), y = firstDay))+geom_boxplot()


ggplot(filter(TempsDDD, DOY >90, DOY <160), aes(x =DOY, y = Degreedays, color = as.factor(Year))) + geom_line()+
 # geom_hline(aes(yintercept = MaxDD, color = Year), linetype = 2)+
  facet_grid(.~Site, scales = "free_x")

####################################################

#Average degree days for the south delta by year.

DDyear = mutate(TempsD, Year = year(Date), DOY = yday(Date)) %>%
  filter(!(Site == "FRK")) %>%
  group_by(Year, DOY) %>%
  summarise(Daily.Max = mean(Daily.Max, na.rm = T), Daily.Min = mean(Daily.Min, na.rm = T), WaterMean = mean(Daily.Mean))%>%
  group_by(Year) %>%
  mutate(Degreedays = gdd(tmax = Daily.Max, tmin = Daily.Min, tbase = 19, tbase_max = 35),
         MaxDD = max(Degreedays, na.rm = T)) 

ggplot(DDyear, aes(x = DOY, y = Degreedays, color = as.factor(Year)))+
  geom_line()+ coord_cartesian(xlim = c(85, 300))+
  geom_hline(aes(yintercept = MaxDD, color = as.factor(Year)), linetype = 2)+
  scale_color_brewer(palette = "Set2", name = NULL)+
  theme_bw()+
  ylab("Degree Days above 19C")+
  scale_x_continuous(breaks = c(91, 152, 213, 274), labels = c("Apr", "Jun", "Aug", "Oct"))

#OK, do air temperature real quick
library(cder)
library(cimir)

Airtemp = cdec_query(c("HBP", "RRI", "MSD", "SJR"), 4, "E", start.date = ymd("2015-01-01"), end.date = ymd("2021-12-30"))

AirtempM = Airtemp %>%
  dplyr::filter(Airtemp, StationID != "SJR") %>%
  mutate(Year = year(ObsDate), DOY = yday(ObsDate)) %>%
  group_by(Year, DOY, StationID) %>%
  summarise(Temp = mean(Value, na.rm = T), DailyMin = min(Value, na.rm = T), DailyMax = max(Value, na.rm = T)) %>%
  mutate(TempC = (Temp-32)*(5/9), DailyMinC = (DailyMin -32)*(5/9), DailyMaxC = (DailyMax -32)*(5/9))

AirtempM2 = group_by(AirtempM, Year, DOY) %>%
  summarise(TempC = mean(TempC, na.rm = T), Min = mean(DailyMinC, na.rm = T), Max = mean(DailyMaxC, na.rm = T))

ggplot(AirtempM, aes(x = DOY, y = TempC, color = as.factor(Year)))+
  geom_point(alpha = 0.5, size = 0.5)+
  geom_smooth()+
  ylab("Daily mean air temperture (C)")+
  xlab("Day of Year")+
  scale_color_brewer(palette = "Set2", name = NULL)+
  scale_x_continuous(breaks = c(91, 152, 213, 274), labels = c("Apr", "Jun", "Aug", "Oct"))+
  theme_bw()

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


ggplot(AirDD, aes(x = DOY, y = Degreedays, color = as.factor(Year)))+
  geom_line()+ coord_cartesian(xlim = c(85, 300))+
  geom_hline(aes(yintercept = MaxDD, color = as.factor(Year)), linetype = 2)+
  scale_color_brewer(palette = "Set2", name = NULL)+
  theme_bw()+
  ylab("Degree Days above 19C (air temp)")+
  scale_x_continuous(breaks = c(91, 152, 213, 274),
                     labels = c("Apr", "Jun", "Aug", "Oct"))+
  xlab("Day of Year")

#air temperature versus water temperature

alltemp = left_join(DDyear, AirDD)

ggplot(alltemp, aes(x = WaterMean, y = TempC, color = as.factor(Year))) + 
  geom_point()+
  scale_color_brewer(palette = "Set2", name = NULL)+
  geom_smooth(method = "lm")

ggplot(alltemp, aes(x = DOY, y = TempC, color = as.factor(Year))) + 
  geom_point(alpha = 0.5)+
  scale_color_brewer(palette = "Set2", name = NULL)+
  geom_smooth(se = FALSE)

ggplot(alltemp, aes(x = DOY, y = WaterMean, color = as.factor(Year))) + 
  geom_point(alpha = 0.5)+
  scale_color_brewer(palette = "Set2", name = NULL)+
  geom_smooth(se = FALSE)

alltemplong = alltemp %>%
  rename(Water = WaterMean, Air = TempC) %>%
  pivot_longer(cols = c(Water, Air), names_to = "Analyte", values_to = "MeanTemp")

#TEMPERATURE PLOT FOR REPORT
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

Maxes = alltemp %>%
  dplyr::select(MaxDD, MaxDDA, Year) %>%
  rename(Water = MaxDD, Air = MaxDDA) %>%
  pivot_longer(cols = c(Water, Air), names_to = "Analyte", values_to = "MaxDD")%>%
  group_by(Year, Analyte) %>%
  summarize(MaxDD = max(MaxDD, na.rm = T))

#DEGREE DAYS PLOT FOR REPORT
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


#####################################################################
# water quality map
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

ggsave("WQmap.tiff", device = "tiff", width = 7, height = 9)
ggsave("plots/WQmap.pdf", device = "pdf", width = 7, height = 9)

###############################################################
#N:P ratio

Ratios = read.csv("data/HABs/DWR_NutChl_NP_ratio.csv") %>%
  mutate(Date = mdy(Date), Month = month(Date)) %>%
  filter(RegionName != "Suisun")


Ratiossum = group_by(Ratios, Year, Month, RegionName) %>%
  summarize(NPratioM = mean(NPratio, na.rm = T), sdNP = sd(NPratio, na.rm = T), minNP = min(NPratio), maxNP = max(NPratio))

ggplot(filter(Ratiossum, Month %in% c(6,7,8,9)), aes(x = Month, y = NPratioM)) + geom_col()+ facet_grid(RegionName~Year)+
 geom_hline(yintercept = 16, color = "red")+
  geom_errorbar(aes(ymin = NPratioM-sdNP, ymax = NPratioM + sdNP))

ggplot(Ratiossum, aes(x = Month, y = NPratioM, fill = RegionName)) + geom_col()+ 
  facet_grid(RegionName~Year, scales = "free_y")+
  geom_hline(yintercept = 16, color = "red")+
  geom_errorbar(aes(ymin = minNP, ymax = maxNP))+
  scale_fill_manual(values = reg3$colors, guide = NULL)+
  scale_x_continuous(breaks = c(2,4,6,8,10))+
  xlab("Month of Year")+ ylab("N:P Ratio")+
  theme_bw()
