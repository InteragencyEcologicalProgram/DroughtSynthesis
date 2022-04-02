#degree day calculations for the South Delta

library(tidyverse)
library(lubridate)
library(smonitr)
library(readxl)
library(pollen)
library(sf)

#quick check of hte nurient data

hab_nutr_chla_mvi <- read_excel("data/hab_nutr_chla_mvi.xlsx")

Franks = filter(hab_nutr_chla_mvi, Station %in% c("OSJ", "FAL", "FCT", "BET", "D19")) %>%
  mutate(Month = month(Date), Year = year(Date), Chlorophyll = as.numeric(Chlorophyll),
         Nitrate = as.numeric(DissNitrateNitrite)) %>%
  filter(Year == 2021, Month %in%c(2, 3, 4, 5, 6,7,8,9))

ggplot(Franks, aes(x= Month, y = Chlorophyll, color = Station ))+geom_point() + geom_line() +
  xlab("month - 2021")+ ylab("chlorophyll ug/L") 




ggplot(Franks, aes(x= Month, y = Nitrate, color = Station ))+geom_point() + geom_line() +
  xlab("month - 2021")+ ylab("Nitrate")


#Let's look at other areas in teh Delta

Nuts = mutate(hab_nutr_chla_mvi, Month = month(Date), 
              Year = year(Date),Chlorophyll = as.numeric(Chlorophyll),
              Nitrate = as.numeric(DissNitrateNitrite)) %>%
  dplyr::filter(Year == 2021, Month %in% c(2, 3, 4, 5, 6,7,8,9))


ggplot(Nuts, aes(x= Month, y = Nitrate, color = Station ))+geom_point() + geom_line() +
  xlab("month - 2021")+ ylab("Nitrate")



reg2 = R_EDSM_Regions_1819P1 %>%
  st_transform(crs = st_crs(4326))

reg3 = R_EDSM_Strata_1718P1%>%
  st_transform(crs = st_crs(4326)) %>%
  mutate(Stratum2 = factor(Stratum, 
                           levels = c("Western Delta", "Suisun Bay", "Suisun Marsh", "Lower Sacramento",
                                      "Lower San Joaquin", "Eastern Delta", "Southern Delta",
                                      "Cache Slough/Liberty Island", "Sac Deep Water Shipping Channel",
                                      "Upper Sacramento"), 
                           labels = c("Far West", "Suisun Bay", "Suisun Marsh", "Lower Sac",
                                      "Lower SJ", "East Delta", "South Delta", "Cache/Liberty", "SDWSC",
                                      "Upper Sac")),
         nudge = c(-.05,0,0,0,0,0,0,0,.1,0))

nutssf = st_as_sf(Nuts, coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_join(reg3) %>%
  st_drop_geometry() %>%
  filter(!is.na(Stratum2)) %>%
  mutate(Station = case_when(Source == "USGS_CAWSC" ~str_sub(Station, start = 6),
                             TRUE ~ Station),
         Ammonium = as.numeric(DissAmmonia))

library(RColorBrewer)
pal = c(brewer.pal(8, "Set2"), brewer.pal(12, "Set3"), brewer.pal(9, "Set1"), brewer.pal(12, "Paired"), "black", "grey")

nutssf %>%
  droplevels() %>%
  filter(!is.na(Nitrate)) %>%
  split(.$Stratum2) %>%
  map(~ ggplot(., aes(x= Date, y = Nitrate, color = Station ))+geom_point() + geom_line() +
        xlab("Date")+ ylab("Nitrage (mg/L)") +
        scale_color_manual(values = pal)+
        annotate("rect", xmin = min(nutssf$Date), 
                 xmax =  max(nutssf$Date),
                 ymin = 0, ymax = 0.04, alpha = 0.5, fill = "gray")+
        ggtitle(.$Stratum2)+
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
        annotate("text", x = median(nutssf$Date), 
y= 0.06, label = "EMP reporting limit")+
        
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
  map(~ ggplot(., aes(x= Date, y = Chlorophyll, color = Station ))+geom_point() + geom_line() +
        xlab("Date")+ ylab("Chlorophyll (ug/L)") +
        scale_color_manual(values = pal)+
        ggtitle(.$Stratum2)+
        theme_bw()+
        theme(legend.margin = margin(0, 0,0,0),
            legend.text = element_text(size = 7),
              legend.title = element_blank(), legend.key.size = unit(1, "line"),
              legend.background = element_rect(fill = "transparent"))+
        guides(color = guide_legend(override.aes = list(size = 1), nrow = 7))) %>%
  cowplot::plot_grid(plotlist = ., nrow = 4)

ggsave(filename = "Chlorophyll.tiff", device = "tiff", width = 10, height = 12)

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
                Orthophos = as.numeric(DissOrthophos))


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
  pivot_longer(cols = c(Ammonium, Chlorophyll, NitrateNitrite, Orthophos), names_to= "Analyte",
               values_to = "Concentration")  %>%
  group_by(Year, Season, Analyte) %>%
summarize(ConcentrationM = mean(Concentration, na.rm = T), 
          SEc = sd(Concentration, na.rm = T)/sqrt(n())) %>%
  mutate(Season = factor(Season, levels = c("Winter", "Spring", "Summer", "Fall")))

ggplot(SoNutsmean, aes(x=Year, y = ConcentrationM, fill = Season)) + geom_col()+
  geom_errorbar(aes(ymin = ConcentrationM - SEc, ymax = ConcentrationM + SEc ))+
  facet_grid(Analyte~Season, scales = "free_y")+
  scale_fill_brewer(palette = "Set2", guide = NULL)+
  ylab("Concentration (mg/L)")+
  theme_bw()

ggsave(filename = "Nutrients.tiff", device = "tiff", width = 6, height = 5)
##################################3
#nutrient statistics
library(lme4)
library(lmerTest)
library(emmeans)
library(DHARMa)
nit = lmer(log(Nitrate) ~ as.factor(Year) + Season + (1|Month)+ (1|Station),  data = Sonutssf)
summary(nit)
plot(nit)
nitres = simulateResiduals(nit)
plot(nitres)
#OK, some issues
emmeans(nit, pairwise ~ "Year")
plot(emmeans(nit, pairwise ~ "Year"), comparison = TRUE)
plot(emmeans(nit, pairwise ~ "Season"))
tukNit = cld(emmeans(nit, pairwise ~ "Year"), Letters = LETTERS) %>%
  mutate(Analyte = "NitrateNitrite")

Amm = lmer(log(Ammonium) ~ as.factor(Year)+Season + (1|Month)+ (1|Station),  data = Sonutssf)
summary(Amm)
plot(simulateResiduals(Amm))

#OK, some issues
tukAM = cld(emmeans(Amm, pairwise ~ "Year"), Letters = LETTERS) %>%
  mutate(Analyte = "Ammonium")

chl= lmer(log(Chlorophyll) ~ as.factor(Year) + Season + (1|Month)+ (1|Station),  data = Sonutssf)
summary(chl)
plot(simulateResiduals(chl))

#OK, some issues
tukcl = cld(emmeans(chl, pairwise ~ "Year"), Letters = LETTERS) %>%
  mutate(Analyte = "Chlorophyll")

tuk = bind_rows(tukcl, tukAM, tukNit) %>%
  mutate(group = str_trim(.group))

#pllot for report##################
ggplot(SoNutsmean, aes(x=Year, y = ConcentrationM, fill = Season)) + geom_col()+
  geom_errorbar(aes(ymin = ConcentrationM - SEc, ymax = ConcentrationM + SEc ))+
  facet_grid(Analyte~Season, scales = "free_y")+
  scale_fill_brewer(palette = "Set2", guide = NULL)+
 # geom_text(data = tuk, aes(x = Year, y = 0, label = group), inherit.aes = FALSE)+
  ylab("Concentration (mg/L)")+
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

##########################################################################################

#Now for the degree day calculations

load("data/WQ.RData")

summary(WQ)
unique(WQ$Analyte)

Temps = filter(WQ, Analyte == "Temp")

ggplot(Temps, aes(x = DateTime, y = Amount)) + geom_point()

load("data/WQ.daily.RData")

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
