#Cclculate and graph potential chlorophyll and N:P ratios
#Origional code by Mine Berg mbearg@esassoc.com
#Updated by Rosemary Hartman


library(plyr)
library(tidyverse)
library(lubridate)
library(ggplot2)
library(gridExtra)
library(sf)
library(ggmap)

#Hi Rosemary, below is the code for NP ratio calculations and potential Chl a. 
#These calculations were used to add the columns to the DWR_NutChl.csv spreadsheet that I emailed you previously. 
#Just FYI, to go from mg to ug, values are multiplied by 1000, and to go from ug to umol, values are divided by 14 for N and by 31 for P. 
#Contracting into one step, to go from mg/L NH4 to umol/L NH4 multiply by 71.43 and to go from mg/L orthophosphate to umol/L orthophosphate, 
#multiply by 32.26. Best, Mine
hab_nutr_chla_mvi <- read_csv("data/hab_nutr_chla_mvi.csv")

#Use the dataset that Dave made for us, change some names
nc<- hab_nutr_chla_mvi %>%
  rename(Ammonium_mgL = DissAmmonia,
         Nitrate_mgL = DissNitrateNitrite,
         Orthophosphate_mgL = DissOrthophos,
         Chla_ugL = Chlorophyll)

#Calculate molar nitrogen and phosphorus  
nc = mutate(nc, NH4_umolL= Ammonium_mgL*71.43,
            NO3_umolL= Nitrate_mgL*71.43,
            PO4_umolL= Orthophosphate_mgL*32.26,
            DIN_umolL= NH4_umolL + NO3_umolL,
            #Nitrogen to phosphorus ratio
            NPratio = DIN_umolL / PO4_umolL,
            #Potential chlorophyll (Based on nitrogen)
            PotChla_ugL= (DIN_umolL + Chla_ugL),
            Year = year(Date),
            month = month(Date))

#To calculate residual chlorophyll, residual nitrogen concentration was converted to chlorophyll 
#using the ratio 1 micromole N: 1 microgram chlorophyll-a (Cloern and Jassby 2012; Gowen et al. 1992). 
#Residual nitrogen was calculated by summing all the dissolved inorganic nitrogen species (nitrate + nitrite + ammonium) in units of molar mass N. 
#Potential chlorophyll-a was compared with measured chlorophyll-a for each region of the Delta for the summers of 2014–2020, and for summer 2021.

#Pivot longer for graphing
lo = pivot_longer(nc, cols = c(Chla_ugL, DIN_umolL), names_to = "Analyte", values_to = "mgL")

#mean value per month and region
lomean = group_by(lo, Region, Year, month, Analyte) %>%
  summarize(mgLm = mean(mgL, na.rm = T))

#Just the summer months
cs<-subset(lomean, month>3 & month<10)
cs$Analyte<-factor(cs$Analyte, levels=c("DIN_umolL","Chla_ugL"))
cs$Region<-factor(cs$Region, levels=c("Cache/Liberty","Upper Sac","Lower Sac","East Delta","Lower SJ","Franks","OMR","South Delta"))

#Plot potentail chlorophyll and measuredchlorophyll by month and region
c<-ggplot(cs, aes(fill=Analyte, y=mgLm, x= month))+
  geom_bar(position="stack", stat="identity")+
  facet_grid(Region~Year)+
  scale_fill_brewer(palette="Set2", name="Chlorophyll", labels=c("Potential","Measured"))+
  scale_y_continuous("Chlorophyll (μg/L)")+scale_x_continuous("Month", breaks=c(4,5,6,7,8,9))
c

#Apply a theme
t<-theme(strip.text.x = element_text(size = 14, colour = "black"),strip.text.y = element_text(size = 14, colour = "black"))+
  theme(axis.title.x=element_text(size=16),axis.title.y=element_text(size=16))+
  theme(axis.text.x  = element_text(size=14), axis.text.y  = element_text(size=14))

#Update and save plot
c1<-c+theme_bw()+t
c1
#ggsave("ChlRegions.png")

#Repeat for just the southern Delta
cf<-cs[cs$Region == 'Lower SJ' | cs$Region == 'Franks' | cs$Region == 'OMR' ,]
d<-ggplot(cf, aes(fill=Analyte, y=mgLm, x=month))+
  geom_bar(position="stack", stat="identity")+
  facet_grid(Region~Year)+
  scale_fill_brewer(palette="Set2", name="Chlorophyll", labels=c("Potential","Measured"))+
  scale_y_continuous("Chlorophyll (μg/L)")+
  scale_x_continuous("Month", breaks=c(4,5,6,7,8,9))

d1<-d+theme_bw()+t
d1
#ggsave("ChlFranks.png")

###############################################################
#N:P ratio


#calculate mean, min, max and standard deviation by month and year and region
Ratiossum = group_by(nc, Year, month, Region) %>%
  summarize(NPratioM = mean(NPratio, na.rm = T), sdNP = sd(NPratio, na.rm = T), minNP = min(NPratio), maxNP = max(NPratio))

ggplot(filter(Ratiossum, month %in% c(6,7,8,9)), aes(x = month, y = NPratioM)) + geom_col()+ facet_grid(Region~Year)+
  geom_hline(yintercept = 16, color = "red")+
  geom_errorbar(aes(ymin = NPratioM-sdNP, ymax = NPratioM + sdNP))

load("Regions.RData")
#This is Figure A-3 in Appendix A. 
ggplot(Ratiossum, aes(x = month, y = NPratioM, fill = Region)) + geom_col()+ 
  facet_grid(Region~Year, scales = "free_y")+
  geom_hline(yintercept = 16, color = "red")+
  geom_errorbar(aes(ymin = minNP, ymax = maxNP))+
  scale_fill_manual(values = reg3$colors, guide = NULL)+
  scale_x_continuous(breaks = c(2,4,6,8,10))+
  xlab("Month of Year")+ ylab("N:P Ratio")+
  theme_bw()

###################################################################
#Now some basic graphs of nutrients

#quick check of hte nurient data
load("Regions.RData")
hab_nutr_chla_mvi <- read_csv("data/hab_nutr_chla_mvi.csv")

#replace values below the reporting limits with zeros
#Note: There are better ways to do this, I was tired and I don't think it really matters that much
#maybe this is somethign Amanda can help with

#First start with just the data from 2021
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



#add the regional assignments
nutssf = st_as_sf(Nuts, coords = c("Longitude", "Latitude"), crs = 4326) %>%
  st_join(reg3) %>%
  st_drop_geometry() %>%
  # filter(!is.na(Region)) %>%
  mutate(Station = case_when(Source == "USGS_CAWSC" ~str_sub(Station, start = 6),
                             TRUE ~ Station),
         Ammonium = as.numeric(DissAmmonia)) %>%
  mutate(Ammonium = case_when(DissAmmonia_Sign == "<" ~ 0,
                              TRUE ~ Ammonium))

#make a color pallet
library(RColorBrewer)
pal = c(brewer.pal(8, "Set2"), brewer.pal(12, "Set3"), brewer.pal(9, "Set1"), brewer.pal(12, "Paired"), "black", "grey")

#Plot all the nitrate data for 2021. This is figure 2-9
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

#ggsave(filename = "Nitrate.tiff", device = "tiff", width = 10, height = 12)


#Plot the ammonium data. This is figure 2-8
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

#ggsave(filename = "Ammonium.tiff", device = "tiff", width = 10, height = 12)


#Plot Chlorophyll. FIgure 2-7
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

#ggsave(filename = "Chlorophyll.tiff", device = "tiff", width = 10, height = 12)


#Plot ortho-phosphate, figure 2-10
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

#ggsave(filename = "Orthophos.tiff", device = "tiff", width = 10, height = 12)


#P8 had some really high values. Is that normal?
P8 = filter(mutate(hab_nutr_chla_mvi, Month = month(Date), 
                   Year = year(Date),Chlorophyll = as.numeric(Chlorophyll),
                   Nitrate = as.numeric(DissNitrateNitrite)), Station %in% c("P8", "D19", "OSJ", "C9"))
ggplot(P8, aes(x = Date, y = Nitrate, color = Station)) + geom_line()
ggplot(filter(P8, !is.na(DissAmmonia)), aes(x = Date, y = as.numeric(DissAmmonia), color = Station)) + geom_line()
#OK, yes, it is normal

###########################################################
#South Delta Nutrients by season and year
#from 2014-2021
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


#Filter it to just the regions we are interested in, and add seasons
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

#Calculate mean and standard error for each parameter
SoNutsmean = Sonutssf %>%
  pivot_longer(cols = c(Ammonium, Chl, Nitrate, Phosphorus), names_to= "Analyte",
               values_to = "Concentration")  %>%
  group_by(Year, Season, Analyte) %>%
  summarize(ConcentrationM = mean(Concentration, na.rm = T), 
            SEc = sd(Concentration, na.rm = T)/sqrt(n())) %>%
  mutate(Season = factor(Season, levels = c("Winter", "Spring", "Summer", "Fall")))

#Plot all the nutrient data across years THis is figure 2-25
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

#First let's do nitrate. Heres's where the reportling limit thing could mess us sup.
nit = lmer(log(Nitrate+0.04) ~ as.factor(Year) + Season + (1|Month)+ (1|Station),  data = Sonutssf)
summary(nit)
plot(nit)
nitres = simulateResiduals(nit)
plot(nitres)
#OK, some issues, but not terrible


#Now the ammonium
Amm = lmer(log(Ammonium+0.05) ~ as.factor(Year)+Season + (1|Month)+ (1|Station),  data = Sonutssf)
summary(Amm)
plot(simulateResiduals(Amm))


#Chlorophyll
chl= lmer(log(Chl+0.01) ~ as.factor(Year) + Season + (1|Month)+ (1|Station),  data = Sonutssf)
summary(chl)
plot(simulateResiduals(chl))

#orthophosphate
Orth= lmer(log(Phosphorus+0.05) ~ as.factor(Year) + Season + (1|Month)+ (1|Station),  data = Sonutssf)
summary(Orth)
plot(Orth)
plot(simulateResiduals(Orth))


#pllot for report##################
#This is figure 2-26

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

