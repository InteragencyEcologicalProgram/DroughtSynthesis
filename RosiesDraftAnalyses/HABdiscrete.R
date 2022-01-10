#Habs, from visual assessments

library(tidyverse)
library(discretewq)
library(sf)
library(emmeans)
library(lubridate)
library(ggmap)
library(readxl)
library(lme4)
library(lmerTest)
library(visreg)
library(deltamapr)
#I had Jeff Galef add teh 2021 , summer townet, and FMWT data to the 
#water quality data set that Sam put together. He did it in python
#and I don't have the code

HABs = read_csv("data/HABs/WQ_data_integrated4.csv")
str(HABs)
HABs = mutate(HABs,  Datex = as.Date(Date2, origin = "1899-12-30"),
              MonthYear = NULL, Datetime = NULL, Month = month(Datex), 
              Year = year(Datex), Date = Datex, Datex = NULL, Date2 = NULL)
#################################################
#lab chlorophyll from EMP
labchl = read_excel("data/HABs/June-Aug 2021 DEMP WQ Data.xlsx", sheet = "labchl") %>%
  mutate(DateTime = mdy_hm(`Sample Date`), Date = as.Date(DateTime)) %>%
  rename(Station = StationCode)

test = left_join(HABs, labchl)

HABs = mutate(test, Chlorophyll = case_when(
  is.na(Chlorophyll) ~ Chlorophyll_lab,
  TRUE ~ Chlorophyll
))

##############################################################################################
#Reorganize NCRO data

NCRO <- read_excel("data/HABs/WQES HAB Observations and Field Readings.xlsx", 
                   col_types = c("text", "date", "numeric", 
                                 "numeric", "text", "numeric"))

#For once the data is in "long" format and we want it in "wide" format
NCRO2 = pivot_wider(NCRO, id_cols = c(StationCode, Date, `Secchi (m)`, `Microcystis`), 
                    names_from = Parameter, values_from = `Field Sonde Value`, values_fn = first)

#read in GPS and attach it
stas = read_excel("data/HABs/Station_Metadata_Coords.xlsx") %>%
  dplyr::select(`WQES Code`, `Latitude (WGS84)`, `Longitude (WGS84)`, StationID) %>%
  rename(StationCode = `WQES Code`, Latitude = `Latitude (WGS84)`, Longitude = `Longitude (WGS84)`)

#join with GPS and rename columns so they are the same as the integrated data set
NCRO3 = left_join(NCRO2, stas) %>%
  mutate(Source = "NCRO") %>%
  rename(Chlorophyll = `Chlorophyll_ug/L`, Temperature = Temp_C, 
         Turbidity = Turbidity_FNU, Salinity = Salinity_ppt, Conductivity = `SpCond_uS/cm`, Station = StationCode) %>%
  mutate(Secchi = `Secchi (m)`/100, Month = month(Date), Year = year(Date)) %>%
  dplyr::select(Source, Station, Date, Secchi, Microcystis, Chlorophyll, Salinity, Conductivity, Temperature,
         Turbidity, Latitude, Longitude, Year, Month)
names(NCRO3)
names(HABs)

#add to the rest of the data
HABs1 = bind_rows(HABs, NCRO3)
##########################################################################'=
#check what's on WDL
NCR = read.csv("data/HABs/WQDataReport.csv")
NCRchl = filter(NCR, Analyte == "Chlorophyll a") %>%
  rename(Chlorophyll = Result, StationID = StationNumber) %>%
  mutate(Chlorophyll = as.numeric(Chlorophyll), Date = mdy_hm(CollectionDate),
         Chlorophyll = case_when(
           is.na(Chlorophyll) ~ 0,
           TRUE ~ Chlorophyll
         ))

NCRchl2 = left_join(NCRchl, stas)

###########################################################################
#Now get the DOP data in there
DOP = read_excel("data/HABs/DOP water quality 2019-2021_11-2-2021.xlsx", na = "NA")

#remove the "deep" samples (with no seperate HAB score) and rename columns
DOP = filter(DOP, !is.na(hab_score)) %>%
  mutate(Source = "DOP", Month = month(date), Year = year(date),
         ) %>%
  rename(Station = site_id,
         Latitude = start_latitude,
         Longitude = start_longitude,
         Date = date,
         Secchi = secchi_depth,
         Salinity = salinity,
         Conductivity = specific_conductivity,
         Microcystis = hab_score,
         Turbidity = turbidity,
         Temperature = temperature,
         Chlorophyll =  ChlA_Fluor
         )

HABs = bind_rows(HABs1, DOP)

lm = lm(ChlA_YSI ~ Chlorophyll, data = DOP)
summary(lm)
ggplot(DOP, aes(x = Chlorophyll, y = ChlA_YSI)) +
  geom_point( color = "darkgreen")+
  geom_smooth(method = "lm")+
  xlab("Chlorophyll ug/L (Sonde)")+ ylab("Chlorophyll ug/L (Fluorometer)")+
  geom_abline(slope = 1, intercept = 0, linetype = 2, color = "red")+
  annotate("text", x = 8.5, y = 7.5, label = "1:1 line", size = 6)+
  annotate("text", x = 20, y = 10, label = "Regression \n0.3X + 1.1", size = 6)


#load regions shapefile
#regions = st_read("C:/Users/rhartman/OneDrive - California Department of Water Resources/Drought/Barrier/BarrierRegions/shpExport.shp") %>%
#  st_make_valid()

regions = R_EDSM_Strata_1718P1%>%
  st_transform(crs = st_crs(4326)) %>%
  filter(Stratum %in% c("Lower Sacramento", "Lower San Joaquin", "Southern Delta"))

HABssf = filter(HABs, !is.na(Longitude), !is.na(Latitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = st_crs(4326))

ggplot()+geom_sf(data = HABssf, aes(color = Source))

foo = filter(HABssf, Source == "STN", Year == 2021)

ggplot() + geom_sf(data = WW_Delta) + geom_sf(data = HABssf)+
  geom_sf_label(data = HABssf, aes(label = Station), 
                position = "jitter", label.size = 0.05)
#crop it to the area right around the barrier
BarHABs = st_crop(HABssf, regions)

ggplot() + geom_sf(data = regions) + geom_sf(data = BarHABs)

BH =   st_join(BarHABs, regions) %>%
  st_drop_geometry() %>%
  filter(!is.na(Stratum), Month >4) %>%
  mutate(Yearf = as.factor(Year))

BHm = filter(BH, !is.na(Microcystis), Year >2013, Month %in% c(5,6,7,8,9, 10))

BH2 = filter(BH, !is.na(Microcystis), Year >2013)

ggplot(BHm, aes(x = as.factor(Year), y = Microcystis)) +geom_boxplot()+ facet_wrap(~Stratum)

ggplot(BHm, aes(x = Stratum, y = Microcystis)) +geom_boxplot()+ facet_wrap(~Year)

ggplot(BH, aes(x = Date, y = Temperature)) +geom_point()+ facet_wrap(~Year)


ggplot(BHm, aes(x = Stratum, fill = as.factor(Microcystis))) +geom_bar(position = "fill")+ facet_wrap(~Year) +
  scale_fill_manual(values = c("white", "tan2", "yellow", "red", "darkred"), 
                    labels = c("absent", "low", "medium", "high", "very high"),
                    name = "Microcystis")+ ylab("Relative Frequency")+
  scale_x_discrete(labels = c("Sacramento", "San Joaquin", "South Delta"))

#definitely more microcystis reported in 2016 and 2018 than 2015



BH2015.2014 = filter(BHm, Year %in% c(2014, 2015, 2016))
BH2018 = filter(BHm, Year == 2018)
BH2021 = filter(BHm, Year == 2021)


ggplot(BH2015.2014, aes(x = Month, fill = as.factor(Microcystis))) +geom_bar(position = "fill")+ facet_wrap(~Regions)  +
  scale_fill_manual(values = c("white", "tan2", "yellow", "red", "darkred"), 
                    labels = c("absent", "low", "medium", "high", "veryhigh"),
                    name = "Microcystis")

ggplot(BH2015.2014, aes(x = as.factor(Microcystis), y = Temperature)) + geom_boxplot()+ facet_wrap(~Year)
ggplot(BH2015.2014, aes(x = Microcystis, y = Temperature, color = Yearf)) +
  geom_point()+ geom_smooth(method = "lm") + theme_bw()


ggplot(BH2015.2014, aes(x = as.factor(Microcystis), y = Secchi)) + geom_boxplot()+ facet_wrap(~Year)
ggplot(BH2015.2014, aes(x = Microcystis, y = Secchi, color = Yearf)) +
  geom_point()+ geom_smooth(method = "lm") + theme_bw() + ylab("Secchi Depth (cm)")


ggplot(BH2018, aes(x = Month, fill = as.factor(Microcystis))) +geom_bar(position = "fill")+ facet_wrap(~Regions)  +
  scale_fill_manual(values = c("white", "tan2", "yellow", "red", "darkred"), 
                    labels = c("absent", "low", "medium", "high", "veryhigh"),
                    name = "Microcystis")


ggplot(BH2021, aes(x = Month, fill = as.factor(Microcystis))) +geom_bar(position = "fill")+ facet_wrap(~Stratum)  +
  scale_fill_manual(values = c("white", "tan2", "yellow", "red", "darkred"), 
                    labels = c("absent", "low", "medium", "high", "veryhigh"),
                    name = "Microcystis")



#OK, how do we model this? Maybe converte to presence/absence?
BHm = mutate(BHm, HABPA = case_when(
  Microcystis == 1 ~ FALSE,
  Microcystis > 1 ~ TRUE)) %>%
filter(Year>2013)


#We may also want to do absent/low/high
BHm = mutate(BHm, HABord = case_when(
  Microcystis == 1 ~ "Absent",
  Microcystis %in% c(2,3) ~ "Low",
  Microcystis %in% c(4,5) ~ "High"
  ),
  HABord = factor(HABord, levels = c("Absent", "Low", "High"), ordered = T)) %>%
  filter(Year>2013) %>%
  droplevels()




#now a glm (binomial)
bn1 = glm(HABPA ~ Month + Temperature + Yearf*Regions, data = BHm, family = "binomial")
summary(bn1)
emmeans(bn1, pairwise ~ Yearf)
plot(bn1)

library(visreg)
visreg(bn1, scale = "response")
visreg(bn1, xvar = "Regions", by = "Yearf")
#we really weren't seeing anything in 2015!



#now an orgered logistic regression
library(MASS)
ord1 = polr(HABord ~Yearf+Stratum + Temperature, data = BHm)
summary(ord1)
Anova(ord1)
pairs = emmeans(ord1, pairwise ~ Yearf)$contrasts
write.csv(pairs, "visualdata.csv")
pr <- profile(ord1)
confint(pr)
plot(pr)
pairs(pr)

(ctable <- coef(summary(ord1)))
## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

## combined table
(ctable <- cbind(ctable, "p value" = p))
(ci <- confint(ord1))
exp(cbind(OR = coef(ord1), ci))

emp = filter(HABs, Source == "EMP", !is.na(Microcystis), Year == 2015)
table(emp$Month)
#I guess EMP only started doing microcystis midway through 2015 darn

#Look at potential drivers of the bloom
library(lme4)
dat = filter(BHm, Year > 2013)
mm1 = glmer(HABPA ~ scale(Temperature) + scale(Secchi) +Month+ (1|Yearf) + (1|Station), data = dat, family = "binomial")
summary(mm1)
visreg(mm1)



#############################################################################################
#What do I use for the SFHSR?

library(deltamapr)
HABs2021 = filter(HABs, Year == 2021)
sumfall = filter(HABs, Month %in% c(6,7,8,9,10), !is.na(Microcystis))
ggplot(sumfall, aes(x = Year, fill = as.factor(Microcystis))) +geom_bar(position = "fill")+ 
  scale_fill_manual(values = c("white", "tan2", "yellow", "red", "darkred"), 
                    labels = c("absent", "low", "medium", "high", "very high"),
                    name = "Microcystis")+ ylab("Relative Frequency")

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


HABssf1 = filter(sumfall, !is.na(Longitude), !is.na(Latitude)) %>%
  st_as_sf(coords = c("Longitude", "Latitude"), crs = st_crs(4326))

ggplot() + geom_sf(data = reg3) + geom_sf(data = HABssf1)
#crop it to the area we are interested in
sfhab = st_crop(HABssf1, reg3)%>%
  st_join(reg3)# %>%
  #filter(Year == 2021) 

#load teh grab sample stations
EMPstas = read.csv("EMP_Discrete_Water_Quality_Stations_1975-2020.csv") 
EMP = st_as_sf(filter(EMPstas, !is.na(Latitude)), coords = c("Longitude", "Latitude"), crs = st_crs(4326)) %>%
  filter(Status == "Active")

#load CDEC coordinates and filter to stations of inerest
cdec = read.csv("data/CDEC_StationsEC.csv") %>%
  filter(STA %in% c("MRZ", "NSL", "MAL", "LIB", "DWS", "SRH", "SJJ", "BET", "FRK", "LPS"))
cdecsf = st_as_sf(cdec, coords = c("Longitude", "Latitude"), crs = 4326) 


##############################################################################

library(ggsn)
#plot the map
ggplot() + geom_sf(data = WW_Delta, fill = "lightgrey")+
  geom_sf(data = reg3, aes(fill = Stratum2), alpha = 0.4) + 
  geom_sf(data = sfhab, aes(shape = Source)) +
  scale_shape_discrete(name = "Visual Index Sites")+
  geom_sf(data = EMP, shape = 16, size = 4, aes(color = "EMP grab samples"))+
  geom_sf(data = cdecsf,shape = 16, size = 4, aes(color = "Temperature stations")) +
  scale_color_manual(values = c("red", "blue"), name = NULL)+
  scale_fill_brewer(palette = "Set3", guide = NULL)+
 coord_sf(xlim = c(-122.4, -121.2), ylim = c(37.6, 38.6))+
  geom_sf_label(data = reg3, aes(label = Stratum2), 
                 label.size = 0.05,
                label.padding = unit(0.1, "lines"),
                nudge_y = reg3$nudge, alpha = 0.8, fontface = "bold")+
  geom_sf_label(data = cdecsf, aes(label = STA), nudge_x = 0.05, alpha = 0.8, fill = "grey",
                label.size = 0.05,
                label.padding = unit(0.1, "lines"))+
  #You can adjust the size, units, etc of your scale bar.
  scalebar(data = EMP, dist = 20, dist_unit = "km",
           transform = TRUE, st.dist = .05) +
  
  #there are a number of different optinos for north arrow symbols. ?north
  north(data = reg3, symbol = 2) +
  theme_bw()+ylab("")+xlab("")

SFH =   sfhab %>%
  st_drop_geometry() %>%
#  filter(!is.na(Stratum), !is.na(Microcystis), 
 #        Stratum %in% c("Suisun Marsh", "Suisun Bay", "Lower Sacramento", "Cache Slough/Liberty Island", "Sac Deep Water Shipping Channel")) %>%
  mutate(Yearf = as.factor(Year),
         Month2 = factor(Month, levels = c(6,7,8,9,10),
                         labels = c("Jun", "Jul", "Aug", "Sep", "Oct")))      

ggplot(SFH, aes(x = Stratum, fill = as.factor(Microcystis))) +geom_bar(position = "fill")+ facet_wrap(~Year)+
  scale_fill_manual(values = c("white", "tan2", "yellow", "red", "darkred"), 
                    labels = c("absent", "low", "medium", "high", "very high"),
                    name = "Microcystis")+ ylab("Relative Frequency") +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))


ggplot(filter(SFH, Year == 2021, !is.na(Stratum)), aes(x = Stratum, fill = as.factor(Microcystis))) + 
  geom_bar(position = "fill")+ 
  facet_grid(.~Month2)+
  scale_fill_manual(values = c("white", "tan2", "yellow", "red", "darkred"), 
                    labels = c("absent", "low", "medium", "high", "very high"),
                    name = "Microcystis")+ ylab("Relative Frequency")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))


ggplot(SFH, aes(x = Year, fill = as.factor(Microcystis))) +geom_bar(position = "fill")+ 
  scale_fill_manual(values = c("white", "tan2", "yellow", "red", "darkred"), 
                    labels = c("absent", "low", "medium", "high", "very high"),
                    name = "Microcystis")+ ylab("Relative Frequency") 

ggplot(filter(SFH, Year == 2021), aes(x = Month, fill = as.factor(Microcystis))) + 
  geom_bar(position = "fill")+ 
  #facet_grid(.~Month2)+
  scale_fill_manual(values = c("white", "tan2", "yellow", "red", "darkred"), 
                    labels = c("absent", "low", "medium", "high", "very high"),
                    name = "Microcystis")+ ylab("Relative Frequency")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1))


###################################################################
#version for the main drought report using Keith's color scheme

HABs = mutate(HABs, Mic2 = case_when(
  Microcystis ==1 ~ "Absent",
  Microcystis %in% c(2,3) ~ "Low",
  Microcystis %in% c(4,5) ~ "High"
)) %>%
  mutate(Mic2 = factor(Mic2, levels = c("Absent", "Low", "High"))) %>%
  filter(!is.na(Microcystis))

c("Gray70", "seagreen4", "seagreen1")

ggplot(HABs, aes(x = Year, fill = Mic2)) +geom_bar(position = "fill")+ 
  scale_fill_manual(values = c("Gray70", "seagreen4", "seagreen1"), 
                    labels = c("None (1)", "Low (2-3)", "High (4-5)"),
                    name = "Microcystis rating")+ ylab("Relative Frequency") +
  theme_bw()


###################################################################################
#box plots by stratum
library(wql)

SFH2021 = filter(SFH, Year == 2021, !is.na(Stratum)) %>%
  mutate(Salinity = ec2pss(Conductivity/1000, t = Temperature))


ggplot(SFH2021, aes(x = Stratum, y = Temperature, fill = Stratum)) + 
  geom_boxplot()+ 
  scale_fill_brewer(palette = "Dark2", guide = NULL)+
  facet_grid(.~Month2)+ ylab("Water Temperature C")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1)) +
  geom_hline(yintercept = 23.9, linetype = 2, color = "red")

ggplot(SFH2021, aes(x = Stratum, y = Turbidity, fill = Stratum)) + 
  geom_boxplot()+ 
  scale_fill_brewer(palette = "Set3", guide = NULL)+
  facet_grid(.~Month2)+ ylab("Turbidity (FNU)")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1))

ggplot(SFH2021, aes(x = Stratum, y = Conductivity)) + 
  geom_boxplot()+ 
  scale_fill_brewer(palette = "Dark2", guide = NULL)+ 
  facet_grid(.~Month2)+ ylab("Conductivity uS/cm")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1))



ggplot(SFH2021, aes(x = Stratum, y = Salinity, fill = Stratum)) + 
  geom_boxplot()+ 
  scale_fill_brewer(palette = "Dark2", guide = NULL)+
  facet_grid(.~Month2)+ ylab("Salinity (PSU)")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1))

ggplot(filter(SFH2021, !is.na(Chlorophyll)), aes(x = Stratum, y = Chlorophyll, fill = Stratum)) + 
  geom_boxplot()+ 
  scale_fill_brewer(palette = "Dark2", guide = NULL)+
  facet_grid(.~Month2)+ ylab("Chlorophyll (ug/L)")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1))



####################################################################################
#Models for HAB weed report


HWR =   sfhab%>%
  st_drop_geometry() %>%
  filter(!is.na(Stratum), !is.na(Microcystis)) %>% 
  mutate(Yearf = as.factor(Year),
         Month2 = factor(Month, levels = c(6,7,8,9,10),
                         labels = c("Jun", "Jul", "Aug", "Sep", "Oct")),
         HABord = case_when(
           Microcystis == 1 ~ "absent",
           Microcystis %in% c(2,3) ~ "Low",
           Microcystis %in% c(4,5) ~ "High")) %>%
  mutate(HABord = factor(HABord, levels = c("absent", "Low", "High"), ordered = T)) %>%
  filter(Year >2013)




ggplot(filter(HWR, Year == 2021, Month != 10), aes(x = Stratum, fill = as.factor(Microcystis))) + 
  geom_bar(position = "fill")+ 
  facet_grid(.~Month)+
  scale_fill_manual(values = c("white", "tan2", "yellow", "red", "darkred"), 
                    labels = c("absent", "low", "medium", "high", "very high"),
                    name = "Microcystis")+ ylab("Relative Frequency")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = .5))



ggplot(HWR, aes(x = Year, fill = as.factor(Microcystis))) +geom_bar(position = "fill")+ 
  scale_fill_manual(values = c("white", "tan2", "yellow", "red", "darkred"), 
                    labels = c("absent", "low", "medium", "high", "very high"),
                    name = "Microcystis")+ ylab("Relative Frequency") 

ggplot(filter(HWR, Year == 2021), aes(x = Month, fill = as.factor(Microcystis))) + 
  geom_bar(position = "fill")+ 
  #facet_grid(.~Month2)+
  scale_fill_manual(values = c("white", "tan2", "yellow", "red", "darkred"), 
                    labels = c("absent", "low", "medium", "high", "very high"),
                    name = "Microcystis")+ ylab("Relative Frequency")+
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 1))



library(readxl)
SFH2 = mutate(HWR, HABPA = case_when(
  Microcystis == 1 ~ FALSE,
  Microcystis > 1 ~ TRUE)) %>%
  filter(Year >2013)

#year types
yeartypes = read_excel("data/Integrated data set.xlsx", sheet = "yearassignments")

SFHwflow = left_join(SFH2, yeartypes)
#now a glm (binomial)

bn1 = glm(HABPA ~ Month + Yearf+Stratum, data = SFH2, family = "binomial")
summary(bn1)
emmeans(bn1, pairwise ~ Yearf)
plot(bn1)

library(visreg)
library(lme4)
library(lmerTest)
visreg(bn1, scale = "response")
visreg(bn1, xvar = "Regions", by = "Yearf")


Ind1 = glmer(HABPA ~ Month + Index +Salinity + Temperature+ (1|Yearf), data = SFHwflow, family = "binomial")
summary(Ind1)
plot(bn1)
visreg(bn1)
library(visreg)

#Could I use the effect of the TUCP on water year index to figure out how big of an impact
#it might have had on weeds?

flow1= glmer(HABPA ~ Month + ShortTerm +Salinity + Temperature+ (1|Yearf), data = SFHwflow, family = "binomial")
summary(flow1)
emmeans(flow1, pairwise ~ ShortTerm)
plot(flow1)
visreg(bn1)
library(visreg)

#Keith mentioned multinomial models
#sound include a random effect for program. 
#random effects for station

#regions for drought report

Regions<-read_csv("RosiesDraftAnalyses/Rosies_regions2.csv")

## Load Delta Shapefile from Brian
Delta<-deltamapr::R_EDSM_Subregions_Mahardja_FLOAT%>%
  filter(SubRegion%in%unique(Regions$SubRegion))%>%  #Filter to regions of interest
  dplyr::select(SubRegion)

Regs = unique(Regions[,c(1,5)])
Delta = merge(Delta, Regs) %>%
  st_transform(crs = 4326)



Habs2 =   st_join(HABssf, Delta) %>%
  st_drop_geometry() %>%
  filter(!is.na(Region), !is.na(Microcystis)) %>% 
  mutate(Yearf = as.factor(Year),
         Month2 = factor(Month, levels = c(6,7,8,9,10),
                         labels = c("Jun", "Jul", "Aug", "Sep", "Oct")))    



SFH2a = mutate(Habs2, HABord = case_when(
  Microcystis == 1 ~ "absent",
  Microcystis %in% c(2,3) ~ "Low",
  Microcystis %in% c(4,5) ~ "High")) %>%
  mutate(HABord = factor(HABord, levels = c("absent", "Low", "High"), ordered = T)) %>%
  filter(Year >2013)

#now an orgered logistic regression
library(MASS)
library(car)
ord2 = polr(HABord ~Yearf, data = SFH2a, Hess = T)
summary(ord2)
Anova(ord2)
pairs = emmeans(ord1, pairwise ~ Yearf)$contrasts
write.csv(pairs, "visualdata_alldelta.csv")
pr <- profile(ord1)
confint(pr)
plot(pr)
pairs(pr)

(ctable <- coef(summary(ord1)))
## calculate and store p values
p <- pnorm(abs(ctable[, "t value"]), lower.tail = FALSE) * 2

## combined table
(ctable <- cbind(ctable, "p value" = p))
(ci <- confint(ord1))
exp(cbind(OR = coef(ord1), ci))

emp = filter(HABs, Source == "EMP", !is.na(Microcystis), Year == 2015)
table(emp$Month)

###################################################
#import temperature analyses
Temps = read_csv("C:/Users/rhartman/OneDrive - California Department of Water Resources/Drought/Barrier/DWR_TempCount.csv")

HAB3 = pivot_wider(HWR, id_cols = c(Yearf, Stratum2), 
                   names_from = HABord, values_from = Microcystis,
                   values_fn = length, values_fill = 0) %>%
  ungroup() %>%
  mutate(present = Low + High)

#connect stratums to temperature stations
cdec = read.csv("data/CDEC_StationsEC.csv")
cdecsf = st_as_sf(cdec, coords = c("Longitude", "Latitude"), crs = 4326)

stasT = filter(cdecsf, STA %in% c("BET", "DWS", "FRK", "LIB", "LPS",
                                      "MAL","MRZ", "NSL", "SJJ", "SRH"))
stasT2 = st_join(stasT, reg3) %>%
  st_drop_geometry() %>%
  rename(Station = STA) %>%
  dplyr::select(Station, Stratum2)

#join temperatures to regions
Temps = left_join(Temps, stasT2)

#Summarize by stratum
Tempssum = group_by(Temps, Stratum2, Year, Threshold) %>%
  summarize(Days = mean(Days)) %>%
  mutate(Yearf = as.factor(Year))

#Attach to HAB data.
HAB3a = left_join(HAB3, Tempssum) %>%
  mutate(PresentPerc = present/(present + absent), Total = present + absent) %>%
  filter(!is.na(Threshold)) %>%
  mutate(Threshold = factor(Threshold, labels = c("19 C", "25 C")))

ggplot(HAB3a, aes(x = Days, y = PresentPerc)) + geom_point(aes(shape = Yearf, color = Stratum2))+
  facet_wrap(~Threshold, scales = "free_x") + geom_smooth(method = "lm")+
  scale_shape_manual(values = c(16,17,15,4,5,6,7,8), name = NULL)+
  scale_color_brewer(palette = "Dark2", name = NULL)+theme_bw()+
  ylab("Percent of observations\n with Microcystis present")+
  xlab("Days above temperature threshold")

HAB3b = group_by(HAB3a, Year, Threshold) %>%
  summarize(Days = mean(Days), Percent = mean(PresentPerc), 
            total = sum(Total), present = sum(present))
ggplot(HAB3b, aes(x = Days, y = Percent)) + geom_point()+
  facet_wrap(~Threshold, scales = "free_x") + geom_smooth(method = "lm")+
  ylab("Percent of observations\n with Microcystis Present")+
  xlab("Days above temperature threshold")
#binomial regression

Thresh25 = filter(HAB3a, Threshold == "Temp25")
Thresh19 = filter(HAB3a, Threshold == "Temp19")
library(lme4)
b1 = glmer(present/Total~ Days + (1|Stratum2), data = Thresh19,
          family = "binomial")
summary(b1)
visreg(b1, scale = "response", gg = TRUE)+ xlab("Days above 19 C")+
  ylab("Probability of Microcystis Presence") + theme_bw()

plot(b1)
b2 = glmer(present/Total~ Days + (1|Stratum2), data = Thresh25,
           family = "binomial")
summary(b2)
visreg(b2, scale = "response")
