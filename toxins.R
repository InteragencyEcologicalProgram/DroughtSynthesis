library(tidyverse)
library(lubridate)
library(sf)
library(readxl)
library(DroughtData)

Toxins = read_csv("data/HABs/ToxinDataDWR2.csv") %>%
  mutate(Date = mdy(Date))
Toxins2 = pivot_wider(Toxins, id_cols = c(Date, Station), names_from = Analyte, 
                      values_from = Result, values_fill = 0) 

ggplot(Toxins2, aes(x = Date, y = MC, color = Station)) + geom_line(size = 1) + geom_point()+
  scale_color_brewer(palette = "Set1")+ ylab("Microcystins (ug/L)") + theme_bw()


hab_samples <- read_excel("data/HABs/DWR_DFD_Cyanotoxin_results_2021 - JG.xlsx")
unique(hab_samples$`PTOX Species`)
habs = mutate(hab_samples, Species = case_when(
  `PTOX Species` %in% c("Aphanizomenon cf. flos-aquae/klebahnii", 
                        "Aphanizomenon flos-aquae/klebahnii", "cf. Aphanizomenon") ~ "Aphanizomenon",
  `PTOX Species` %in% c("Oscillatoria cf. perornata", "Oscillatorialean filament(s)") ~ "Oscillatoria",
  `PTOX Species` %in% c("cf. Phormidium", "Phormidium/Microcoleus") ~ "Phormidium",
  `PTOX Species` == "cf. Planktothrix" ~ "Planktothrix",
  TRUE ~ `PTOX Species`
), Concentration = parse_number(`units/ml`))
unique(habs$Species)

habs2 = pivot_wider(habs, id_cols = c(Station, Date), names_from = Species, 
                       values_from = Concentration, values_fill = 0, values_fn = sum) %>%
  dplyr::select(!`none observed`) %>%
  pivot_longer(cols = "Aphanizomenon":"cf. Chrysosporum", names_to = "Species") %>%
  filter(!is.na(value))

ggplot(habs2, aes(x = as.factor(Date), y = value, fill = Species)) + geom_bar(stat = "identity")

habs2021 = filter(habs2, year(Date)==2021, Station %in% c("Banks PP", "Clifton Court Forebay"),
                  value !=0) %>%
  mutate(Day = date(Date))

ggplot(habs2021, aes(x = as.factor(Date), y = value, fill = Species)) + geom_col() +
  facet_wrap(~Station)+ scale_fill_manual(breaks = HABcol$Genus, values = HABcol$Color) + theme_bw()+
  theme(axis.text.x = element_text(vjust = 1, hjust = 1, angle = 90))+
  ylab("units present") +xlab(NULL)
  
HABcol = data.frame(Color = brewer.pal(7, "Dark2"),
                    Genus = sort(unique(EMPHAB2$Genus)))


hab_samples = mutate(hab_samples, Result = case_when(
  `Result (ng/mL)` %in% c("ND", "ND*") ~"0",
  TRUE ~ `Result (ng/mL)`
), Result = as.numeric(Result))

toxin3 = group_by(hab_samples, Station, Analyte, Date) %>%
  summarize(result = mean(Result, na.rm = T)) %>%
  mutate(result = case_when(
    is.nan(result) ~ 0,
    TRUE ~ result
  ), Year = year(Date))

ggplot(toxin3, aes(x = Date, y = result)) + geom_line()

##############################################################
#look at data from CEDEN
ceden = read_csv("data/HABs/ceden_data_20220208093111.xls.csv")
str(ceden)

cdensf = st_as_sf(ceden, coords = c("TargetLongitude","TargetLatitude"), crs = st_crs(4326))

regions = R_EDSM_Strata_1718P1%>%
  st_transform(crs = st_crs(4326))

cdensf = st_join(cdensf, regions)  
cdensfin = st_intersection(cdensf, regions)  

ggplot() + geom_sf(data = WW_Delta) + geom_sf(data = regions)+
  geom_sf(data = cdensfin)
#UGH not helpful What am I doing wrong?

#just filter by counties instead.

cedensub = filter(ceden, county %in% c("Contra Costa", "San Joaquin", "Solano", "Sacramento"))

##########################################################
#Now the data from USGS/DWR SPATT sudy

SpattWater = read_csv("data/HABs/USGS_DWR_fixed_station_WW_cyanotoxins_Rexport.csv")

Spatt = read_csv("data/HABs/USGS_DWR_fixed_station_SPATT_cyanotoxins_Rexport.csv")

ggplot(SpattWater, aes(x = date_time, y = resultNum)) + geom_point()+
  facet_grid(class~Site, scales = "free_y")

SpattWater2 = SpattWater %>%
 # filter(toxin %in% c("Total Anatoxin-a" , "Total BMAA",                
  #                    "Total Saxitoxin", "Total Nodularin-R" , "Total Cylindrospermopsin","Total Anabaenopeptins")) %>%
  group_by(toxin)%>%
  summarize(Res = sum(resultNum, na.rm = T))

NoTox = filter(SpattWater2, Res ==0)
Tox = filter(SpattWater, detect_tox == "Yes")
SpattWaterX = filter(SpattWater, !toxin %in% NoTox$toxin, !is.na(resultNum)) %>%
  group_by(BGC_ID, Site, NWIS_site_no, Date, date_time, lab, Year, Month, DOY, class)%>%
  summarize(result = sum(resultNum, na.rm = T)) %>%
  mutate(Date = ymd(Date))


ggplot(SpattWaterX, aes(x = date_time, y = result)) + geom_point()+
  facet_grid(class~Site, scales = "free_y")

SpattWaterXX = SpattWater %>%
  group_by( Site, Date, date_time, Year, Month, DOY, class)%>%
  summarize(result = sum(resultNum, na.rm = T)) %>%
  mutate(Date = ymd(Date))

write.csv(SpattWaterXX, "USGSwater.csv")

#######################################
#Spatts

Spatt2 = Spatt %>%
  # filter(toxin %in% c("Total Anatoxin-a" , "Total BMAA",                
  #                    "Total Saxitoxin", "Total Nodularin-R" , "Total Cylindrospermopsin","Total Anabaenopeptins")) %>%
  group_by(toxin)%>%
  summarize(Res = sum(resultNum, na.rm = T))

NoTox = filter(Spatt2, Res ==0)
Tox = filter(Spatt, detect_tox == "Yes")
SpattX = filter(Spatt, !toxin %in% NoTox$toxin, !is.na(resultNum), Year== 2021) %>%
  group_by(BGC_ID, Site, NWIS_site_no, Date, date_time, lab, Year, Month, DOY, class)%>%
  summarize(result = sum(resultNum, na.rm = T))


ggplot(SpattX, aes(x = Date, y = result)) + geom_point()+
  facet_grid(class~Site, scales = "free_y")
summary(SpattX$Date)


#######################################################
#Can I put all these datasets together?
unique(toxin3$Analyte)
unique(SpattWater$toxin)
unique(SpattWater$class)

SpattWaterX = mutate(SpattWaterX, Analyte = class) %>%
  rename(Station = Site)

allTox = bind_rows(SpattWaterX, filter(toxin3, Station %in% c("Banks PP", "Clifton Court Forebay"))) %>%
  filter(!Analyte %in% c("STX", "CYN", "PTOX Screen- toxin analysis not recommended"),
         Year == 2021) %>%
  mutate(Analyte = case_when(
    Analyte == "MC"~ "Microcystins",
    Analyte == "ANTX-A"~"Anatoxins",
    TRUE ~ Analyte 
  ))

#water board samples from Franks Tract
Frk = data.frame(Station = c("FRK", "FRK", "FRK", "MI"), Analyte = c("Microcystins", "Microcystins", "Anatoxins", "Microcystins"),
                 Date = c(as.Date("2021-07-02"), as.Date("2021-08-06"),as.Date("2021-08-06"), as.Date("2021-07-02")),
                 result = c(0, 0.63, 0, 0.6))

allTox2 = bind_rows(allTox, Frk) %>%
  mutate(Station = case_when(Station == "Banks PP" ~ "BPP",
                             Station == "Clifton Court Forebay" ~ "CCF",
                             TRUE ~ Station))

#East Bay Parks data
EastBay = read_excel("data/HABs/HAB_Monitoring.xlsx", sheet = "East Bay")

EastBayX = dplyr::filter(EastBay, Water_Body == "Big Break Regional Shoreline") %>%
  dplyr::select(Station_Code, Site_Name, Actual_Latitude, Actual_Longitude, Sample_ID, `Microcystin (µg/L)\r\nELISA_Method`, Sample_Date) %>%
  rename(resultF = `Microcystin (µg/L)\r\nELISA_Method`, Date = Sample_Date) %>%
  mutate(result = case_when(resultF == "ND" ~ 0,
                            resultF == ">50" ~ 50,
                            TRUE ~ as.numeric(resultF)),
         Analyte = "Microcystins", Station = "BigBreak") %>%
  dplyr::filter(!is.na(result))


#Nautilus data (whoever that is)

Nautilus = read_excel("data/HABs/HAB_Monitoring.xlsx", sheet = "Nautalis")

Naut = Nautilus %>%
  dplyr::select(Station_Code, Site_Name, Actual_Latitude, Actual_Longitude, Sample_ID, `Anatonxin-a_ELISA_Method (ug/L)`,
                `Cylindrospermopsin_ELISA_Method (ug/L)`, `Microcystin_ELISA_Method (ug/L)`, `Saxitoxin_ELISA_Method (ug/L)`,
                Sample_Date) %>%
  rename(Station = Station_Code, Microcystins = `Microcystin_ELISA_Method (ug/L)`,
         Anatoxins = `Anatonxin-a_ELISA_Method (ug/L)`,
         Cylindrospermopsins = `Cylindrospermopsin_ELISA_Method (ug/L)`,
         Saxitoxins = `Saxitoxin_ELISA_Method (ug/L)`,
         Date = Sample_Date) %>%
  pivot_longer(cols = c(Microcystins, Anatoxins, Cylindrospermopsins, Saxitoxins), names_to= "Analyte", values_to = "resultF")%>%
  mutate(result = case_when(resultF == "ND" ~ 0,
                            resultF == ">50" ~ 50,
                            TRUE ~ as.numeric(resultF))) %>%
  dplyr::filter(!is.na(result))



#Preece/Otten data
library(readxl)
preece = read_excel("data/HABs/Prop 1 data_4.1.22.xlsx", sheet = "preecedata") %>%
  mutate(Analyte = "Microcystins", result = case_when(`Final_conc (ug/L)`=="ND"~ 0,
                                                      TRUE ~ as.numeric(`Final_conc (ug/L)`)),
         Year = year(Collection_Date), Month = as.character(month(Collection_Date))) %>%
  filter(Year == 2021) %>%
  rename(Station = Sample_ID, Date = Collection_Date)


#bind them all together!!
allTox3 = bind_rows(allTox2, preece, Naut, EastBayX) %>%
  mutate(Date = case_when(is.na(Date) ~ date_time,
                          TRUE ~ Date)) %>%
  filter(Analyte != "Saxitoxins")

#Attatch stations
Stas =  read_excel("data/HABs/Prop 1 data_4.1.22.xlsx", sheet = "stations")
allTox3a = left_join(allTox3, Stas) %>%
  mutate(Longitude = as.numeric(Longitude))
bleh = filter(allTox3a, is.na(Longitude))

load("data/data package/Alltoxindata.RData")


#Alltoxsf = dplyr::select(Alltoxsf, Station, Date, Year, Month, Analyte, result, Study, Region, Stratum2, Stratum3)
#save(Alltoxsf, file = "Alltoxindata.RData")

Stastest = st_as_sf(Stas, coords = c("Latitude", "Longitude"), crs = 4326) %>%
  st_join(reg3) %>%
  extract(geometry, c('lat', 'lon'), '\\((.*), (.*)\\)') 
write.csv(Stastest, "toxinstations.csv")

Alltoxx = left_join(Alltox3, Stastest)

Alltoxsf = st_as_sf(Alltoxx, coords = c("Latitude", "Longitude"), crs = 4326) 
reg3crop = st_crop(reg3, xmin = -121.9, xmax = -121.2, ymin = 37.65, ymax = 38.4)

library(deltamapr)

library(RColorBrewer)
mypal =  c(brewer.pal(10, "Set3"), "gray", "darkolivegreen")

load("Regions.RData")

Alltoxsf = mutate(Alltoxsf, Study = case_when(
  Study == "CVRWQCB" ~ "Regional Board",
  Study == "Preece" ~ "Prop 1",
  TRUE ~ Study
))

ggplot()+
  geom_sf(data =reg3crop, aes(fill = Stratum2), alpha = 0.5)+
  scale_fill_manual(values = reg3crop$colors, name = NULL, guide = NULL)+
  geom_sf(data = WW_Delta, color = "grey", fill = "lightblue")+ 
  geom_sf(data = Alltoxsf, aes(color = Study))+
  scale_color_brewer(palette = "Dark2")+
  theme_bw()+
 scale_x_continuous(limits = c(-121.9, -121.2)) +
  scale_y_continuous( limits = c(37.65, 38.4))+
  #geom_sf_text(data = Alltoxsf, aes(label = Station), size = 2)
  geom_sf_text(data = reg3crop, aes(label = Stratum2))+
  annotate("text", x = -121.3, y = 37.7, label = "Vernalis")+
  annotate("text", x = -121.6, y = 37.77, label = "Clifton Court")+
  scalebar(dist = 10, dist_unit = "km",
           transform = TRUE, st.dist = .1, x.min = -121.6, x.max = -121.8, y.min = 37.7, y.max = 37.9) +
  
  #there are a number of different optinos for north arrow symbols. ?north
  north(data = reg3crop, symbol = 2) +
  theme_bw()+ylab("")+xlab("")

ggsave(filename = "plots/Toxinmap.tiff", device = "tiff", width = 6, height = 6)

ggsave(filename = "plots/Toxinmap.pdf", device = "pdf", width = 6, height = 6)


ggplot()+geom_sf(data = Delta)+ geom_sf(data = filter(Alltoxsf, Analyte == "Microcystins"),
                                        aes(color = Study, size = result)) 

#regions to send to folks
RegionsHABs = mutate(reg3, Colors = brewer.pal(10, "Set3"))

ggplot(Alltoxsf, aes(x = Date, y = result, color = Station)) + geom_point()+
  facet_grid(Analyte~Stratum3, scales = "free_y") +
  theme_bw()

Stas2 = group_by(Alltoxsf, Station, Year, Region) %>%
  summarise(n = n())


health = data.frame(Analyte = c("Microcystins", "Microcystins", "Microcystins", "Anatoxins","Anatoxins"), Advisory = c(0.8, 6,20, .5, 20),
                    AdviseType = c("Caution\nTier I", "Warning \nTier II","Danger \nTier III", "Caution\nTier I", "Warning \nTier II")) %>%
  mutate(AdviseType = factor(AdviseType, levels = c("Caution\nTier I", "Warning \nTier II","Danger \nTier III")))

ggplot(filter(Alltoxsf, Study == "EastBay", Analyte == "Microcystins"), aes(x = month(Date), y = result)) + geom_point()+
    geom_hline(data = filter(health, Analyte == "Microcystins"), aes(yintercept = Advisory, color = AdviseType))+
  scale_color_manual(values = c("yellow", "orange", "red"), name = "Recreational \nAdvisory")+
  xlab("Month of 2021")+ ylab("Microcystin Concentration ug/L")+
  scale_x_continuous(breaks = c(2,5,8,11))+
  ggtitle("Big Break Regional Shoreline")+
  theme_bw()
ggsave(filename = "BigBreak2021.tiff", device = "tiff", width = 5, height = 5, units = "in")

Alltoxsf$Stratum2[which(Alltoxsf$Region == "Vernalis")] = "Vernalis"
Alltoxsf$Stratum2[which(Alltoxsf$Region == "Clifton Court")] = "CCF"

ggplot(filter(Alltoxsf, Study != "EastBay", year(Date) == 2021, Analyte != "Saxitoxins"), aes(x = month(Date), y = result)) + geom_point(aes(shape = Study))+
  geom_hline(data = filter(health, AdviseType != "Danger \nTier III"), aes(yintercept = Advisory, color = AdviseType))+
  scale_color_manual(values = c("yellow", "orange", "red"), name = "Recreational \nAdvisory")+
  facet_grid(Analyte~Stratum2, scales = "free_y") +
  xlab("Month of 2021")+ ylab("Concentration ug/L")+
  scale_x_continuous(breaks = c(2,5,8,11))+
  theme_bw()+
  theme(legend.position = "bottom")

ggsave(filename = "Toxins.tiff", device = "tiff", width = 11, height = 6, units = "in")

methods = group_by(SpattWater, toxin, method, class) %>%
  summarize(n= n())

#to a version without Anabaenopeptins, because those are confusing
test = filter(Alltoxsf, Study != "EastBay", Analyte == "Microcystins", year(Date) == 2021)
health2 = filter(health, Analyte == "Microcystins", AdviseType != "Danger \nTeir III")
ggplot(test, aes(x = month(Date), y = result)) + geom_point(aes(shape = Study))+
  geom_hline(data = health2, aes(yintercept = Advisory, color = AdviseType))+
  scale_color_manual(values = c("yellow", "orange", "red"), name = "Recreational \nAdvisory")+
  facet_wrap(~Region, scales = "free_y") +
  xlab("Month of 2021")+ ylab("Concentration ug/L")+
  scale_x_continuous(breaks = c(2,5,8,11))+
  theme_bw()

################################################################################
#incident data

incidents = read_excel("data/HABs/Legal Delta HAB incidents 2016-2021.xlsx")

incsf = st_as_sf(incidents, coords = c("Longitude", "Latitude"), crs = 4326) %>%
  rename(Advisory = `Initial Advisory Level`) %>%
  dplyr::filter(Advisory != "No Advisory") %>%
  mutate(Advisory = factor(Advisory, levels = c("Caution",  "Warning","Danger"), labels = c("Caution",  "Warning","Danger")),
         Year = year(`Incident date`))

#add warning levels from samples we have

levels = filter(Alltoxsf, Analyte == "Microcystins") %>%
  mutate(Advisory = case_when(result > 0.8 & result < 6 ~ "Caution",
                              result >= 6 & result < 20 ~ "Warning",
                              result >= 20 ~ "Danger"),
         Advisory2 = case_when(result > 0.8 & result < 6 ~ 1,
                               result >= 6 & result < 20 ~ 2,
                               result >= 20 ~ 3)) %>%
  filter(!is.na(Advisory)) %>%
  group_by(Station) %>%
  mutate(Max = max(Advisory2), Year = 2021) %>%
  dplyr::select(Max, Station, Year) %>%
  distinct()%>%
  mutate(Advisory = factor(Max, levels = c(1,  2,3), labels = c("Caution",  "Warning","Danger")))


incsf2 = bind_rows(incsf, levels) 
inc2021 = 
  filter(incsf2, Year == 2021)

ggplot()+
  geom_sf(data = WW_Delta, color = "grey", fill = "lightblue")+ 

  geom_sf(data = reg3crop, aes(fill = Stratum2), alpha = 0.4) + 
  scale_fill_manual(values = reg3$colors, guide = NULL)+
  geom_sf_text(data = reg3crop, aes(label = Stratum2), 
                label.size = 0.05,
                label.padding = unit(0.1, "lines"),
                nudge_y = reg3$nudge, alpha = 0.8, fontface = "bold")+
  
  theme_bw()+ylab("")+xlab("")+

  geom_sf(data = inc2021, aes(color = Advisory), size = 3)+
  scale_color_manual(values = c("yellow",  "red"), labels = c("Caution", "Danger"), name = "Incident Reports\nAdvisory Level")+
  theme_bw()+ scalebar(dist = 10, dist_unit = "km",
           transform = TRUE, st.dist = .1, x.min = -121.6, x.max = -121.8, y.min = 37.75, y.max = 37.9) +
    north(data = inc2021, symbol = 2) +
  scale_x_continuous(limits = c(-121.9, -121.2)) +
  scale_y_continuous( limits = c(37.7, 38.4))

ggsave("Incidentsmap.pdf", device = "pdf", width = 6, height = 6)


ggplot()+
  geom_sf(data = WW_Delta, color = "grey", fill = "lightblue")+ 
  geom_sf(data = incsf2, aes(fill = Advisory), shape = 21, color = "black", size = 3)+
  scale_fill_manual(values = c("yellow",  "orange", "red"), labels = c("Caution", "Warning","Danger"))+
  theme_bw()+
  scale_x_continuous(limits = c(-121.9, -121.2)) +
  scale_y_continuous( limits = c(37.65, 38.4))+
  facet_wrap(~Year)  

###############################################################
#Big break

Bigbreak = read_excel("data/HABs/2015-22 BB HAB Monitoring.xlsx", sheet = "Sheet1")
Bigbreak = filter(Bigbreak, Analyte == "Microcystins") %>%
  mutate(Year = year(Date), DOY = yday(Date))

ggplot(Bigbreak, aes(x = DOY, y = Result)) + geom_point()+
  geom_hline(data = filter(health, Analyte == "Microcystins"), aes(yintercept = Advisory, color = AdviseType))+
  scale_color_manual(values = c("yellow", "orange", "red"), name = "Recreational \nAdvisory")+
  facet_wrap(~Year) + theme_bw() + ylab("Microcystin concentration ug/L")+
  scale_x_continuous(breaks = c(30, 91, 152, 213, 274, 335),
                     labels = c("Feb", "Apr", "Jun", "Aug", "Oct", "Dec"))
ggsave(filename = "BigBreak.tiff", device = "tiff", width = 7, height = 6)

ggplot(filter(Bigbreak, Year == 2021), aes(x = DOY, y = Result)) + geom_point()+
  geom_hline(data = filter(health, Analyte == "Microcystins"), aes(yintercept = Advisory, color = AdviseType))+
  scale_color_manual(values = c("yellow", "orange", "red"), name = "Recreational \nAdvisory")+
  facet_wrap(~Year) + theme_bw() + ylab("Microcystin concentration ug/L")+
  scale_x_continuous(breaks = c( 91, 152, 213, 274),
                     labels = c("Apr", "Jun", "Aug", "Oct"))
ggsave(filename = "BigBreak2021.tiff", device = "tiff", width = 4, height = 4)

#Can I plot all the 2021 toxin data in terms of warning levels?

levels2 = filter(Alltoxsf, Analyte == "Microcystins", year(Date) == 2021) %>%
  mutate(Advisory = case_when(result > 0.8 & result < 6 ~ "Caution",
                              result >= 6 & result < 20 ~ "Warning",
                              result >= 20 ~ "Danger",
                              result < 0.8 ~ "No Advisory"),
         Advisory2 = case_when(result > 0.8 & result < 6 ~ 1,
                               result >= 6 & result < 20 ~ 2,
                               result >= 20 ~ 3,
                               result < 0.8 ~ 0))%>%
  group_by(Station) %>%
  mutate(Max = max(Advisory2)) %>%
  dplyr::select(Max, Station) %>%
  distinct()%>%
  mutate(Advisory = factor(Max, levels = c(0,1,  2,3), labels = c("No Advisory",  "Caution",  "Warning","Danger")))

ggplot()+
  geom_sf(data = WW_Delta, color = "grey", fill = "lightblue")+ 
  geom_sf(data = levels2, aes(fill = Advisory), shape = 21, color = "black", size = 3)+
  scale_fill_manual(values = c("yellow", "orange",  "red"), labels = c("No Advisery","Caution", "Danger"))+
  theme_bw()+
  scale_x_continuous(limits = c(-121.9, -121.2)) +
  scale_y_continuous( limits = c(37.65, 38.4))


#############################3
#add health adivsory levels to toxin data
load("data/data package/Alltoxindata.RData")

Alltox3 = mutate(Alltox3, Advisory = case_when(Analyte == "Microcystins" & result > 0.8 & result < 6 ~ "Caution",
                                                 Analyte == "Microcystins"  & result >= 6 & result < 20 ~ "Warning",
                                                 Analyte == "Microcystins"  & result >= 20 ~ "Danger",
                                                 Analyte == "Microcystins"  & result < 0.8 ~ "No Advisory",
                                               Analyte == "Anatoxins" & result > 20 & result <90 ~ "Warning",
                                               Analyte == "Anatoxins"  & result > 0 & result < 20 ~ "Caution",
                                               Analyte == "Anatoxins"  & result == 0 ~ "No Advisory"))

write.csv(Alltox3, "Alltoxindata.csv")
Alltoxsf = st_as_sf(Alltox3, coords = c("Latitude", "Longitude"), crs = 4326)
st_write(Alltoxsf, "data/HABs/Toxindata.shp", )

############################################
#
ggplot()+
  geom_sf(data =reg3crop, aes(fill = Stratum2), alpha = 0.5)+
  scale_fill_manual(values = reg3crop$colors, name = NULL, guide = NULL)+
  geom_sf(data = WW_Delta, color = "grey", fill = "lightblue")+ 
  geom_sf(data = filter(Alltoxsf, Study == "Prop 1"))+
  scale_color_brewer(palette = "Dark2")+
  theme_bw()+
  scale_x_continuous(limits = c(-121.9, -121.2)) +
  scale_y_continuous( limits = c(37.65, 38.4))+
  #geom_sf_text(data = Alltoxsf, aes(label = Station), size = 2)
  geom_sf_text(data = filter(Alltoxsf, Study == "Prop 1"), aes(label = Station))+
  annotate("text", x = -121.3, y = 37.7, label = "Vernalis")+
  annotate("text", x = -121.6, y = 37.77, label = "Clifton Court")

alltox2021 = Alltox3 %>%
  mutate(Year = year(Date)) %>%
  filter(Year == 2021)

write.csv(alltox2021, "AllTox2021.csv", row.names = FALSE)
