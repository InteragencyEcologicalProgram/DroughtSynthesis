#This script organizes and graphs all the toxin data from a variety of sources
#Rosemary Hartman
#5/29/2022

library(tidyverse)
library(lubridate)
library(sf)
library(readxl)
library(DroughtData)
library(RColorBrewer)
library(deltamapr)
library(ggmap)
library(ggsn)

#set up a color palette for later
HABcol = data.frame(Color = brewer.pal(7, "Dark2"),
                    Genus = c( "Anabaenopsis", "Aphanizomenon","Cylindrospermopsis", "Dolichospermum" ,   
                               "Microcystis","Oscillatoria","Planktothrix"))

#Toxin Data from DWR, recieved from Brianne Sakata Brianne.Sakata@water.ca.gov
#We had to do a little post-processing to get them in the right format

hab_samples <- read_excel("data/HABs/DWR_DFD_Cyanotoxin_results_2021 - JG.xlsx")

#Get the species into a consistant format
#I didn't end up using this in the report, but might be useful later.
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

#Now pivot wider then longer so I can add in zeros
habs2 = pivot_wider(habs, id_cols = c(Station, Date), names_from = Species, 
                       values_from = Concentration, values_fill = 0, values_fn = sum) %>%
  dplyr::select(!`none observed`) %>%
  pivot_longer(cols = "Aphanizomenon":"cf. Chrysosporum", names_to = "Species") %>%
  filter(!is.na(value))

#quick plot
ggplot(habs2, aes(x = as.factor(Date), y = value, fill = Species)) + geom_bar(stat = "identity")

#look at data from just 2021
habs2021 = filter(habs2, year(Date)==2021, Station %in% c("Banks PP", "Clifton Court Forebay"),
                  value !=0) %>%
  mutate(Day = date(Date))

#I used this plot in the Deceber 15 draft report, but cut it from the final version
ggplot(habs2021, aes(x = as.factor(Date), y = value, fill = Species)) + geom_col() +
  facet_wrap(~Station)+ scale_fill_manual(breaks = HABcol$Genus, values = HABcol$Color) + theme_bw()+
  theme(axis.text.x = element_text(vjust = 1, hjust = 1, angle = 90))+
  ylab("units present") +xlab(NULL)
  

#Now I'll look at the toxins
#replace non-detects with zeros
hab_samples = mutate(hab_samples, Result = case_when(
  `Result (ng/mL)` %in% c("ND", "ND*") ~"0",
  TRUE ~ `Result (ng/mL)`
), Result = as.numeric(Result))

#Get rid of the species and just look at toxins at each station. 
toxin3 = group_by(hab_samples, Station, Analyte, Date) %>%
  summarize(result = mean(Result, na.rm = T)) %>%
  mutate(result = case_when(
    is.nan(result) ~ 0,
    TRUE ~ result
  ), Year = year(Date))

ggplot(toxin3, aes(x = Date, y = result, color = Analyte)) + geom_line()

##############################################################
##########################################################
#Now the data from USGS/DWR SPATT study

#This is the data from the whole-water grab samples taken with the spats
SpattWater = read_csv("data/HABs/USGS_DWR_fixed_station_WW_cyanotoxins_Rexport.csv")

#This is data from teh spats themselves
Spatt = read_csv("data/HABs/USGS_DWR_fixed_station_SPATT_cyanotoxins_Rexport.csv")

#quick exploritory plot
ggplot(SpattWater, aes(x = date_time, y = resultNum)) + geom_point()+
  facet_grid(class~Site, scales = "free_y")

#Look at total concentration of all toxins in a category
SpattWater2 = SpattWater %>%
  group_by(toxin)%>%
  summarize(Res = sum(resultNum, na.rm = T))

#Subset all stations that had no toxins
NoTox = filter(SpattWater2, Res ==0)

#subset all stations with toxins detected
Tox = filter(SpattWater, detect_tox == "Yes")

#remove stations with no toxins
SpattWaterX = filter(SpattWater, !toxin %in% NoTox$toxin, !is.na(resultNum)) %>%
  group_by(BGC_ID, Site, NWIS_site_no, Date, date_time, lab, Year, Month, DOY, class)%>%
  summarize(result = sum(resultNum, na.rm = T)) %>%
  mutate(Date = ymd(Date))


ggplot(SpattWaterX, aes(x = date_time, y = result)) + geom_point()+
  facet_grid(class~Site, scales = "free_y")

# Just total toxins with in a class per sample
SpattWaterXX = SpattWater %>%
  group_by( Site, Date, date_time, Year, Month, DOY, class)%>%
  summarize(result = sum(resultNum, na.rm = T)) %>%
  mutate(Date = ymd(Date))

write.csv(SpattWaterXX, "USGSwater.csv")

#######################################
#Spatts
#I didn't present the SPATT data because there were issues getting data
#for part of hte year, and I wasn't quite sure how to make it comparable
#to the water samples, but they might be useful for next year.

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

#Bind the spatt data to DWR's data, changing the 'analyte' to the same names as 'class'
allTox = bind_rows(SpattWaterX, filter(toxin3, Station %in% c("Banks PP", "Clifton Court Forebay"))) %>%
  filter(!Analyte %in% c("STX", "CYN", "PTOX Screen- toxin analysis not recommended"),
         Year == 2021) %>%
  mutate(Analyte = case_when(
    Analyte == "MC"~ "Microcystins",
    Analyte == "ANTX-A"~"Anatoxins",
    TRUE ~ Analyte 
  ))

#water board samples from Franks Tract
#There were only a few samples and they were in a really 
#weird format so it was easier to just copy them in this way
Frk = data.frame(Station = c("FRK", "FRK", "FRK", "MI"), Analyte = c("Microcystins", "Microcystins", "Anatoxins", "Microcystins"),
                 Date = c(as.Date("2021-07-02"), as.Date("2021-08-06"),as.Date("2021-08-06"), as.Date("2021-07-02")),
                 result = c(0, 0.63, 0, 0.6))

allTox2 = bind_rows(allTox, Frk) %>%
  mutate(Station = case_when(Station == "Banks PP" ~ "BPP",
                             Station == "Clifton Court Forebay" ~ "CCF",
                             TRUE ~ Station))

#East Bay Parks data
EastBay = read_excel("data/HABs/HAB_Monitoring.xlsx", sheet = "East Bay")

#read in the data, filter to just Big Break, keep the columns I need,
#and replace non-detects with zeros
EastBayX = dplyr::filter(EastBay, Water_Body == "Big Break Regional Shoreline") %>%
  dplyr::select(Station_Code, Site_Name, Actual_Latitude, Actual_Longitude, Sample_ID, `Microcystin (µg/L)\r\nELISA_Method`, Sample_Date) %>%
  rename(resultF = `Microcystin (µg/L)\r\nELISA_Method`, Date = Sample_Date) %>%
  mutate(result = case_when(resultF == "ND" ~ 0,
                            resultF == ">50" ~ 50,
                            TRUE ~ as.numeric(resultF)),
         Analyte = "Microcystins", Station = "BigBreak") %>%
  dplyr::filter(!is.na(result))


#Nautilus data 

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
#also get rid of saxitoxins, because no one ever found any
allTox3 = bind_rows(allTox2, preece, Naut, EastBayX) %>%
  mutate(Date = case_when(is.na(Date) ~ date_time,
                          TRUE ~ Date)) %>%
  filter(Analyte != "Saxitoxins")

#Attatch stations
Stas =  read.csv("toxinstations.csv")
allTox3a = left_join(allTox3, Stas) 

#Alltoxsf = dplyr::select(Alltoxsf, Station, Date, Year, Month, Analyte, result, Study, Region, Stratum2, Stratum3)
#save(Alltoxsf, file = "Alltoxindata.RData")
load("Regions.RData")
Alltoxsf = st_as_sf(allTox3a, coords = c("lat", "lon"), crs = 4326) 
reg3crop = st_crop(reg3, xmin = -121.9, xmax = -121.2, ymin = 37.65, ymax = 38.4)

Alltoxsf = dplyr::select(Alltoxsf, Station, Date, Year, Month, Analyte, result, Study, Region, Stratum2)
save(Alltoxsf, file = "data/HABs/Alltoxindata.RData")
load(file = "data/HABs/Alltoxindata.RData")


mypal =  c(brewer.pal(10, "Set3"), "gray", "darkolivegreen")

load("Regions.RData")

Alltoxsf = mutate(Alltoxsf, Study = case_when(
  Study == "CVRWQCB" ~ "Regional Board",
  Study == "Preece" ~ "Prop 1",
  TRUE ~ Study
))

#Map all the toxin stations
#This is plot 2-4 in the report
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
  north(data = reg3crop, symbol = 2) +
  theme_bw()+ylab("")+xlab("")

ggsave(filename = "plots/Toxinmap.tiff", device = "tiff", width = 6, height = 6)

ggsave(filename = "plots/Toxinmap.pdf", device = "pdf", width = 6, height = 6)



#To plot the toxin data, I want to put it in terms of the health
#advisory levels from OEHHA. Here is a dataframe of those levels:
health = data.frame(Analyte = c("Microcystins", "Microcystins", "Microcystins", "Anatoxins","Anatoxins"), Advisory = c(0.8, 6,20, .5, 20),
                    AdviseType = c("Caution\nTier I", "Warning \nTier II","Danger \nTier III", "Caution\nTier I", "Warning \nTier II")) %>%
  mutate(AdviseType = factor(AdviseType, levels = c("Caution\nTier I", "Warning \nTier II","Danger \nTier III")))

#The vernalis and Clifton court stations are outside the area I use for the
#rest of the analysis, so I need to give them new region names
Alltoxsf$Stratum2[which(Alltoxsf$Region == "Vernalis")] = "Vernalis"
Alltoxsf$Stratum2[which(Alltoxsf$Region == "Clifton Court")] = "CCF"

#Now plot all the toxin data except Big Break
#This is figure 2-16
ggplot(filter(Alltoxsf, Study != "EastBay", year(Date) == 2021, Analyte != "Saxitoxins"), aes(x = month(Date), y = result)) + geom_point(aes(shape = Study))+
  geom_hline(data = filter(health, AdviseType != "Danger \nTier III"), aes(yintercept = Advisory, color = AdviseType))+
  scale_color_manual(values = c("yellow", "orange", "red"), name = "Recreational \nAdvisory")+
  facet_grid(Analyte~Stratum2, scales = "free_y") +
  xlab("Month of 2021")+ ylab("Concentration ug/L")+
  scale_x_continuous(breaks = c(2,5,8,11))+
  theme_bw()+
  theme(legend.position = "bottom")

ggsave(filename = "plots/Toxins.tiff", device = "tiff", width = 11, height = 6, units = "in")

#Take out Anabaneopeptins

filter(Alltoxsf, Study != "EastBay", year(Date) == 2021, Analyte != "Saxitoxins", Analyte != "Anabaenopeptins",
       Stratum2 != "Upper Sac", Stratum2 != "Vernalis") %>%
  ggplot(aes(x = month(Date), y = result)) + geom_point(aes(shape = Study))+
  geom_hline(data = filter(health, AdviseType != "Danger \nTier III"), aes(yintercept = Advisory, color = AdviseType))+
  scale_color_manual(values = c("yellow", "orange", "red"), name = "Recreational \nAdvisory")+
  facet_grid(Analyte~Stratum2, scales = "free_y") +
  xlab("Month of 2021")+ ylab("Concentration ug/L")+
  scale_x_continuous(breaks = c(2,5,8,11))+
  theme_bw()+
  theme(legend.position = "bottom")

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

#Combine incidents from our toxin levels with the ones reported to the Board
incsf2 = bind_rows(incsf, levels) 
inc2021 = 
  filter(incsf2, Year == 2021)

#Now plot it. THis is figure 2-18 in the report
ggplot()+
  geom_sf(data = WW_Delta, color = "grey", fill = "lightblue")+ 

  geom_sf(data = reg3crop, aes(fill = Stratum2), alpha = 0.4) + 
  scale_fill_manual(values = reg3$colors, guide = NULL)+
  
  theme_bw()+ylab("")+xlab("")+

  geom_sf(data = inc2021, aes(color = Advisory), size = 3)+
  scale_color_manual(values = c("yellow",  "red"), labels = c("Caution", "Danger"), name = "Incident Reports\nAdvisory Level")+
  theme_bw()+ scalebar(dist = 10, dist_unit = "km",
           transform = TRUE, st.dist = .1, x.min = -121.6, x.max = -121.8, y.min = 37.75, y.max = 37.9) +
    north(data = inc2021, symbol = 2) +
  scale_x_continuous(limits = c(-121.9, -121.2)) +
  scale_y_continuous( limits = c(37.7, 38.4))

ggsave("plots/Incidentsmap.pdf", device = "pdf", width = 6, height = 6)


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
#The data that East Bay Regional Parks sent me was a little different
#Than what the water boards had, but it's the origional source, so it's what I'll use

Bigbreak = read_excel("data/HABs/2015-22 BB HAB Monitoring.xlsx", sheet = "Sheet1")
Bigbreak = filter(Bigbreak, Analyte == "Microcystins") %>%
  mutate(Year = year(Date), DOY = yday(Date))

#This is Figure 2-30
ggplot(Bigbreak, aes(x = DOY, y = Result)) + geom_point()+
  geom_hline(data = filter(health, Analyte == "Microcystins"), aes(yintercept = Advisory, color = AdviseType))+
  scale_color_manual(values = c("yellow", "orange", "red"), name = "Recreational \nAdvisory")+
  facet_wrap(~Year) + theme_bw() + ylab("Microcystin concentration ug/L")+
  scale_x_continuous(breaks = c(30, 91, 152, 213, 274, 335),
                     labels = c("Feb", "Apr", "Jun", "Aug", "Oct", "Dec"))
ggsave(filename = "plots/BigBreak.tiff", device = "tiff", width = 7, height = 6)

#This is figure 2-17 in the report
ggplot(filter(Bigbreak, Year == 2021), aes(x = DOY, y = Result)) + geom_point()+
  geom_hline(data = filter(health, Analyte == "Microcystins"), aes(yintercept = Advisory, color = AdviseType))+
  scale_color_manual(values = c("yellow", "orange", "red"), name = "Recreational \nAdvisory")+
  facet_wrap(~Year) + theme_bw() + ylab("Microcystin concentration ug/L")+
  scale_x_continuous(breaks = c( 91, 152, 213, 274),
                     labels = c("Apr", "Jun", "Aug", "Oct"))
ggsave(filename = "plotsBigBreak2021.tiff", device = "tiff", width = 4, height = 4)

#############################3
#add health adivsory levels to toxin data 
#This is for the table in the appendix
load("data/data package/Alltoxindata.RData")

Alltox3 = mutate(Alltox3, Advisory = case_when(Analyte == "Microcystins" & result > 0.8 & result < 6 ~ "Caution",
                                                 Analyte == "Microcystins"  & result >= 6 & result < 20 ~ "Warning",
                                                 Analyte == "Microcystins"  & result >= 20 ~ "Danger",
                                                 Analyte == "Microcystins"  & result < 0.8 ~ "No Advisory",
                                               Analyte == "Anatoxins" & result > 20 & result <90 ~ "Warning",
                                               Analyte == "Anatoxins"  & result > 0 & result < 20 ~ "Caution",
                                               Analyte == "Anatoxins"  & result == 0 ~ "No Advisory"))

write.csv(Alltox3, "Alltoxindata.csv")

#Export a shapefile so that ICF can use it for the EJ chapter
Alltoxsf = st_as_sf(Alltox3, coords = c("Latitude", "Longitude"), crs = 4326)
st_write(Alltoxsf, "data/HABs/Toxindata.shp", )

############################################
