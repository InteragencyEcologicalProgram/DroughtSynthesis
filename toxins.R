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
  ))

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
  summarize(result = sum(resultNum, na.rm = T))


ggplot(SpattWaterX, aes(x = date_time, y = Result)) + geom_point()+
  facet_grid(class~Site, scales = "free_y")

#######################################
#Spatts

Spatt2 = Spatt %>%
  # filter(toxin %in% c("Total Anatoxin-a" , "Total BMAA",                
  #                    "Total Saxitoxin", "Total Nodularin-R" , "Total Cylindrospermopsin","Total Anabaenopeptins")) %>%
  group_by(toxin)%>%
  summarize(Res = sum(resultNum, na.rm = T))

NoTox = filter(Spatt2, Res ==0)
Tox = filter(Spatt, detect_tox == "Yes")
SpattX = filter(Spatt, !toxin %in% NoTox$toxin, !is.na(resultNum), Date > as.Date("2021-01-01")) %>%
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
         Date > as.Date("2021-01-01")) %>%
  mutate(Analyte = case_when(
    Analyte == "MC"~ "Microcystins",
    Analyte == "ANTX-A"~"Anatoxins",
    TRUE ~ Analyte 
  ))

#water board samples from Franks Tract
Frk = data.frame(Station = c("FRK", "FRK", "FRK"), Analyte = c("Microcystins", "Microcystins", "Anatoxins"),
                 Date = c(as.Date("2021-07-02"), as.Date("2021-08-06"),as.Date("2021-08-06")),
                 result = c(0, 0.63, 0))

allTox2 = bind_rows(allTox, Frk) %>%
  mutate(Station = case_when(Station == "Banks PP" ~ "BPP",
                             Station == "Clifton Court Forebay" ~ "CCF",
                             TRUE ~ Station))

ggplot(allTox2, aes(x = Date, y = result)) + geom_point()+
  facet_grid(Analyte~Station, scales = "free_y") +
  theme_bw()

health = data.frame(Analyte = c("Microcystins", "Microcystins", "Anatoxins","Anatoxins"), Advisory = c(0.3, 8, .5, 20),
                    AdviseType = c("Caution", "Warning \nTeir I", "Caution", "Warning \nTeir I"))

ggplot(allTox2, aes(x = month(Date), y = result)) + geom_point()+
    geom_hline(data = health, aes(yintercept = Advisory, color = AdviseType))+
  scale_color_manual(values = c("orange", "red"), name = "Recreational \nAdvisory")+
  facet_grid(Analyte~Station, scales = "free_y") +
  xlab("Month of 2021")+ ylab("Concentration ug/L")+
  scale_x_continuous(breaks = c(2,5,8,11))+
  theme_bw()

ggsave(filename = "Toxins.tiff", device = "tiff", width = 8, height = 5, units = "in")

methods = group_by(SpattWater, toxin, method, class) %>%
  summarize(n= n())
