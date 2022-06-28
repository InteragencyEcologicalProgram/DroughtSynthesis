#Data organization

# •	HAB satellite data
# •	Continuous water quality
# •	Visual assessments (ALL THE SURVEYS!)
# •	Discrete water quality (Nutrients and chlorophyll, USGS + EMP + NCRO)
# •	EMP community composition
# •	Flow (dayflow)
# •	Hydrodyanimc modeling of water age and temperature
# •	Fluoroprobe data (EMP + USGS)
# •	Toxin data
# o	USGS/EMP
# o	DWR SWP
# o	CV Regional Board
# o	East Bay Parks
# o	Nautilis
# o	Preece/Otten study
# •	HAB incident data
# •	SeaPro vegetation data
# •	Hyperspectral data
# •	DBW survey data
# •	DBW herbicide data
# •	UCD ground-truthing data

library(tidyverse)
library(lubridate)
library(readxl)

###########################################################################
#First let's do the toxin data
## DWR's State Water Project samples
hab_samples <- read_excel("data/datapackage/RawData/DWR_DFD_Cyanotoxin_results_2021 - JG.xlsx")

hab_samples = mutate(hab_samples, Result = case_when(
  `Result (ng/mL)` %in% c("ND", "ND*") ~"0",
  TRUE ~ `Result (ng/mL)`
), Result = as.numeric(Result))

toxin3 = group_by(hab_samples, Station, Analyte, Date) %>%
  summarize(result = mean(Result, na.rm = T)) %>%
  mutate(result = case_when(
    is.nan(result) ~ 0,
    TRUE ~ result
  ), Year = year(Date)) %>%
  filter(Station %in% c("Banks PP", "Clifton Court Forebay"),
         !Analyte %in% c("STX", "CYN", "PTOX Screen- toxin analysis not recommended")) %>%
  mutate(Station = case_when(Station == "Banks PP" ~ "BPP",
                             Station == "Clifton Court Forebay" ~ "CCF",
                             TRUE ~ Station))


#USGS's spat study, water samples only
SpattWater = read_csv("data/datapackage/RawData/USGS_DWR_fixed_station_WW_cyanotoxins_Rexport.csv")

SpattWater2 = SpattWater %>%
  group_by(toxin)%>%
  summarize(Res = sum(resultNum, na.rm = T))

NoTox = filter(SpattWater2, Res ==0)

SpattWaterX = filter(SpattWater, !toxin %in% NoTox$toxin, !is.na(resultNum)) %>%
  group_by(BGC_ID, Site, NWIS_site_no, Date, date_time, lab, Year, Month, DOY, class)%>%
  summarize(result = sum(resultNum, na.rm = T)) %>%
  mutate(Date = mdy(Date)) %>%
  rename(Analyte = class, Station = Site)





#water board samples from Franks Tract
Frk = data.frame(Station = c("FRK", "FRK", "FRK", "MI"), Analyte = c("Microcystins", "Microcystins", "Anatoxins", "Microcystins"),
                 Date = c(as.Date("2021-07-02"), as.Date("2021-08-06"),as.Date("2021-08-06"), as.Date("2021-07-02")),
                 result = c(0, 0.63, 0, 0.6))


#Nautilus data
#This data was from the State Board's database, given to me by Karen Atkinson
Nautilus = read_excel("data/datapackage/RawData/HAB_Monitoring.xlsx", sheet = "Nautalis")

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


#East Bay Parks data
#This data was from the State Board's database, given to me by Karen Atkinson
EastBay = read_excel("data/datapackage/RawData/HAB_Monitoring.xlsx", sheet = "East Bay")

EastBayX = dplyr::filter(EastBay, Water_Body == "Big Break Regional Shoreline") %>%
  dplyr::select(Station_Code, Site_Name, Actual_Latitude, Actual_Longitude, Sample_ID, `Microcystin (µg/L)\r\nELISA_Method`, Sample_Date) %>%
  rename(resultF = `Microcystin (µg/L)\r\nELISA_Method`, Date = Sample_Date) %>%
  mutate(result = case_when(resultF == "ND" ~ 0,
                            resultF == ">50" ~ 50,
                            TRUE ~ as.numeric(resultF)),
         Analyte = "Microcystins", Station = "BigBreak") %>%
  dplyr::filter(!is.na(result))


#Preece/Otten data
library(readxl)
preece = read_excel("data/datapackage/RawData/Prop 1 data_4.1.22.xlsx", sheet = "preecedata") %>%
  mutate(Analyte = "Microcystins", result = case_when(`Final_conc (ug/L)`=="ND"~ 0,
                                                      TRUE ~ as.numeric(`Final_conc (ug/L)`)),
         Year = year(Collection_Date), Month = as.character(month(Collection_Date))) %>%
  filter(Year == 2021) %>%
  rename(Station = Sample_ID, Date = Collection_Date)



#Put them all together
allTox = bind_rows(SpattWaterX, toxin3, Frk, preece, Naut, EastBayX)  %>%
  mutate(Analyte = case_when(
    Analyte == "MC"~ "Microcystins",
    Analyte == "ANTX-A"~"Anatoxins",
    TRUE ~ Analyte
  ))


#Attatch stations
Stas =  read_excel("data/datapackage/RawData/Prop 1 data_4.1.22.xlsx", sheet = "stations")
allTox3a = left_join(allTox, Stas) %>%
  mutate(Longitude = as.numeric(Longitude))  %>%
  ungroup()


Alltox3 = dplyr::select(allTox3a, Station, Date, Year, Month, Analyte, result, Study, Region, Latitude, Longitude)
save(Alltox3, file = "data/datapackage/outputs/Alltoxindata.RData")
write.csv(Alltox3, file = "data/datapackage/outputs/Alltoxindata.csv", row.names = F)

#Final Columns:
#Station - Name of station
#Date - Date of sample collection
#Month - month of sample collection
#Year - Year of sample collection
#Analyte - class of toxins analyzed. WHen origional data had more than one toxin in the class, they were combined to calculate the total.
#result - concentration of contaminant in ug/L. Non-detects have been replaced with zeros.
#Study - Source of the data. USGS - USGS/DWR spatt study. DWR - DWR's State Water Project samples. CVRWQCB - Central
#Valley Regional Water Quality Control Board. Preece - Data from Ellen Preece. Nautalis - Nauatlis data, Inc. From the STate Board's HAB database
#EastBay - East Bay regional parks (BIg Break)
#Region - Region of the Delta
#Latitude - Latitude in WGS84
#Longitude - Longitude in WGS84


############################################################################
#Visual assessment data

#Start with Sam's integrated water quality dataset. For the first draft of the report we
#updated it with 2021 data by hand, but now it's all together

library(discretewq)

HABs = wq(Sources = c("EMP", "STN", "FMWT"), Start_year = 2007, End_year = 2021) %>%
  filter(!is.na(Microcystis)) %>%
  select(Source, Station, Latitude, Longitude, Date, Microcystis, Secchi, Temperature, Conductivity,
         Chlorophyll, StationID, Month, Season, Year)

#Now add NCRO and DOP data
##############################################################################################
#Reorganize NCRO data

NCRO <- read_excel("data/datapackage/RawData/WQES HAB Observations and Field Readings.xlsx",
                   col_types = c("text", "date", "numeric",
                                 "numeric", "text", "numeric"))

NCRO <- read_excel("data-raw/Microcystis_visual_index_data/WQES HAB Observations and Field Readings.xlsx",
                   col_types = c("text", "date", "numeric",
                                 "numeric", "text", "numeric"))

#For once the data is in "long" format and we want it in "wide" format
NCRO2 = pivot_wider(NCRO, id_cols = c(StationCode, Date, `Secchi (m)`, `Microcystis`),
                    names_from = Parameter, values_from = `Field Sonde Value`, values_fn = first)

#read in GPS and attach it
stas = read_excel("data/datapackage/RawData/Station_Metadata_Coords.xlsx") %>%
  dplyr::select(`WQES Code`, `Latitude (WGS84)`, `Longitude (WGS84)`, StationID) %>%
  rename(StationCode = `WQES Code`, Latitude = `Latitude (WGS84)`, Longitude = `Longitude (WGS84)`)

stas = read_excel("data-raw/Microcystis_visual_index_data/Station_Metadata_Coords.xlsx") %>%
  dplyr::select(`WQES Code`, `Latitude (WGS84)`, `Longitude (WGS84)`, StationID) %>%
  rename(StationCode = `WQES Code`, Latitude = `Latitude (WGS84)`, Longitude = `Longitude (WGS84)`)

#join with GPS and rename columns so they are the same as the integrated data set
NCRO3 = left_join(NCRO2, stas) %>%
  mutate(Source = "NCRO") %>%
  rename(Chlorophyll = `Chlorophyll_ug/L`, Temperature = Temp_C,
         Turbidity = Turbidity_FNU, Salinity = Salinity_ppt, Conductivity = `SpCond_uS/cm`, Station = StationCode) %>%
  mutate(Secchi = `Secchi (m)`*100, Month = month(Date), Year = year(Date)) %>%
  dplyr::select(Source, Station, Date, Secchi, Microcystis, Chlorophyll, Salinity, Conductivity, Temperature,
                Turbidity, Latitude, Longitude, Year, Month)
names(NCRO3)
names(HABs)

#add to the rest of the data
HABs1 = bind_rows(HABs, NCRO3)

###########################################################################
#Now get the DOP data in there
DOP = read_excel("data/datapackage/RawData/DOP water quality 2019-2021_11-2-2021.xlsx", na = "NA")
DOP = read_excel("data-raw/Microcystis_visual_index_data/DOP water quality 2019-2021_11-2-2021.xlsx", na = "NA")

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

HABs = bind_rows(HABs1, DOP) %>%
  select(Source, Station, Date, Secchi, Microcystis, Chlorophyll, Salinity, Conductivity, Temperature,
         Turbidity, Latitude, Longitude, Year, Month)

save(HABs, file = "data/datapackage/outputs/HABs.RData")
write.csv(HABs, "data/datapackage/outputs/AllVisualAssessments.csv", row.names = FALSE)

#Final COlumn names
#Source - Name of program collecting the data
#Station - station name/number
#Date - date of data collection
#Secchi - Secchi depth in cm
#Microcystis - visual assessment on a scale of 1-5, 1 = absent, 2 = low, 3 - medium, 4 - high, 5 = very high.
# https://figshare.com/articles/figure/A_Visual_Scale_for_Microcystis_Bloom_Severity/19239882
#Chlorophyll - lab chlorophyll in ug/L (when collected)
# Salinity in PSU
#conductivity in uS/cm
#Temperature in C
#Latitude and Longitude in WGS84
#Year and month of data collection

#########################################################################################

#################################################
