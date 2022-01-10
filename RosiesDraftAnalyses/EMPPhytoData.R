#Let's take a look at Tiffany's phytoplankton data

#Load a few packags to make things easier
library(tidyverse)
library(lubridate)
library(readxl)


#Import Tiffany's files
Phy2011 = read_excel("data/HABs/2011 HAB Data.xlsx")
Phy2012 = read_excel("data/HABs/2012 HAB Data.xlsx")
Phy2013 = read_excel("data/HABs/2013 HAB Data.xlsx")
Phy2014 = read_excel("data/HABs/2014 HAB Data.xlsx")
Phy2015 = read_excel("data/HABs/2015 HAB Data.xlsx")
Phy2016 = read_excel("data/HABs/2016 HAB Data.xlsx")
Phy2017 = read_excel("data/HABs/2017 HAB Data.xlsx")
Phy2018 = read_excel("data/HABs/2018 HAB Data.xlsx")
Phy2019 = read_excel("data/HABs/2019 HAB Data.xlsx")
#Check to see that they read in correctly:
str(Phy2011)
#looks pretty good

#are the column names the same?
names(Phy2011) %in% names(Phy2012)
names(Phy2011) %in% names(Phy2013)
names(Phy2011) %in%names(Phy2014)
names(Phy2014)%in% names(Phy2016)
#hmmm... let's figure out which columns are different and what to do about it.
# It looks like the issue is just 2011-2013

names(Phy2016)[which(!names(Phy2016)%in% names(Phy2011))]
names(Phy2011)[which(!names(Phy2011)%in% names(Phy2016))]

#Ok, some of those are easy fixes

Phy2011 = rename(Phy2011, `Slide/ Chamber Area (mm²)`= `Slide/Chamber Area (mm²)`,
                 `Colony/Filament/Individual Group Code` =`Colony/Filament Group Code`,
                 `Picograms Carbon per mL (Value)`= `Total Picograms Carbon per mL (Value)`)

#now let's check 2012
names(Phy2016)[which(!names(Phy2016)%in% names(Phy2012))]
names(Phy2012)[which(!names(Phy2012)%in% names(Phy2016))]

Phy2012 = rename(Phy2012, `Slide/ Chamber Area (mm²)`= `Slide/Chamber Area (mm²)`,
                 `Colony/Filament/Individual Group Code` =`Colony/Filament Group Code`,
                 `Picograms Carbon per mL (Value)`= `Total Picograms Carbon per mL (Value)`) %>%
  mutate(Species = NULL)

#now let's check 2013
names(Phy2016)[which(!names(Phy2016)%in% names(Phy2013))]
names(Phy2013)[which(!names(Phy2013)%in% names(Phy2016))]
#2013 is good to go 



#Let's put them all together
PhytoAll = bind_rows(Phy2011, Phy2012, Phy2013, Phy2014, Phy2015, Phy2016, Phy2017, Phy2018, Phy2019)
str(PhytoAll)
#Ugh, almost got it. A few of the column names were still different, so they got filled in with NAs.
#let's fix those
PhytoAll = mutate(PhytoAll,
                  `Average Biovolume per mL (cubic microns per mL)` = case_when(
                    is.na(`Average Biovolume per mL (cubic microns per mL)`) ~ `Average Cell Biovolume per mL (cubic microns per mL)`,
                    TRUE ~`Average Biovolume per mL (cubic microns per mL)`
                  ),
                  `Picograms Carbon per mL (Formula)` = case_when(
                    is.na(`Picograms Carbon per mL (Formula)`) ~`Total Picograms Carbon per mL (Formula)`,
                    TRUE ~ `Picograms Carbon per mL (Formula)`
                  ),
                  `Total Picograms Carbon per mL (Formula)` = NULL,
                  `Average Cell Biovolume per mL (cubic microns per mL)` = NULL,
                  GALD = NULL,
                  `GALD 1` = NULL) %>%
  rename(CarbonperML=`Picograms Carbon per mL (Value)`)
str(PhytoAll)
#Much better

summary(PhytoAll)


#now let's add the other sample names

Phy2011b = read_excel("data/HABs/All 2011 Data.xlsx") 
Phy2012b = read_excel("data/HABs/All 2012 Data.xlsx")
Phy2013b = read_excel("data/HABs/All 2013 Data.xlsx") 
Phy2014b = read_excel("data/HABs/All 2014 Data.xlsx")
Phy2015b = read_excel("data/HABs/All 2015 Data.xlsx")
Phy2016b = read_excel("data/HABs/All 2016 Data.xlsx")
Phy2017b = read_excel("data/HABs/All 2017 Data.xlsx")
Phy2018b = read_excel("data/HABs/All 2018 Data.xlsx")
Phy2019b = read_excel("data/HABs/All 2019 Data.xlsx")
Phy2020b = read_excel("data/HABs/All 2020 Data.xlsx")

Phyallb = bind_rows(Phy2011b, Phy2013b, Phy2014b, Phy2015b, Phy2016b, Phy2017b, Phy2018b, Phy2019b, Phy2020b)

Phyallsamples = group_by(Phyallb, SampleDate, StationCode) %>%
  summarize(N = n())


Phyallc = left_join(Phyallsamples, PhytoAll) %>%
  mutate(Genus = case_when(
    is.na(Genus) ~ "NoHABs",
    TRUE~Genus
  ),
  CarbonperML = case_when(
    is.na(CarbonperML) ~ 0,
    TRUE ~ CarbonperML))

#now we can add in the zeros and stuff. 

Phyto0 = complete(Phyallc, SampleDate, StationCode, Taxon, 
                  fill = list(UnitAbundance = 0, `Organisms per mL (Unit Abundance per mL)`=0,
                              `Average Biovolume per mL (cubic microns per mL)`=0,
                              CarbonperML = 0))

unique(PhytoAll$Taxon)[order(unique(PhytoAll$Taxon))]
#UGHHH!!! Some of the taxa are all named different things!!!!!!!!!!!!!!!!!!!!!!!!
unique(PhytoAll$Genus)
#That's better. We're going to group by genus

PhytoAll2 = group_by(Phyallc, SampleDate, StationCode, Genus) %>%
  summarize(Abundance = sum(`Unit Abundance`), 
            OrgPerML = sum(`Organisms per mL (Unit Abundance per mL)`),
            BioVolperML = sum(`Average Biovolume per mL (cubic microns per mL)`),
            CarbonperML = sum(CarbonperML))

Phyto0 = pivot_wider(PhytoAll2, id_cols = c(SampleDate, StationCode), 
                     names_from = Genus, values_from = CarbonperML, values_fill = 0) %>%
  pivot_longer(cols = 3:13,names_to = "Genus", values_to = "CarbonperML") %>%
  filter(Genus != "NoHABs")



###############################################################################
#some of the station codes don't line up with the wq data
Phyto0 = mutate(Phyto0, StationCode = case_when(
  StationCode == "EZ2 SJR" ~ "EZ2-SJR",
  StationCode == "EZ2SJR" ~ "EZ2-SJR",
  StationCode == "EZ6 SJR" ~ "EZ6-SJR",
  StationCode == "EZ6SJR" ~ "EZ6-SJR",
  StationCode == "EZ6SAC" ~ "EZ6",
  StationCode == "EZ2SAC" ~ "EZ2",
  StationCode == "EZ2 SAC" ~ "EZ2",
  StationCode == "EZ6 SAC" ~ "EZ6",
  StationCode == "EZ6 SAC" ~ "EZ6",
  StationCode == "D16-Twitchell" ~ "D16",
  StationCode == "D16-Twitchel" ~ "D16",
  StationCode == "D16 - Twitchel" ~ "D16",
  StationCode == "D16 - Twitchell" ~ "D16",
  StationCode == "D16- Twitchel" ~ "D16",
  StationCode == "D16 Twitchell" ~ "D16",
  StationCode == "C3A- Hood" ~ "C3A",
  StationCode == "C3A-Hood" ~ "C3A",
  StationCode == "C3A Hood" ~ "C3A",
  StationCode == "NZ542" ~ "NZS42",
  StationCode == "E26" ~ "EZ6",
  StationCode == "E22" ~ "EZ2",
  TRUE ~ StationCode
),
Month = month(SampleDate), Year = year(SampleDate))


###################################################################################
#now let's add the water quality data

WQ = read_csv("data/SACSJ_delta_water_quality_1975_2020.csv")
WQ = mutate(WQ, Date = mdy(Date), Month = month(Date), Year = year(Date)) %>%
  rename(StationCode = Station, SampleDate = NULL)


#join the data together
Phyto02 = left_join(Phyto0, WQ)
test = filter(Phyto02, is.na(WTSurface))

#how many samples don't have paired water quality?
missing = group_by(test, Month, Year, StationCode) %>%
  summarize(N = n())

#let's try attaching the gps coordinates
GPS = read_csv("data/EMP_Discrete_Water_Quality_Stations_1975-2020.csv") %>%
  rename(StationCode = Station) %>%
  select(StationCode, Latitude, Longitude)
Phyto02reg = left_join(Phyto02, GPS) %>%
  mutate(Latitude = case_when(
    is.na(Latitude) ~ NorthLat,
    TRUE ~ Latitude
  ), Longitude = case_when(
    is.na(Longitude) ~ WestLong,
    TRUE ~ Longitude
  )) %>%
  rename(PhytoDate = SampleDate, WQDate = Date) %>%
  select(PhytoDate, WQDate, StationCode, Month, Year, Latitude, Longitude, Genus, CarbonperML,
         AirTemp, Chla, Pheophytin, TotAmmonia, DissAmmonia, DissNitrateNitrite, DOC,
         TOC, DON, TON, DissOrthophos, TotPhos, DissSilica, TDS, TSS, TKN, Depth, Secchi, Microcystis,
         SpCndSurface, DOSurface, WTSurface) %>%
  filter(Year < 2020)

write.csv(Phyto02reg, "HABBiomassByGenus.csv")

#calculate total biomass of harmful algal species
phytosum = group_by(Phyto02reg, PhytoDate, WQDate, StationCode, Month, Year, Latitude, Longitude, 
                    AirTemp, Chla, Pheophytin, TotAmmonia, DissAmmonia, DissNitrateNitrite, DOC,
                    TOC, DON, TON, DissOrthophos, TotPhos, DissSilica, TDS, TSS, TKN, Depth, Secchi, Microcystis,
                    SpCndSurface, DOSurface, WTSurface) %>%
  summarize(CarbonperML = sum(CarbonperML))

write.csv(phytosum, "HABBiomassTotal.csv")

Microcystis = filter(Phyto02reg, Genus == "Microcystis")

write.csv(Microcystis, "MicrocystisBiomass.csv")


####################################################################################
phytosum2 = group_by(Phyto02reg, Month, Year) %>%
  summarize(CarbonperMLm = mean(CarbonperML), sd = sd(CarbonperML))

ggplot(phytosum2, aes(x = Month, y = CarbonperMLm)) +geom_col() + facet_wrap(~Year)


