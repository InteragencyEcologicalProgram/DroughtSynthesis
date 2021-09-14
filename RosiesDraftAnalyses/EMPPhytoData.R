#Let's take a look at Betsy's phytoplankton data

#Load a few packags to make things easier
library(tidyverse)
library(lubridate)
library(readxl)


#Import Tiffany's files
Phy2011 = read_excel("data/2011 HAB Data.xlsx")
Phy2012 = read_excel("data/2012 HAB Data.xlsx")
Phy2013 = read_excel("data/2013 HAB Data.xlsx")
Phy2014 = read_excel("data/2014 HAB Data.xlsx")
Phy2015 = read_excel("data/2015 HAB Data.xlsx")
Phy2016 = read_excel("data/2016 HAB Data.xlsx")
Phy2017 = read_excel("data/2017 HAB Data.xlsx")
Phy2018 = read_excel("data/2018 HAB Data.xlsx")
Phy2019 = read_excel("data/2019 HAB Data.xlsx")

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

#now we can add in the zeros and stuff. 

Phyto0 = complete(PhytoAll, SampleDate, StationCode, Taxon, 
                  fill = list(UnitAbundance = 0, `Organisms per mL (Unit Abundance per mL)`=0,
                              `Average Biovolume per mL (cubic microns per mL)`=0,
                              CarbonperML = 0))

unique(PhytoAll$Taxon)[order(unique(PhytoAll$Taxon))]
#UGHHH!!! Some of the taxa are all named different things!!!!!!!!!!!!!!!!!!!!!!!!
unique(PhytoAll$Genus)
#That's better. We're going to group by genus

PhytoAll2 = group_by(PhytoAll, SampleDate, StationCode, Genus) %>%
  summarize(Abundance = sum(`Unit Abundance`), 
            OrgPerML = sum(`Organisms per mL (Unit Abundance per mL)`),
            BioVolperML = sum(`Average Biovolume per mL (cubic microns per mL)`),
            CarbonperML = sum(CarbonperML))

Phyto0 = pivot_wider(PhytoAll2, id_cols = c(SampleDate, StationCode), 
                     names_from = Genus, values_from = CarbonperML, values_fill = 0) %>%
  pivot_longer(cols = 3:12,names_to = "Genus", values_to = "CarbonperML")

###################################################################################
#now let's add the water quality data

WQ = read_csv("data/SACSJ_delta_water_quality_1975_2020.csv")
WQ = mutate(WQ, Date = mdy(Date)) %>%
  rename(StationCode = Station, SampleDate = Date)

Phyto02 = left_join(Phyto0, WQ)
test = filter(Phyto02, is.na(WTSurface))
missing = group_by(test, SampleDate, StationCode) %>%
  summarize(N = n())

testagain = filter(WQ, StationCode == "D28A" & year(SampleDate)== 2011)
write.csv(missing, "MissingSamples.csv")
write.csv(PhytoAll2, "PhytoSamples.csv")
