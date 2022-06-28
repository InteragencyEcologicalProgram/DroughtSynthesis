#jellyvish

#Add water year types
library(tidyverse)
library(readxl)
library(lubridate)
library(glmmTMB)
library(DHARMa)
library(multcomp)
library(lme4)
library(emmeans)
library(wql)
library(LTMRdata)

####################################################
#first I'll organize the FMWT data
#Import the query with all the FMWT catch data
FMWTjellies = read_excel("data/Qry_Jellies Step 1_7feb.xlsx")

#let's add the zeros back in and filter out the jellyfish
names(FMWTjellies)
JellyWide = pivot_wider(FMWTjellies, id_cols = c(Year, Month, SurveyNumber, StationCode, MeterStart, MeterEnd),
                        names_from = OrganismCode, values_from = Catch, values_fill = 0) %>%
  pivot_longer(cols = 7:last_col(), names_to = "OrganismCode", values_to = "Catch")
#Let's go grab common names

OrgCodes = read_excel("data/OrganismsLookUp.xlsx") %>%
  dplyr::select(OrganismCode, CommonName)
JellyFMWT = left_join(JellyWide, OrgCodes) %>%
  mutate(Volume = (MeterEnd-MeterStart)*0.2875,
         CPUE = Catch/Volume*10000)

##################################################
#Now the Bay Study Data.
#I exported teh same query from each Bay Study database
#and saved them as excel files with similar names.
#I'll want to process each excel file in a similar wa
#before I join them together, so I wrote a function

BS = function(Year){
  #definethe name of the excel file
  path = paste("data/baystudy/BSjelly", Year, ".xlsx", sep = "" )
  #read the file
  dat = read_excel(path)
  #Jellies are plus counted, fish are usually zeros, so this just 
  #simplifies things
  group_by(dat, Year, Survey, SampleDate, Station, Net, Tow, MeterIn, MeterOut, OrganismCode) %>%
  summarize(Count = sum(PlusCount)) %>%
    #adding the zeros back in
  pivot_wider(id_cols = c(Year, Survey, SampleDate, Station, Net, Tow, MeterIn, MeterOut),
                              names_from = OrganismCode, values_from = Count, values_fill = 0) %>%
  pivot_longer(cols = 9:last_col(), names_to = "OrganismCode", values_to = "Catch") %>%
 
    #just grab the jellies
     filter(OrganismCode %in% c("AAEORA", "AAURIT", "AEQSPP", "ALABIA", "BVIRGI", "CCOLOR",
                             "CFUSCE", "CHRSPP", "JELSPP", "MMARGI", 
                             "PBACHE", "PPENIC", "SPACIF"),
         #Net 1 is midwater trawl
         Net == 1)%>%
    #calculate CPUE
    mutate(Volume = (MeterOut-MeterIn)*0.2875,
           CPUE = Catch/Volume*10000) }
  
#now repeate for each file.
BS2009 = BS(Year = "2009")
BS2010 = BS(Year = "2010")
BS2011_1 = BS(Year = "2011_1")
BS2011_2 = BS(Year = "2011_2")
BS2012 = BS(Year = "2012")
BS2013 = BS(Year = "2013")
BS2014 = BS(Year = "2014")
BS2015 = BS(Year = "2015")
BS2016 = BS(Year = "2016")
BS2017 = BS(Year = "2017")
BS2018 = BS(Year = "2018")
BS2019 = BS(Year = "2019")
BS2020 = BS(Year = "2020")

BSjellies = bind_rows(BS2009, BS2010, BS2011_1, BS2011_2, BS2012, BS2013, BS2014,
                      BS2015, BS2016, BS2017, BS2018, BS2019, BS2020)
#############################################################
#BS for 2000-2008 is different
BS2000 = read_excel("data/baystudy/BSJelly2000-2008.xlsx")
BSboat = read_excel("data/baystudy/BoatTow_2000-2008.xlsx") %>%
  filter(Year >1999, Net ==1)

BS00_08 = left_join(BSboat, BS2000) %>%
  mutate(OrganismCode = case_when(
    is.na(AlphaCode) ~ "NoJellies",
    TRUE ~ AlphaCode
  ),
  Total = case_when(
    is.na(Total) ~ 0,
    TRUE ~ Total
  ),
  CPUE = Total/TowVolumeOLD*10000) %>%
  rename(Volume = TowVolumeOLD)

BS2000a = pivot_wider(BS00_08, id_cols = c(Year, Survey,  Station, Net, Tow,
                        Volume),
            names_from = OrganismCode, values_from = CPUE, values_fill = 0) %>%
  pivot_longer(cols = 7:last_col(), names_to = "OrganismCode", values_to = "CPUE") %>%
  filter(OrganismCode != "NoJellies") %>%
  mutate(Station = as.character(Station), Net = as.character(Net))
  
BSjelliesX = bind_rows(BSjellies, BS2000a)  %>%
  mutate(Month = Survey)


############################################################3
#now 20mm
#there was a different database for the jellyfish than the environmentals
#so I exported two seperate queries and I'll join them together

#Jellyfish query
X20j = read_excel("data/jellies_20mm.xlsx")

#Environmentals. This also has al the fish on it, which we don't really need,
#So I"ll summarize it and drop the fish
X20 = read_excel("data/TotalCatch20mm.xlsx")%>%
  mutate(SpeciesCode = as.character(FishCode), FishCode = NULL) %>%
  group_by(SampleDate, Survey, Station, TowNum, 
           MeterCheck, Temp, TopEC, Secchi) %>%
  summarize(N = n())

#Join fish and environmentals. Calculate CPUE
X20b = left_join(X20, X20j) %>%
  mutate(Station = as.character(Station),
         Year = year(SampleDate),
         Month = month(SampleDate),
         Volume = (MeterCheck)*0.2875,
         SpeciesCode = case_when(
           is.na(SpeciesCode) ~ "NoJellies",
           TRUE ~ SpeciesCode),
         Sal_surf = ec2pss(TopEC/1000, Temp)) %>%
  #If there are more than one tow, combine catch and volume and CPUE is the average
  group_by(Station, SampleDate, Survey, SpeciesCode, Year, Month, Temp, Sal_surf, Secchi) %>%
  summarize(Volume = sum(Volume), Catch = sum(Catch, na.rm = T)) %>%
  mutate(CPUE = case_when(
           is.na(Catch) ~ 0,
           TRUE ~ Catch/Volume*10000))

#wide to long and back to add the zeros, filter out jellies
#It looks like we only have data on jellies from 2015 onward
X20wide = pivot_wider(X20b, id_cols = c(SampleDate, Survey, Sal_surf, Secchi, Temp,
                                        Station, Volume, Year, Month), 
                      names_from = "SpeciesCode", values_from = "CPUE",
                      values_fill = 0, values_fn = sum) %>%
  pivot_longer(cols = 10:last_col(),names_to = "OrganismCode", values_to = "CPUE") %>%
  dplyr::filter(OrganismCode != "NoJellies", Year >2014)

  #Now get the regions for the stations
regionsf = read_csv("data/IEPstationsw_Regions.csv") %>%
  filter(Source == "FMWT") %>%
  mutate(Station = as.character(Station))
regionsb = read_csv("data/IEPstationsw_Regions.csv") %>%
  filter(Source == "Baystudy") %>%
  mutate(Station = as.character(Station))
regions2 = read_csv("data/IEPstationsw_Regions.csv") %>%
  filter(Source == "20mm") %>%
  mutate(Station = as.character(Station))
regions3 = read_csv("data/IEPstationsw_Regions.csv") %>%
  filter(Source == "STN") %>%
  mutate(Station = as.character(Station))

#Join each data set to the regions lookup table and calculate
#number of samples and jelly density per region and year


X20b2 = left_join(regions2, X20wide)%>%
  filter(!is.na(Region))

#Calculate total Jelly CPUE (all species) for each station
X20tot = group_by(X20b2, Year, Station, Survey, Region, Month, Temp, Secchi, Sal_surf) %>%
  summarize(totJellies = sum(CPUE)) 


JellyFMWT2 = left_join(JellyFMWT, regionsf, by =  c("StationCode" = "Station"))%>%
  filter(!is.na(Region))

JellyFMWTtot = group_by(JellyFMWT2, Year, StationCode, SurveyNumber, Month, Region) %>%
  summarize(totJellies = sum(CPUE))
  
BSjellies2 = left_join(BSjelliesX, regionsb) %>%
  filter(!is.na(Region))
BSjelliestot = group_by(BSjellies2, Year, Station, Survey, Region, Month) %>%
  summarize(totJellies = sum(CPUE)) %>%
  filter(!is.na(Region))


#Calculate number of samples and average Jelly density
samplesizeF = group_by(JellyFMWTtot, Year, Region) %>%
  summarise(N = n(), Jellies = mean(totJellies), sd = sd(totJellies),
            se = sd/N)

samplesizeB = group_by(BSjelliestot, Year, Region) %>%
  summarise(N = n(), Jellies = mean(totJellies), sd = sd(totJellies),
            se = sd/N)

samplesize2 = group_by(X20tot, Year, Region) %>%
  summarise(N = n(), Jellies = mean(totJellies), sd = sd(totJellies),
            se = sd/N)


#Some exploratory plots
ggplot(samplesizeF, aes(x = Year, y = N)) + geom_col()+
  facet_wrap(~Region)

ggplot(samplesizeF, aes(x = Year, y = Jellies)) + geom_col()+
  facet_wrap(~Region)# + geom_errorbar(aes(ymin = Jellies - sd, ymax = Jellies +sd))

ggplot(samplesizeB, aes(x = Year, y = N)) + geom_col()+
  facet_wrap(~Region)

ggplot(samplesizeB, aes(x = Year, y = Jellies)) + geom_col()+
  facet_wrap(~Region)


ggplot(samplesize2, aes(x = Year, y = N)) + geom_col()+
  facet_wrap(~Region)

ggplot(samplesize2, aes(x = Year, y = Jellies)) + geom_col()+
  facet_wrap(~Region)

###################################################3
#let's see if we can't combine these three data sets
unique(X20b2$OrganismCode)
unique(BSjellies2$OrganismCode)
unique(JellyFMWT2$OrganismCode)
#GRRRRRRRRRRRRRRRRRRRRRRRRRRR

#I made a lookup table
JelLookup = read_excel("data/JellyLookup.xlsx") %>%
  rename(OrganismCode = Name) %>%
  mutate(FMWT = as.character(FMWT))

X20b3 = rename(X20b2, `20mm` = OrganismCode) %>%
  left_join(dplyr::select(JelLookup, `20mm`, OrganismCode)) %>%
  mutate(Month = month(SampleDate)) %>%
  dplyr::select(StationID, Source, Region, SampleDate, Month,
         Latitude, Longitude, Volume, Year, CPUE,
         OrganismCode, Survey, Sal_surf, Temp, Secchi)


JellyFMWT3 = rename(JellyFMWT2, `FMWT` = OrganismCode) %>%
  left_join(dplyr::select(JelLookup, `FMWT`, OrganismCode))%>%
  dplyr::select(StationID, Source, Region,
         Latitude, Longitude, Volume, Year, CPUE, Month,
         OrganismCode, SurveyNumber) %>%
  rename(Survey = SurveyNumber)

BSjellies3 = rename(BSjellies2, `BayStudy` = OrganismCode) %>%
  ungroup() %>%
  left_join(dplyr::select(JelLookup, `BayStudy`, OrganismCode))%>%
  dplyr::select(StationID, Source, Region, SampleDate, 
         Latitude, Longitude, Volume, Year, Catch, CPUE,
         OrganismCode, Survey) %>%
  mutate(Month = Survey, Source = "Bay Study")


#I shouls also add in salinity, but I do't want to go back to the origional queries

salinities = fish(sources = c("Baystudy", "FMWT"), species = "Morone saxatilis", remove_unknown_lengths = FALSE) %>%
  mutate(Year = year(Date)) %>%
  dplyr::select(Source, Station, Year, Survey, Temp_surf, Secchi, Sal_surf) 

BSjellies4 = left_join(BSjellies3, mutate(salinities, StationID = paste("Baystudy", Station))) %>%
  distinct()

JellyFMWT4 = left_join(JellyFMWT3, mutate(salinities, StationID = paste("FMWT", Station))) %>%
  distinct

#Now we are ready to combine everything!
Alljellies = bind_rows(X20b3, BSjellies4, JellyFMWT4)

##########################################################
#Suisun Marsh Data

#import query from the Suisun Marsh database
SM = read_csv("data/SuisunFish.csv")
#the only jellyfish they record is "maeotius". I have the feeling there are
#other species that get ID'd as maeotius

SMjell = filter(SM, OrganismCode == "MAEOTIAS", MethodCode == "OTR", CatchCnt != 0) 
SMjell = mutate(SMjell, SampleDate =  mdy_hms(SampleDate), OrganismCode = "Maeotias",
                Source = "SuisunMarsh", Month = month(SampleDate), Year = year(SampleDate),
                Region = "Suisun Marsh") %>%
  rename(StationID = StationCode, Count = CatchCnt)
max(dplyr::filter(SMjell, Count != 0)$Year)

#I need to figure out volume.I bet Sam did something.

#let's try the LTMR data package
#library(devtools)
#install_github("sbashevkin/LTMRdata")
library(LTMRdata)
unique(Suisun$Taxa)

SMtest = dplyr::filter(Suisun, Taxa == "Maeotias marginata", Method == "Otter trawl", remove_unknown_lengths=FALSE) 
#Bleh. He doesn't have volume or zeros either

#Oh, but wait! He made a function for that
SMtest2 = fish(sources = "Suisun", species = "Maeotias marginata", zero_fill = TRUE, remove_unknown_lengths=FALSE)
#Wow I love Sam.


SMjell2 = mutate(SMtest2, Volume = Tow_area*1.5, CPUE = Count/Volume*10000,
                 Region = "Suisun Marsh",  Month = month(Date), Year = year(Date),
                 OrganismCode = "Maeotias") %>%
  rename(StationID = Station, SampleDate = Date, Catch = Count) %>%
  dplyr::select(StationID, Source, Region, SampleDate, 
                Latitude, Longitude, Volume, Year, Month, Catch, CPUE,
                OrganismCode, Sal_surf, Secchi, Temp_surf)

#what years does sam have?
min(dplyr::filter(SMjell2, Catch != 0)$Year)
max(dplyr::filter(SMjell2, Catch != 0)$Year)

SMjel3 = SMjell2 %>%
  dplyr::select(StationID, Source, Region, SampleDate, 
                Latitude, Longitude, Volume, Year, Month, Catch, CPUE,
                OrganismCode, Sal_surf, Temp_surf, Secchi)

ggplot(left_join(SMjel3, WYs), aes(x = Year, y = CPUE, color = Yr_type)) + geom_point()

Alljelliesx = bind_rows(Alljellies, SMjel3)

ggplot(filter(Alljelliesx, Source %in% c("Bay Study", "Suisun")),
       aes(x = Volume)) + geom_histogram(bins = 30) + facet_wrap(~Source)

ggplot(Alljelliesx,
       aes(x = Volume)) + geom_histogram(bins = 30) + facet_wrap(~Source)


group_by(Alljelliesx, Source) %>%
  summarize(Volume = mean(Volume, na.rm = T), CPUEmean = mean(CPUE, na.rm = T),
            CPUEmin = min(CPUE, na.rm = T), CPUEmax = max(CPUE, na.rm = T),
            Catchmax = max( Catch, na.rm = T))

###################################################
#STN data
STN = read_csv("data/TNS.csv")
names(STN)
unique(STN$CommonName)
STN = filter(STN, Year>2005) %>%
  mutate(CPUE = Catch/TowVolm3*10000)

STN0 = pivot_wider(STN, id_cols = c(StationCode, SampleDate, TowNumber, TemperatureTop, Secchi, ConductivityTop, TurbidityTop, TowVolm3),
                   names_from = CommonName, values_from = CPUE, values_fill = 0) %>%
  pivot_longer(cols = `Age-0 Striped Bass`:last_col(), values_to = "CPUE", names_to = "CommonName")

STNx = left_join(STN0, OrgCodes) %>%
  filter(OrganismCode %in% JelLookup$FMWT, !is.na(OrganismCode)) %>%
  mutate(StationCode = as.character(StationCode), Source = "STN", SampleDate = mdy_hms(SampleDate), 
         StationID = paste("STN", StationCode), Month = month(SampleDate),
         Sal_surf = ec2pss(ConductivityTop/1000, t = 25), Year = year(SampleDate)) %>%
  rename(Volume = TowVolm3, Temp_surf = TemperatureTop, FMWT = OrganismCode) %>%
  left_join(dplyr::select(regions3, Station, Region), by = c("StationCode" = "Station")) %>%
  filter(!is.na(Region))

STNx2 = STNx %>%
  left_join(JelLookup)

ggplot(STNx, aes(x = SampleDate, y = CPUE)) + geom_point()

Alljelliesxy = bind_rows(Alljelliesx, STNx2)

ggplot(Alljelliesxy, aes(x = Year, y = CPUE)) + geom_point()+ facet_grid(Region~Source, scales = "free_y")

######################################################
WYs <- read_csv("data/yearassignments.csv")

Alljellies2 = mutate(Alljelliesxy,
                       Season = case_when(
                         Month %in% c(12,1,2)~"Winter",
                         Month %in% c(3,4,5) ~ "Spring",
                         Month %in% c(6,7,8) ~ "Summer",
                         Month %in% c(9,10,11) ~ "Fall"
                       ),
                       Year = case_when(
                         Month == 12 ~ Year +1,
                         TRUE ~ Year
                       )) %>%
  left_join(WYs)

####################################################

#Now filter so it's just maeotias, just 2007-present

Alljellies2b = filter(Alljellies2, OrganismCode == "Maeotias", Source != "20mm", Year>2006)

#total jelly catch per station
AlljelliesTot = group_by(Alljellies2b, Year, Yr_type, StationID, Source, Region, 
                         Season, Index, Drought, ShortTerm, Month, Volume, Sal_surf, Temp_surf) %>%
  summarize(TotJellies = sum(CPUE, na.rm = T)) %>%
  mutate(Yr_type = factor(Yr_type, levels = c("Critical", "Dry", "Below Normal", "Above Normal", "Wet"), ordered = T)) 

#average jelly catch per region and month
AlljelliesMean = group_by(AlljelliesTot, Year, Index, Yr_type, Region, Season, ShortTerm, Drought,
                          Month) %>%
  summarize(meanJellies = mean(TotJellies, na.rm = T), sdJellies = sd(TotJellies), NTrawl = n(),
            Sal_mean = mean(Sal_surf, na.rm =T))

save(AlljelliesTot, AlljelliesMean, Alljellies2, Alljellies2b, file = "jellyfish.RData")
write.csv(Alljellies2,"data/alljelly_4FEB2022.csv", row.names = FALSE)
write.csv(AlljelliesTot,"data/alljelly_totalcatch_4FEB2022.csv", row.names = FALSE)
write.csv(AlljelliesMean,"data/Jelly_meanRegionMonth_4FEB2022.csv", row.names = FALSE)
##################################################################################
#graphs
#Alljellies2 = read_csv( "data/alljelly_4FEB2022.csv")
#AlljelliesMean = read_csv("data/Jelly_meanRegionMonth_4FEB2022.csv")
#AlljelliesTot = read_csv("data/alljelly_totalcatch_4FEB2022.csv")
#let's explore!
