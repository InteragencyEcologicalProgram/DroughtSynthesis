#jellyvish

#Add water year types
library(tidyverse)
library(readxl)
library(lubridate)

####################################################
#first I'll organize the FMWT data
#Import the query with all the FMWT catch data
FMWTjellies = read_excel("data/Qry_Jellies Step 1.xlsx")

#let's add the zeros back in and filter out the jellyfish
names(FMWTjellies)
JellyWide = pivot_wider(FMWTjellies, id_cols = c(Year, Month, SurveyNumber, StationCode, MeterStart, MeterEnd),
                        names_from = OrganismCode, values_from = Catch, values_fill = 0) %>%
  pivot_longer(cols = 7:98, names_to = "OrganismCode", values_to = "Catch") %>%
  filter(OrganismCode %in% c(107, 106, 100, 102, 101, 105, 111, 98))
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
  CPUE = Total/TowVolumeOLD) %>%
  rename(Volume = TowVolumeOLD)

BS2000 = pivot_wider(BS00_08, id_cols = c(Year, Survey,  Station, Net, Tow,
                        Volume),
            names_from = OrganismCode, values_from = CPUE, values_fill = 0) %>%
  pivot_longer(cols = 7:last_col(), names_to = "OrganismCode", values_to = "CPUE") %>%
  filter(OrganismCode != "NoJellies") %>%
  mutate(Station = as.character(Station), Net = as.character(Net))
  
BSjelliesX = bind_rows(BSjellies, BS2000)  


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
           MeterCheck) %>%
  summarize(N = n())

#Join fish and environmentals. Calculate CPUE
X20b = left_join(X20, X20j) %>%
  mutate(Station = as.character(Station),
         Year = year(SampleDate),
         Volume = (MeterCheck)*0.2875,
         SpeciesCode = case_when(
           is.na(SpeciesCode) ~ "NoJellies",
           TRUE ~ SpeciesCode)) %>%
  #If there are more than one tow, combine catch and volume and CPUE is the average
  group_by(Station, SampleDate, Survey, SpeciesCode, Year) %>%
  summarize(Volume = sum(Volume), Catch = sum(Catch, na.rm = T)) %>%
  mutate(CPUE = case_when(
           is.na(Catch) ~ 0,
           TRUE ~ Catch/Volume*10000))

#wide to long and back to add the zeros, filter out jellies
#It looks like we only have data on jellies from 2015 onward
X20wide = pivot_wider(X20b, id_cols = c(SampleDate, Survey,
                                        Station, Volume, Year), 
                      names_from = "SpeciesCode", values_from = "CPUE",
                      values_fill = 0, values_fn = sum) %>%
  pivot_longer(cols = 7:last_col(),names_to = "OrganismCode", values_to = "CPUE") %>%
  filter(OrganismCode != "NoJellies", Year >2014)

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

#Join each data set to the regions lookup table and calculate
#number of samples and jelly density per region and year


X20b2 = left_join(regions2, X20wide)%>%
  filter(!is.na(Region))

#Calculate total Jelly CPUE (all species) for each station
X20tot = group_by(X20b2, Year, Station, Survey, Region) %>%
  summarize(totJellies = sum(CPUE)) 


JellyFMWT2 = left_join(JellyFMWT, regionsf, by =  c("StationCode" = "Station"))%>%
  filter(!is.na(Region))

JellyFMWTtot = group_by(JellyFMWT2, Year, StationCode, SurveyNumber, Month, Region) %>%
  summarize(totJellies = sum(CPUE))
  
BSjellies2 = left_join(BSjelliesX, regionsb) %>%
  filter(!is.na(Region))
BSjelliestot = group_by(BSjellies2, Year, Station, Survey, Region) %>%
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
         OrganismCode, Survey)


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
  mutate(Month = month(SampleDate))

#Now we are ready to combine everything!
Alljellies = bind_rows(X20b3, BSjellies3, JellyFMWT3)
######################################################
WYs <- read_csv("data/yearassignments.csv")

Alljellies = mutate(Alljellies,
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



#total jelly catch per station
AlljelliesTot = group_by(Alljellies, Year, Yr_type, StationID, Source, Region, 
                         Survey, Season, Index, Drought, ShortTerm, Month) %>%
  summarize(TotJellies = sum(CPUE)) %>%
  mutate(Yr_type = factor(Yr_type, levels = c("Critical", "Dry", "Below Normal", "Above Normal", "Wet")))

#average jelly catch per region and month
AlljelliesMean = group_by(AlljelliesTot, Year, Yr_type, Region, Season, ShortTerm, Drought, Index,
                          Month) %>%
  summarize(meanJellies = mean(TotJellies), sdJellies = sd(TotJellies), NTrawl = n())

##################################################################################
#graphs
#Alljellies = read_csv( "AllJelly_18SEP2021.csv")
#AlljelliesMean = read_csv("MeanJelliesRegionMonth_18Sep2021.csv")
#AlljelliesTot = read_csv( "AllJelly_totalCatch_30DEC2021.csv")
#let's explore!
ggplot(filter(AlljelliesMean, Year == 2017), aes(x = Month, y = meanJellies)) +
  geom_col()+ facet_grid(Region~Year)

ggplot(AlljelliesMean, aes(x = Year, y = meanJellies, fill = ShortTerm)) +
  geom_col()+ facet_wrap(~Region)



ggplot(AlljelliesMean, aes(x = Year, y = meanJellies, fill = Yr_type)) +
  geom_col()+ facet_wrap(~Region)


ggplot(AlljelliesMean, aes(x = Year, y = meanJellies)) +
  geom_col(aes(fill = Yr_type), position = "dodge")+
  scale_fill_viridis_d()+
 # geom_errorbar(aes(ymin = meanJellies-sdJellies, ymax = meanJellies + sdJellies))+
  ylab("Mean monthly jellyfish CPUE") + theme_bw()+
  facet_wrap(~Region)


ggplot(AlljelliesTot, aes(x = Year, y = TotJellies, 
                          color = Yr_type)) +
  geom_point()+ facet_wrap(~Region)

ggplot(AlljelliesTot, aes(x = Month, y = TotJellies)) +
  geom_point()+ facet_wrap(~Region)+ 
  scale_y_log10()


ggplot(AlljelliesTot, aes(x = Drought, y = TotJellies)) +
  geom_point()+ facet_grid(Season~Region)

Alltotsub = filter(AlljelliesTot, Month %in% c(6,7,8,9,10), Region %in% c( "Suisun Bay","Confluence","Suisun Marsh"))

ggplot(Alltotsub, aes(x = Drought, y = log(TotJellies+1))) +
  geom_boxplot()+ facet_wrap(~Region)


Allmeansub = filter(AlljelliesMean, Month %in% c(6,7,8,9,10), Region %in% c( "Suisun Bay","Confluence","Suisun Marsh"))

ggplot(Allmeansub, aes(x = Drought, y = log(meanJellies+1), fill = Drought)) +
  scale_fill_viridis_d(guide = NULL, direction = -1)+
  geom_boxplot()+ facet_wrap(~Region) + theme_bw()+
  scale_x_discrete(labels = c("Drought", "Neutral", "Wet"))+
  ylab("Mean monthly jellyfish CPUE (log-transformed)")


write.csv(Alljellies, "AllJelly_18SEP2021.csv", row.names = FALSE)
write.csv(AlljelliesMean, "MeanJelliesRegionMonth_18Sep2021.csv",  row.names = FALSE)
write.csv(AlljelliesTot, "AllJelly_totalCatch_30DEC2021.csv",  row.names = FALSE)
#Alljellies = read_csv( "AllJelly_18SEP2021.csv",  row.names = FALSE)
#AlljelliesMean = read_csv("MeanJelliesRegionMonth_18Sep2021.csv")
#AlljelliesTot = read_csv( "AllJelly_totalCatch_18SEP2021.csv")

#What kind of analysis can we do on this?

hist(AlljelliesMean$meanJellies)
hist(Allmeansub$meanJellies)
hist(log(Allmeansub$meanJellies+1))
#OK, slightyl zero=inflated