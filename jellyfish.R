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
  CPUE = Total/TowVolumeOLD) %>%
  rename(Volume = TowVolumeOLD)

BS2000 = pivot_wider(BS00_08, id_cols = c(Year, Survey,  Station, Net, Tow,
                        Volume),
            names_from = OrganismCode, values_from = CPUE, values_fill = 0) %>%
  pivot_longer(cols = 7:last_col(), names_to = "OrganismCode", values_to = "CPUE") %>%
  filter(OrganismCode != "NoJellies") %>%
  mutate(Station = as.character(Station), Net = as.character(Net))
  
BSjelliesX = bind_rows(BSjellies, BS2000)  %>%
  mutate(Month = month(SampleDate))


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
  mutate(Month = month(SampleDate), Source = "Bay Study")


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

SMjell = filter(SM, OrganismCode == "MAEOTIAS", MethodCode == "OTR") 
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

SMtest = filter(Suisun, Taxa == "Maeotias marginata", Method == "Otter trawl", remove_unknown_lengths=FALSE) 
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

group_by(Alljelliesx, Source) %>%
  summarize(Volume = mean(Volume, na.rm = T), CPUEmean = mean(CPUE, na.rm = T),
            CPUEmin = min(CPUE, na.rm = T), CPUEmax = max(CPUE, na.rm = T),
            Catchmax = max( Catch, na.rm = T))


######################################################
WYs <- read_csv("data/yearassignments.csv")

Alljellies2 = mutate(Alljelliesx,
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



#total jelly catch per station
AlljelliesTot = group_by(Alljellies2, Year, Yr_type, StationID, Source, Region, 
                         Survey, Season, Index, Drought, ShortTerm, Month, Volume, Sal_surf, Temp_surf) %>%
  summarize(TotJellies = sum(CPUE, na.rm = T), Totcatch = sum(Catch)) %>%
  mutate(Yr_type = factor(Yr_type, levels = c("Critical", "Dry", "Below Normal", "Above Normal", "Wet"))) 

#average jelly catch per region and month
AlljelliesMean = group_by(AlljelliesTot, Year, Yr_type, Region, Season, ShortTerm, Drought, Index,
                          Month) %>%
  summarize(meanJellies = mean(TotJellies, na.rm = T), sdJellies = sd(TotJellies), NTrawl = n(),
            Sal_mean = mean(Sal_surf, na.rm =T))

save(AlljelliesTot, AlljelliesMean, Alljellies2, file = "jellyfish.RData")
write.csv(Alljellies2,"data/alljelly_4FEB2022.csv", row.names = FALSE)
write.csv(AlljelliesTot,"data/alljelly_totalcatch_4FEB2022.csv", row.names = FALSE)
write.csv(AlljelliesMean,"data/Jelly_meanRegionMonth_4FEB2022.csv", row.names = FALSE)
##################################################################################
#graphs
#Alljellies2 = read_csv( "data/alljelly_4FEB2022.csv")
#AlljelliesMean = read_csv("data/Jelly_meanRegionMonth_4FEB2022.csv")
#AlljelliesTot = read_csv("data/alljelly_totalcatch_4FEB2022.csv")
#let's explore!

load("jellyfish.RData")
ggplot(filter(AlljelliesMean, Year == 2017), aes(x = Month, y = meanJellies)) +
  geom_col()+ facet_grid(Region~Year)

ggplot(AlljelliesMean, aes(x = Year, y = meanJellies, fill = ShortTerm)) +
  geom_col()+ facet_wrap(~Region)



ggplot(AlljelliesMean, aes(x = Year, y = meanJellies, fill = Yr_type)) +
  geom_col()+ facet_wrap(~Region)

pal_yrtype <- c( "Critical" = "#FDE333", "Dry" = "#53CC67", "Below Normal" = "#009B95","Above Normal" = "#00588B", "Wet" = "#4B0055") 
ggplot(AlljelliesMean, aes(x = Year, y = meanJellies)) +
  geom_col(aes(fill = Yr_type), position = "dodge")+
  scale_fill_manual(values = pal_yrtype)+
 # geom_errorbar(aes(ymin = meanJellies-sdJellies, ymax = meanJellies + sdJellies))+
  ylab("Mean monthly jellyfish CPUE") + theme_bw()+
  facet_wrap(~Region)

ggplot(AlljelliesTot, aes(x = Temp_surf, y = TotJellies, 
                          color = Yr_type)) +
  geom_point()+ facet_wrap(~Region)

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


Allmeansub = filter(AlljelliesMean, Month %in% c(6,7,8,9,10), Year >1999) #, Region %in% c( "Suisun Bay","Confluence","Suisun Marsh"))

ggplot(Allmeansub, aes(x = Drought, y = log(meanJellies+1), fill = Drought)) +
#  scale_fill_manual(guide = NULL, values = pal_drought)+
  geom_boxplot()+ facet_wrap(~Region) + theme_bw()+
  scale_x_discrete(labels = c("Drought", "Neutral", "Wet"))+
  ylab("Mean monthly jellyfish CPUE (log-transformed)")

ggplot(Allmeansub, aes(x = Yr_type, y = log(meanJellies+1), fill = Yr_type)) +
    scale_fill_manual(guide = NULL, values = pal_yrtype)+
  geom_boxplot()+ facet_wrap(~Region) + theme_bw()+
 # scale_x_discrete(labels = c("Drought", "Neutral", "Wet"))+
  ylab("Mean monthly jellyfish CPUE (log-transformed)")


ggplot(Allmeansub, aes(x =Index, y = log(meanJellies+1))) +
  geom_smooth(method = "lm")+
  geom_point()+ facet_wrap(~Region) + theme_bw()+
  # scale_x_discrete(labels = c("Drought", "Neutral", "Wet"))+
  ylab("Mean monthly jellyfish CPUE (log-transformed)")


Allmeansub2 = filter(AlljelliesMean, Month %in% c(6,7,8,9,10), Region %in% c( "Suisun Bay","Confluence","Suisun Marsh"))

###############################################################################
#What kind of analysis can we do on this?

hist(AlljelliesMean$meanJellies)
hist(Allmeansub2$meanJellies)
hist(log(Allmeansub2$meanJellies+1))
#slightly zero-inflated

#hmmm... or I could do it right and use count and the offset instead of the mean, then it's sure to be zero-inflated
AlljelliesMean2 = mutate(AlljelliesMean, rJellies = round(meanJellies), Yearf = as.factor(Year)) %>%
  filter( Month %in% c(6,7,8,9,10), Region %in% c( "Suisun Bay","Confluence","Suisun Marsh"))


#OK, this is teh best model so far, but it's still not great.
jelz1 = glmmTMB(rJellies~ Region*Yr_type + (1|Month) + (1|Year), zi = ~Region + (1|Month), 
               family = "nbinom2", data = AlljelliesMean2)

summary(jelz1)

ecf1 = emmeans(jelz1, specs=pairwise ~Yr_type*Region,adjust="sidak")
plot(ecf1)
resz1 = simulateResiduals(jelz1)
plot(resz1)
#I'm just so confused
jelz1 = glmmTMB(rJellies~ Region*ShortTerm + (1|Month) + (1|Year), zi = ~Region, 
                family = "nbinom2", data = AlljelliesMean2)

summary(jelz1)

ecf1 = emmeans(jelz1, specs=pairwise ~ShortTerm*Region,adjust="sidak")
plot(ecf1)

z1res = simulateResiduals(jelz1)
plot(z1res)

library(MASS)

jelnb = glmmTMB(rJellies~ Region + Drought + (1|Month) + (1|Year), 
                family = "nbinom2", data = AlljelliesMean2)
summary(jelnb)
ecf1 = emmeans(jelnb, specs=pairwise ~Drought,adjust="sidak")
plot(ecf1)


#UGH. 


hist(Alltotsub$TotJellies)
hist(log(Alltotsub$TotJellies+1))

jelz2 = glmmTMB(Totcatch ~ Region + Yr_type, offset = Volume, zi=~ Region, family = "nbinom2", data = Alltotsub)
summary(jelz2)
ecf1 = emmeans(jelz2, specs=pairwise ~Region,adjust="sidak")
plot(ecf1)
#ZThis is weird

#############BEST MODEL!!!!!!!!!!!!!!
jelz3 = glmmTMB(rJellies~ Yr_type*Region +(1|Yearf) + Month,  family = "nbinom2", 
               data = AlljelliesMean2)
summary(jelz3)
ecf1 = emmeans(jelz3, specs=pairwise ~Yr_type*Region,adjust="sidak")
plot(ecf1)
z3res = simulateResiduals(jelz3)
plot(z3res)
#Gross


##############################################Salinity plots
ggplot(AlljelliesMean2, aes(x = Sal_mean, y = rJellies)) + geom_point(aes(color = Yr_type))+
  facet_wrap(~Region)


ggplot(filter(AlljelliesTot, Month %in% c(6,7,8,9,10), Year > 1999), aes(x = Sal_surf, y = log(TotJellies+1))) + 
  geom_point(aes(color = Yr_type))+
 facet_wrap(~Region, scales = "free") + 
  geom_smooth()+
  ylab("Ln Jellyfish per m3")+
  xlab("Salinity (PSU)")

ggplot(filter(Alljellies2, Month %in% c(6,7,8,9,10), Year > 1999), aes(x = Sal_surf, y = log(CPUE+1))) + 
  geom_point(aes(color = Region))+
facet_wrap(~OrganismCode) 


jelz4 = glmmTMB(rJellies~ Index*Region ,  family = "nbinom2", 
                data = AlljelliesMean2)
summary(jelz4)
library(visreg)
visreg(jelz4, "Index", by = "Region", scale = "response")

#check out species by year
Alljellies2 = mutate(Alljellies2, OrganismCode = 
                      case_when(OrganismCode == "Maeotius" ~ "Maeotias", 
                                TRUE ~ OrganismCode))
ggplot(filter(Alljellies2, CPUE != 0), aes(x = Year, y = CPUE, color = Source)) + geom_point()+
  facet_wrap(~OrganismCode)
