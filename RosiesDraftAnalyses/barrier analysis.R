#various ways of dealing with data from around the barrier. 


library(tidyverse)

library(MASS)
library(lme4)
library(lmerTest)
library(emmeans)
library(visreg)
library(cder)

##############################################################################333
#ZOOPLANKTON
#load data set#check out zooplankton from around the barrier in 2015
# I downloaded all the EMP, STN, and FMWT zooplantkon data from near
#the barrier from 2012-2019 from the ZoopSynth shiny app.
zoops = read_csv("RosiesDraftAnalyses/barrierzoops2015.csv", guess_max = 21000)

#get rid of unersampled taxa and the one really strange 20mm survey with zero catch
zoops = filter(zoops, Undersampled != TRUE, SampleID != "twentymm 706 2017-07-06 2")


#calculate total CPUE
zoosum = group_by(zoops, SampleID, Year, Date, SalSurf, Latitude, Longitude, Station, Chl, Secchi, Temperature,
                  Turbidity, Microcystis) %>%
  summarize(CPUE = sum(CPUE))

#Designate regions for each of the stations
#unique(zoosum$Station)
#write.csv(unique(zoosum$Station), "barrierstations.csv")
stations = read_csv("RosiesDraftAnalyses/barrierstations.csv")
stations = mutate(stations, Region2 = Region)
stations$Region2[which(stations$Region== "San Joaquin")] = "Sacramento"

zoosum2 = left_join(zoosum, stations) %>%
  filter(Include == "Yes") %>%
  mutate(Region2 = factor(Region2, levels = c("Sacramento", "Central")))

zoopsEZ = left_join(zoosum, stations) %>%
  filter(Station %in% c("NZEZ2SJR", "NZEZ6SJR", "NZEZ2", "NZEZ6"))

#Plot zooplankton and water quality info to see if there are
#any differences between 2015 and other years. 
ggplot(filter(zoosum2, Year > 2013), aes(x = Region2, y = CPUE, fill = Region2)) +geom_boxplot()+
  facet_grid(.~Year)+scale_y_log10() + 
  xlab("Region") + ylab("Zooplankton Catch per Unit Effort") + theme_bw() +
  scale_fill_manual(values = c("lightblue", "red"), guide = NULL)

ggplot(zoosum2, aes(x = Region2, y = Chl)) +geom_boxplot()+
  facet_grid(.~Year)+scale_y_log10()

ggplot(zoosum2, aes(x = Region2, y = Secchi)) +geom_boxplot()+
  facet_grid(.~Year)

ggplot(zoosum2, aes(x = Region2, y = SalSurf)) +geom_boxplot()+
  facet_grid(.~Year)

#add water year and run some models
ind = filter(indecies, location == "Sacramento Valley", WY >2011) %>%
  rename(Year = WY) %>%
  dplyr::select(Year, Index, Yr_type) %>%
  mutate(fyear = as.factor(Year))
 
zoosum2 = left_join(zoosum2, ind)

#Now try using a linear model to statistically test differences between years
zoosum3 = filter(zoosum2, Year > 2013)
z1 = lmer(log(CPUE +1)~ Region2*fyear + (1|Station), 
          data = zoosum3) 
summary(z1)
Foo = emmeans(z1, pairwise ~ Region2:fyear)
visreg(z1, xvar = "Region2", by = "fyear")
plot(z1)

z1 = lmer(log(Chl)~ Region*fyear + (1|Station), data = zoosum2) 
summary(z1)
Foo = emmeans(z1, pairwise ~ Region:fyear)
visreg(z1, xvar = "Region", by = "fyear")
plot(z1)

#look for differences with water year type. 
z2 = lmer(log(CPUE +1)~ Region*Yr_type + (1|Station), data = zoosum2) 
summary(z2)
emmeans(z2, pairwise ~ Region:Yr_type)
visreg(z2, xvar = "Region", by = "Yr_type")
plot(z2)

#just see "wet" versus "dry"
zoosum2 = mutate(zoosum2, YrTy2 = Yr_type)
zoosum2$YrTy2[which(zoosum2$Yr_type == "Below Normal")] = "Dry"
zoosum2$YrTy2[which(zoosum2$Yr_type == "Critical")] = "Dry"


z3 = lmer(log(CPUE +1)~ Region*YrTy2 + (1|Station), data = zoosum2) 
summary(z3)
emmeans(z3, pairwise ~ Region:YrTy2)
visreg(z3, xvar = "Region", by = "YrTy2")
plot(z3)

#query DO and temp data from CDEC

cdec = cdec_query(c("SRH", "MIR", "RVB", "TWI", "FRK", "ANH", "SJJ", "GGS"), 
                  c(61, 25, 146), start.date = as.Date("2014-06-01"), 
                  end.date = as.Date("2017-09-01"))

cdecH = mutate(cdec, Month =month(ObsDate), Hour = hour(DateTime), julian = yday(DateTime), Year = as.factor(year(DateTime))) %>%
  filter(Month %in% c(6,7,8)) %>%
  group_by(StationID, SensorNumber, SensorType, ObsDate, Month, Hour, Year, julian) %>%
  summarize(Value = mean(Value, na.rm = T))

ggplot(filter(cdecH, SensorType == "TEMPW C", Value > 1, Value < 40), aes(x = julian, y = Value, color = Year)) + geom_line() +
  facet_wrap(~StationID)

temp = filter(cdecH, SensorType == "TEMP W", Value > 60, Value < 90) %>%
  mutate(celsius = (Value-32)*5/9)
ggplot(temp, aes(x = julian, y = celsius, color = as.factor(Year))) + geom_line() +
  facet_wrap(~StationID) + theme_bw()+ ylab("Temperature in degrees C") + xlab("Day of Year") +
  scale_color_discrete(name = NULL)

ggplot(temp, aes(x = Year, y = celsius, fill = Year)) + geom_boxplot() +
  facet_wrap(~StationID) + theme_bw()+ ylab("Summer Temperature in degrees C") + xlab("Day of Year") +
  scale_color_discrete(name = NULL)



ggplot(filter(cdecH, SensorType == "DIS OXY", Value > 1, Value < 20), aes(x = julian, y = Value, color = as.factor(Year))) + geom_line() +
  facet_wrap(~StationID) + ylab("Dissolved Oxygen mg/L") + xlab("Day of Year") + theme_bw() + scale_color_discrete(name = NULL
                                                                                                                   )
ggplot(filter(cdecH, SensorType == "DIS OXY", Value > 1, Value < 20), aes(x = Year, y = Value, fill = Year)) + geom_boxplot() +
  facet_wrap(~StationID) + ylab("Dissolved Oxygen mg/L") + xlab("Year") + theme_bw() + scale_color_discrete(name = NULL)

###############################################################################
#SALINITY
#Compare salinity intrusion from 2015 versus 2021

cdec2015 = cdec_query(c("EMM", "MAL", "FAL", "FCT", "MOK", "ANH", "SJJ"), 
                  c(5, 100), start.date = as.Date("2015-05-01"), 
                  end.date = as.Date("2015-11-01"))

cdec2021 = cdec_query(c("EMM", "MAL", "FAL", "FCT", "MOK", "ANH", "SJJ"), 
                  c(5, 100), start.date = as.Date("2021-05-01"), 
                  end.date = as.Date("2021-06-18"))


cdec2017 = cdec_query(c("EMM", "MAL", "FAL", "FCT", "MOK", "ANH", "SJJ"), 
                      c(5, 100), start.date = as.Date("2017-05-01"), 
                      end.date = as.Date("2017-11-01"))

cdecCond = bind_rows(cdec2015, cdec2017, cdec2021) %>%
mutate(Year = as.factor(year(ObsDate)), day = yday(ObsDate))

cdecave = group_by(cdecCond, day, Year, SensorType, StationID) %>%
  summarise(Cond = mean(Value)) %>%
  filter(!(StationID == "SJJ" & Cond >5000), StationID != "MAL") %>%
  mutate(StationID = factor(StationID, levels = c("ANH", "EMM", "SJJ", "FAL", "FCT", "MOK"),
                            labels = c("Antioch", "Emmanton", "Jersy Point", "False River", "Fishermans Cut",
                                       "Mokolumne")))
  

ggplot(cdecave, aes(x = day, y = Cond, color = Year)) + geom_line() + geom_point()+
  facet_wrap(~StationID) + ylab("Specific Conductance uS/cm")

test = filter(cdecave, StationID == "SJJ", Cond > 5000)  



