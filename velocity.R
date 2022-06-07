# LEt's look at the relationship between Delta Outflow and current speed

library(cder)
library(tidyverse)
library(lubridate)


flowdata = cdec_query(c("SVR", "LIB", "SJC", "RYF", "C31", "SJJ"), c(20, 21), "E", ymd("2010-01-01"), ymd("2021-12-31"))
Outflow = cdec_query(c("DTO"), c(23), "D", ymd("2010-01-01"), ymd("2021-12-31")) %>%
  mutate(Date = date(DateTime), Year = year(Date), Month = month(Date), StationID = NULL) %>%
  rename(Outflow = Value)

Flowmean = flowdata %>%
  filter(!is.na(Value), Value < 100000) %>%
  mutate(Date = date(DateTime)) %>%
  group_by(Date, StationID, SensorType) %>%
  summarise(Mean = mean(Value, an.rm = T), Max = max(Value, na.rm = T), Min = min(Value, na.rm = T))

Flow2 = pivot_wider(Flowmean, id_cols = c(Date, StationID), names_from = SensorType, values_from = Mean)
Flow3 = pivot_wider(Flowmean, id_cols = c(Date, StationID), names_from = SensorType, values_from = Max, names_prefix = "Max")
Flow4 = pivot_wider(Flowmean, id_cols = c(Date, StationID), names_from = SensorType, values_from = Min, names_prefix = "Min")
Flow = left_join(Flow2, Flow3) %>%
  left_join(Flow4) %>%
  left_join(Outflow)

Flow = mutate(Flow, MaxVel = max(abs(MinVLOCITY), abs(MaxVLOCITY)), Year = year(Date), Month = month(Date)) %>%
  filter(MaxVel < 5, FLOW > -10000, !(StationID == "RYF" & FLOW > 20000),
         !(StationID == "SJC" & MaxVel > 2)) %>%
  mutate(Season = case_when(Month %in% c(12,1,2) ~ "Winter",
                            Month %in% c(3,4,5) ~ "Spring",
                            Month %in% c(6,7,8) ~ "Summer",
                            Month %in% c(9,10,11) ~ "Fall")) %>%
  filter(Outflow < 200000)

ggplot(Flow, aes(x= FLOW, y = MaxVel))+ geom_point()+ geom_smooth() + facet_wrap(~StationID, scales = "free")            
ggplot(Flow, aes(x= Outflow, y = MaxVel))+ geom_point()+ geom_smooth() + facet_wrap(~StationID, scales = "free")            

ggplot(filter(Flow, StationID != "SJJ"), aes(x= Outflow, y = MaxVel))+ geom_point(aes(color = as.factor(Year)))+ geom_smooth() + 
  facet_grid(StationID~Season, scales = "free") +
  ylab("Daily Maximum current speed")+ xlab("Daily Mean Delta OutFlow") 

ggplot(filter(Flow, StationID != "SJJ"), aes(x= log(Outflow), y = MaxVel))+ geom_point(aes(color = as.factor(Year)))+ geom_smooth() + 
  facet_grid(StationID~Season, scales = "free") +
  ylab("Daily Maximum current speed")+ xlab("Log Daily Mean Delta OutFlow") 


summerFlow = filter(Flow, Month %in% c(6,7,8), StationID != "SJJ", FLOW < 10000, Outflow < 100000)
ggplot(summerFlow, aes(x= FLOW, y = MaxVel))+ geom_point(aes(color = as.factor(Year)))+ geom_smooth() + facet_wrap(~StationID, scales = "free") +
  ylab("Daily Maximum current speed")+ xlab("Daily Mean Flow")
ggplot(summerFlow, aes(x= Outflow, y = MaxVel))+ geom_point(aes(color = as.factor(Year)))+ geom_smooth() + facet_wrap(~StationID, scales = "free") 

ggplot(summerFlow, aes(x= Outflow, y = MaxVel))+ geom_point(aes(color = as.factor(Year)))+ geom_smooth() + facet_wrap(~StationID, scales = "free") +
  xlim(0, 10000) + ylab("Daily Maximum current speed")+ xlab("Daily Mean Delta Outflow")

LIB = filter(flowdata, StationID == "LIB", SensorNumber == 21)


ggplot(LIB, aes(x = DateTime, y = Value)) + geom_point() + geom_line()
