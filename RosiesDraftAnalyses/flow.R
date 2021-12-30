#various flow metrics

library(cder)
library(lubridate)
library(tidyverse)

#apparently exports are HRO and TRP sensor 70

#https://cdec.water.ca.gov/dynamicapp/staMeta?station_id=HRO

#https://cdec.water.ca.gov/dynamicapp/staMeta?station_id=TRP

#we also want DTO for Delta Outflow - sensor 23

Outflow = cdec_query("DTO", 23, "D", as.Date("2014-01-01"), as.Date("2020-09-30"))
names(Outflow)
#better source for 2021 outflow
DTO = read_csv("data/NDOI_WY2021.csv")
DTO = mutate(DTO, DateTime = mdy(Date), SensorType = "OUTFLOW", SensorNumber = 23, StationID = "DTO") %>%
  rename(Value = NDOIcfs)


Ex1 = cdec_query("HRO", 70, "D", as.Date("2014-01-01"), as.Date("2021-10-31"))

Ex2 = cdec_query("TRP", 70, "D", as.Date("2014-01-01"), as.Date("2021-10-31"))

flow = bind_rows(Outflow, Ex1, Ex2, DTO)

ggplot(flow, aes(x = DateTime, y = Value, color = StationID))+geom_line()+
  facet_wrap(~StationID, scales = "free_y", nrow = 3)

#Calculate total exports

flow2 = group_by(flow, DateTime, SensorNumber, SensorType) %>%
  summarize(Value = sum(Value)) %>%
  mutate(Year = year(DateTime), Month = month(DateTime))

ggplot(flow2, aes(x = DateTime, y = Value, color = SensorType))+geom_line()+
  facet_wrap(~SensorType, scales = "free_y", nrow = 3)

#Average by month
flowmonth = group_by(flow2, Month, Year, SensorType) %>%
  summarize(Value = mean(Value, na.rm=T)) %>%
  mutate(Yearmonth = Year + (Month-1)/12)

ggplot(flowmonth, aes(x = Yearmonth, y = Value, color = SensorType))+geom_line()+
  facet_wrap(~SensorType, scales = "free_y", nrow = 3)

load("barrierhabs.RData")

#We may also want to do absent/low/high
BH2 = mutate(BH2, HABord = case_when(
  Microcystis == 1 ~ "Absent",
  Microcystis %in% c(2,3) ~ "Low",
  Microcystis %in% c(4,5) ~ "High"
),
HABord = factor(HABord, levels = c("Absent", "Low", "High"), ordered = T)) %>%
  filter(Year>2013) %>%
  droplevels()


#summarize by month
BHmonth = group_by(BH2, Month, Year) %>%
  summarise(Absent = length(HABord[which(HABord == "Absent")]),
          Low = length(HABord[which(HABord == "Low")]),
         High = length(HABord[which(HABord == "High")]),
         Total = length(HABord)) %>%
  mutate(Yearmonth = Year + (Month-1)/12,
         Absentp = Absent/Total,
         Highp = High/Total,
         Lowp = Low/Total,
         Present = Low+High,
         Presentp = 1-Absentp)

flowm2 = pivot_wider(flowmonth, id_cols = c(Month, Year), names_from = SensorType, values_from = Value)

Habflow = left_join(BHmonth, flowm2)

ggplot(Habflow, aes(x=Absentp, y = log(OUTFLOW))) +geom_point()+geom_smooth()

ggplot(Habflow, aes(x=Lowp, y = log(OUTFLOW))) +geom_point()+geom_smooth()
ggplot(Habflow, aes(x=Highp, y = log(OUTFLOW))) +geom_point()+geom_smooth()
ggplot(Habflow, aes(y=Presentp, x = log(OUTFLOW))) +
  geom_point(aes( color = as.factor(Year)))+geom_smooth(method = "lm") +
  ylab("Percent of observations with Microcystis present")+
  xlab("Log-transformed Delta Outflow (CFS)") + theme_bw()+
  scale_color_discrete(name = NULL)+
  geom_vline(xintercept = log(3000), linetype = 2, color = "blue")+
  geom_vline(xintercept = log(4000), color = "red")+
  annotate("text", x = 7.95, y = 0, label = "TUCP", angle = 90, color = "blue")+
  annotate("text", x = 8.25, y = 0, label = "D-1641", angle = 90, color = "red")

b1 = glmer(cbind(Present,Total)~ log(OUTFLOW) + (1|Year), data = Habflow,
           family = "binomial")
summary(b1)

newdata = filter(Habflow, Year == 2021, Month %in% c(6,7,8))

ggplot(filter(Habflow, Year == 2021, Month %in% c(6,7,8)), 
              aes(x=Month, y = OUTFLOW)) + geom_line() + 
  coord_cartesian(ylim= c(2000, 4000))

foo = predict(b1, newdata = newdata)

newdata2 = newdata       
newdata2$OUTFLOW = 4000
foo2 = predict(b1, newdata = newdata2)
foo
foo2

library(rsq)
rsq(b1)

#############################################################################
#look at all months, not just May-October

str(HABs)
HABs = filter(HABs, !is.na(Month), !is.na(Microcystis))
ggplot(HABs, aes(x = as.factor(Month), fill = as.factor(Microcystis))) + 
  geom_bar(position = "fill")+
  scale_fill_manual(values = c("white", "tan2", "yellow", "red", "darkred"), 
                    labels = c("absent", "low", "medium", "high", "very high"),
                    name = "Microcystis")+ ylab("Relative Frequency")+
  xlab("Month of year (2013-2021)")
