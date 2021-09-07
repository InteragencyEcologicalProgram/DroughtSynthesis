#Check out water temperature in Franks Tract this year versus 2015

library(tidyverse)
library(cder)
library(lubridate)

FRK = cdec_query("FRK", 25, start.date = as.Date("2015-01-01"), 
                 end.date = as.Date("2021-08-26"))

FAL = cdec_query("FAL", 25, start.date = as.Date("2015-01-01"), 
                 end.date = as.Date("2021-08-26"))

FRKFAL = bind_rows(FRK, FAL) %>%
  filter(Value < 100) %>%
  mutate(julian = yday(DateTime), Date = date(DateTime), Year = year(DateTime)) %>%
  group_by(StationID, julian, Year, Date) %>%
  summarize(Max = max(Value), Min = min(Value), MeanTemp = mean(Value, na.rm = T))

ggplot(FRKFAL, aes(x = julian, y = MeanTemp, color = as.factor(Year))) + geom_line()+
  facet_wrap(~StationID)

ggplot(filter(FRKFAL, julian >60 & julian <300, Year %in% c(2015, 2018, 2021)), 
aes(x = julian, y = MeanTemp, color = as.factor(Year))) + geom_line()+
  facet_wrap(~StationID)+
  scale_x_continuous(breaks = c(60, 121, 182, 244, 300),
                                            labels = c("Mar", "May", "Jul", "Sep", "Nov"))+
  coord_cartesian(ylim = c(50, 80)) +
  ylab("Mean Water Temperature (F)")+
  xlab("Day of the Year")

ggplot(filter(FRKFAL, julian >60 & julian <300, Year %in% c(2015, 2018, 2021)), 
       aes(x = julian, y = Max, color = as.factor(Year))) + geom_line()+
  facet_wrap(~StationID)+
  scale_x_continuous(breaks = c(60, 121, 182, 244, 300),
                     labels = c("Mar", "May", "Jul", "Sep", "Nov"))+
  coord_cartesian(ylim = c(50, 80))
