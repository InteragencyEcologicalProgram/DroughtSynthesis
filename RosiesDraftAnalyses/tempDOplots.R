#query DO and temp data from CDEC

library(cder)
library(tidyverse)
library(lubridate)

#Use the "cdec_query" function to query all the water temperature and
#DO data from relevent stations.
cdec = cdec_query(c("SRH", "MIR", "RVB", "TWI", "FRK", "ANH", "SJJ", "GGS"), 
                  c(61, 25, 146), start.date = as.Date("2014-06-01"), 
                  end.date = as.Date("2017-09-01"))

#Create new variable for year, day of year, and hour. THen filter for just the summer months
#and caclculate hourly means
cdecH = mutate(cdec, Month =month(ObsDate), Hour = hour(DateTime), julian = yday(DateTime), Year = as.factor(year(DateTime))) %>%
  filter(Month %in% c(6,7,8)) %>%
  group_by(StationID, SensorNumber, SensorType, ObsDate, Month, Hour, Year, julian) %>%
  summarize(Value = mean(Value, na.rm = T))

#filter out just the temperature data and convert it to Celsius.
#The CDEC data aren't QAQC'd, so I just filtered out the rediculous values.
temp = filter(cdecH, SensorType == "TEMP W", Value > 60, Value < 90) %>%
  mutate(celsius = (Value-32)*5/9)

#line plot of temperature by station, with different colors for each year
ggplot(temp, aes(x = julian, y = celsius, color = as.factor(Year))) + geom_line() +
  facet_wrap(~StationID) + theme_bw()+ ylab("Temperature in degrees C") + xlab("Day of Year") +
  scale_color_discrete(name = NULL)

#box plot
ggplot(temp, aes(x = Year, y = celsius, fill = Year)) + geom_boxplot() +
  facet_wrap(~StationID) + theme_bw()+ ylab("Summer Temperature in degrees C") + xlab("Day of Year") +
  scale_color_discrete(name = NULL)


#now the plots for dissolved oxygen
ggplot(filter(cdecH, SensorType == "DIS OXY", Value > 1, Value < 20), aes(x = julian, y = Value, color = as.factor(Year))) + geom_line() +
  facet_wrap(~StationID) + ylab("Dissolved Oxygen mg/L") + 
  xlab("Day of Year") + theme_bw() + scale_color_discrete(name = NULL)

ggplot(filter(cdecH, SensorType == "DIS OXY", Value > 1, Value < 20), aes(x = Year, y = Value, fill = Year)) + geom_boxplot() +
  facet_wrap(~StationID) + ylab("Dissolved Oxygen mg/L") + xlab("Year") + theme_bw() + scale_color_discrete(name = NULL)
