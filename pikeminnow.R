
library(tidyverse)
library(readxl)
library(lubridate)
library(chron)
library(magrittr)
library(tsibble)
library(ggplot2)
library(dataRetrieval) # may require R v4.1.1
library(DescTools)
library(emmeans)

Trawl1 = read_csv("data/1976-2001_DJFMP_trawl_fish_and_water_quality_data.csv")
Trawl2 = read_csv("data/2002-2021_DJFMP_trawl_fish_and_water_quality_data.csv")
Trawls = bind_rows(Trawl1, Trawl2)%>%
  dplyr::select(Location, StationCode, SampleDate, MethodCode, TowNumber, TowDuration, Volume, IEPFishCode, Count)
TrawlsSP = pivot_wider(Trawls, names_from = IEPFishCode, values_from= Count, values_fn = sum, values_fill = 0) %>%
  dplyr::select(Location, StationCode, SampleDate, MethodCode, TowNumber, TowDuration, Volume, SACPIK) %>% 
  mutate(year_wk = yearweek(SampleDate),
         setID = paste(SampleDate, StationCode),
         CPUE = SACPIK/Volume)
#calculate cummulative CPUE for Chipps
Chipps = filter(TrawlsSP, Location == "Chipps Island") %>%
  arrange(SampleDate) %>%
  mutate(Year = year(SampleDate), julian = yday(SampleDate), Month = month(SampleDate), ymonth = Year + (Month/12-1), wyjulian = case_when(
    julian > 274 ~ julian -274,
    TRUE ~ julian +91
  ))%>%
  rename(Date = SampleDate) %>%
  addWaterYear() %>%
  group_by(Year, Month, ymonth) %>%
  summarize(SPike = sum(SACPIK, na.rm = T))

ggplot(Chipps, aes(x = ymonth, y = log(SPike+1))) + geom_point()
ggplot(filter(Chipps, Year >2013), aes(x = Month, y = SPike)) + geom_point() +
  facet_wrap(~Year)
