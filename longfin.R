
library(tidyverse)
library(lubridate)

EDSM <- read_csv("Enhanced Delta Smelt Monitoring Trawls CHN & POD Species 2016-2021.csv")
EDSM = mutate(EDSM, Date = mdy(Date))

EDSM2 = group_by(EDSM, Date, StartLat, StartLong, CommonName) %>%
  summarize(Count = n())
                 
EDSM3 = pivot_wider(EDSM2, id_cols = c(Date, StartLat, StartLong), names_from = CommonName, values_from = Count)
EDSMfall = filter(EDSM3, Date > ymd("2021-09-01"))

EDSMfallLongfin = mutate(EDSMfall, longfin = case_when(
  is.na(`longfin smelt`) ~ as.integer(0),
  TRUE ~ `longfin smelt`
))

EDSMsf = st_as_sf(EDSMfallLongfin, coords = c("StartLong", "StartLat"), crs = 4326) 

ggplot() + geom_sf(data = WW_Delta, fill = "lightblue")+
  geom_sf(data =EDSMsf, aes(shape = as.factor(longfin), size = as.factor(longfin)))+
  scale_shape_manual(values = c(4, 19))+
  scale_size_manual(values = c(1, 5))+
  coord_sf(xlim = c(-122.2, -121.4), ylim = c(37.8, 38.6))+
  north(data = reg3, symbol = 2) +
  theme_bw()+ylab("")+xlab("")
