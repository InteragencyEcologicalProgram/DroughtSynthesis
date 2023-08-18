#What do we know about how quickly reservoirs rebound after droughts?

library(lubridate)
library(tidyverse)
library(cder)

shasta = cdec_query("SHA", 15, "D", start.date = as.Date("1975-01-01"), end.date = as.Date("2023-01-01"))

ggplot(shasta, aes(x = ObsDate, y = Value))+ geom_line()


#list of droughts
droughts = data.frame(droughtstart = as.POSIXct(c(ymd("1975-10-1"), ymd("1986-10-1"), ymd("2006-10-1"), ymd("2011-10-1"), ymd("2019-10-1"))),
                      droughtend = as.POSIXct(c(ymd("1977-9-30"), ymd("1994-9-30"), ymd("2009-09-30"), ymd("2016-09-30"), ymd("2022-09-30")))) 


ggplot(shasta)+ 
  geom_rect(data = droughts, aes(xmin = droughtstart, xmax = droughtend, 
                                 ymin = 0, ymax = max(shasta$Value+1000, na.rm =T)), 
            alpha = 0.5, inherit.aes = F, fill = "goldenrod")+
  geom_line(aes(x = ObsDate, y = Value, color = month(ObsDate)), linewidth =1)+theme_bw()+
  scale_color_viridis_c(option ="A")+
  coord_cartesian(xlim = c(ymd_hms("1989-01-01 00:00:00"), ymd_hms("2022-10-31 00:00:00")))+
  xlab("Date")+
  ylab("Reservior Storage (thousand acre-feet)")

oro = cdec_query("ORO", 15, "D", start.date = as.Date("1975-01-01"), end.date = as.Date("2023-01-01")) 
ggplot(oro, aes(x = ObsDate, y = Value))+ geom_line()+
  geom_rect(data = droughts, aes(xmin = droughtstart, xmax = droughtend, ymin = 0, ymax = max(oro$Value+1000, na.rm =T)), 
            alpha = 0.5, inherit.aes = F)+
  theme_bw()


folsom = cdec_query("FOL", 15, "D", start.date = as.Date("1975-01-01"), end.date = as.Date("2023-01-01")) 
ggplot(folsom, aes(x = ObsDate, y = Value))+ geom_line()+
  geom_rect(data = droughts, aes(xmin = droughtstart, xmax = droughtend, ymin = 0, ymax = max(folsom$Value+1000, na.rm =T)), 
            alpha = 0.5, inherit.aes = F)+
  theme_bw()
