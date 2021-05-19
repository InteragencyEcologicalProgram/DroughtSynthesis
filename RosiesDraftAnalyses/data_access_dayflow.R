#get teh dayflow data

library(tidyverse)
library(lubridate)
library(smonitr)

#I need to read in the Dayflow data from the CNRA portal
# https://data.cnra.ca.gov/dataset/dayflow
#Still needs a little fiddling, but much better!.

Dayflow = get_odp_data(pkg_id = "dayflow", fnames = "Dayflow Results")


DF1997_2020 =  Dayflow$`Dayflow Results 1997 - 2020` %>%
  mutate( Date = as.Date(Date, format = "%m/%d/%Y")) %>%
  select(Date, OUT, X2)

DF1997_2020 = mutate(DF1997_2020, Month = month(Date), Julian = yday(Date))

Fall = data.frame(Starts = c("2017-09-01", "2018-09-01", "2019-09-01", "2020-09-01"),
                     Stops = c("2017-11-01", "2018-11-01", "2019-11-01", "2020-11-01"))
Fall = mutate(Fall, Starts = as.Date(Starts), Stops = as.Date(Stops))

ggplot(DF1997_2020, aes(x = Date, y = X2)) + geom_point()

ggplot(DF1997_2020, aes(x = Date, y = X2)) + geom_point() +
  coord_cartesian(xlim = c(as.Date("2017-01-01"), as.Date("2020-12-30")))+
 annotate("rect", xmin = Fall$Starts, xmax = Fall$Stops, ymin = 35, ymax = 90, alpha = 0.3)+
  annotate("text", x = Fall$Start, y = 50, label = "FALL")+
  annotate("rect", xmin = as.Date("2017-01-01"), xmax = as.Date("2020-12-30"), 
           ymin = 55, ymax = 75, alpha = 0.3, fill = "cyan2")+
  annotate("rect", xmin = as.Date("2017-01-01"), xmax = as.Date("2020-12-30"), 
           ymin = 75, ymax = 90, alpha = 0.3, fill = "orange")+
  annotate("text", x= as.Date("2017-04-01"), 
           y = 65, label = "Suisun", angle = 90)+
  annotate("text", x = as.Date("2017-04-01"), 
           y = 85, label = "Confluence", angle = 90)
