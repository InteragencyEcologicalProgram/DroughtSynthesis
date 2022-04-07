#quick plots of flow

library(smonitr)
library(lubridate)
library(tidyverse)
library(zoo)

Dayflow = get_odp_data(pkg_id = "dayflow", fnames = "Dayflow Results")


DF1997_2020 =  Dayflow$`Dayflow Results 1997 - 2020` %>%
  mutate( Date = as.Date(Date, format = "%m/%d/%Y")) %>%
  dplyr::select(Date, OUT, EXPORTS, SJR)


DF2021 =  Dayflow$`Dayflow Results 2021` %>%
  mutate( Date = as.Date(Date, format = "%m/%d/%Y")) %>%
  dplyr::select(Date, OUT, EXPORTS, SJR) 


#now I can put them all together!
DF = bind_rows(DF1997_2020, DF2021)

DFlong = pivot_longer(DF, cols = c(OUT, EXPORTS, SJR), names_to = "Metric", values_to = "CFS") %>%
  filter(CFS >0) %>%
  group_by(Metric) %>%
  mutate(Sevenday = rollmean(CFS, k = 7, fill = NA), DOY = yday(Date))

DFmean = group_by(DFlong, Metric, DOY) %>%
  summarize(Sevenday = exp(mean(log(Sevenday), na.rm = T)), CFS = mean(CFS, na.rm = T)) %>%
  mutate(Metric2 = "Historic Mean")

DFlong2 = mutate(DFlong, Metric2 = "2021") %>%
  filter(Date > ymd("2021-05-01")) %>%
  bind_rows(DFmean)

ggplot(filter(DFlong, Date > ymd("2021-05-01"), CFS >0), aes(x = Date, y = CFS, color = Metric))+
         geom_line()

ggplot(filter(DFlong, Date > ymd("2021-05-01"), CFS >0), aes(x = Date, y = Sevenday, color = Metric))+
  geom_line()

ggplot(filter(DFlong2, DOY >120, DOY <270) )+
  geom_line(aes(x = DOY, y = Sevenday, color = Metric, linetype = Metric2), size = 1)+
   theme_bw()+
  scale_x_continuous(breaks = c(121, 152, 182, 213, 244, 274), labels = c("May", "Jun", "Jul", "Aug", "Sep", "Oct"))+
  scale_linetype(name = NULL)+
  scale_color_brewer(palette = "Set2", name = NULL)+
  xlab("Date")+ ylab("Flow (CFS - seven day average)")

DFmonth = mutate(DFlong2, Month = month(Date)) %>%
  group_by(Metric, Metric2, Month) %>%
  summarize(Min = min(CFS, na.rm = T), Max = max(CFS, na.rm = T), mean = mean(CFS, na.rm = T))

DFmonth2 = pivot_wider(filter(DFmonth, Metric2 == "2021"), id_cols = Month, names_from = Metric, values_from = c(Min, Max, mean))       
write.csv(DFmonth2, file = "Flowsummary.csv", row.names = F)       
