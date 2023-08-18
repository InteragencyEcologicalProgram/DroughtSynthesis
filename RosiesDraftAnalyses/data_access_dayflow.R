#get teh dayflow data

library(tidyverse)
library(lubridate)
library(smonitr)

#I need to read in the Dayflow data from the CNRA portal
# https://data.cnra.ca.gov/dataset/dayflow
#Still needs a little fiddling, but much better!.

Dayflow = get_odp_data(pkg_id = "dayflow", fnames = "Dayflow Results")

DF20s = Dayflow$`Dayflow Results 1929 - 1939`%>%
  mutate( Date = as.Date(Date, format = "%m/%d/%Y"), OUT = OUT1) %>%
  select(Date, OUT, SAC, SJR)

DF40s = Dayflow$`Dayflow Results 1940 - 1949`%>%
  mutate( Date = as.Date(Date, format = "%m/%d/%Y"), OUT = OUT2, EXPORTS =EXPOR) %>%
  select(Date, OUT, SAC, SJR, EXPORTS)

DF50s = Dayflow$`Dayflow Results 1950 - 1955`%>%
  mutate( Date = as.Date(Date, format = "%m/%d/%Y"), OUT = OUT2, EXPORTS =EXPOR) %>%
  select(Date, OUT, SAC, SJR, EXPORTS)

DF60s = Dayflow$`Dayflow Results 1956 - 1969`%>%
  mutate( Date = as.Date(Date, format = "%m/%d/%Y"), EXPORTS =EXPORT) %>%
  select(Date, OUT, SAC, SJR, EXPORTS, CVP, SWP)


DF1997_2020 =  Dayflow$`Dayflow Results 1997 - 2020` %>%
  mutate( Date = as.Date(Date, format = "%m/%d/%Y")) %>%
  select(Date, OUT, X2, EXPORTS, SAC, SJR, SWP, CVP)

DF1997_2020 = mutate(DF1997_2020, Month = month(Date), Julian = yday(Date))



DF1970_1983 =  Dayflow$`Dayflow Results 1970 - 1983` %>%
  mutate( Date = as.Date(Date, format = "%m/%d/%Y")) %>%
  select(Date, OUT, EXPORT, SAC, SJR, SWP, CVP) %>%
mutate(Month = month(Date), Julian = yday(Date)) %>%
  rename(EXPORTS = EXPORT)

DF1984_1996 = Dayflow$`Dayflow Results 1984 - 1996` %>%
  mutate( Date = as.Date(Date, format = "%m/%d/%Y")) %>%
  select(Date, OUT, EXPORT, SAC, SJR, SWP, CVP) %>%
  mutate(Month = month(Date), Julian = yday(Date)) %>%
  rename(EXPORTS = EXPORT)

Dflow = bind_rows(DF20s, DF40s, DF50s, DF60s, DF1970_1983, DF1984_1996, DF1997_2020) %>%
 mutate(Year = year(Date), Year=if_else(Month==12, Year+1, Year), # Move Decembers to the following year
Season=case_when(Month%in%3:5 ~ "Spring", # Create seasonal variables
                 Month%in%6:8 ~ "Summer",
                 Month%in%9:11 ~ "Fall",
                 Month%in%c(12, 1, 2) ~ "Winter",
                 TRUE ~ NA_character_))

write.csv(Dflow, "Dayflow.csv")

DFsum = group_by(Dflow, Year) %>%
  summarize(X2 = mean(X2), OUT = mean(OUT), EXPORTS = mean(EXPORTS),
            SAC = mean(SAC), SJR = mean(SJR), SWP = mean(SWP), CVP = mean(CVP))


ggplot(DFsum, aes(x = Year, y = SAC)) +
  geom_line()

ggplot(DFsum, aes(x = Year, y = SJR)) +
  geom_line()

ggplot(DFsum, aes(x = Year, y = EXPORTS)) +
  geom_line()

ggplot(filter(Dflow, Year >2015), aes(x = Date, y = EXPORTS)) +
  geom_line()

ggplot(DFsum, aes(x = Year, y = OUT)) +
  geom_line()

ggplot(DFsum, aes(x = Year, y = CVP)) +
  geom_line()

yeartypes <- read_csv("~/Drought/FLOATDrought/yeartypes.csv")
DFsum2 = left_join(DFsum, yeartypes) %>%
  filter(!is.na(Drought))

DFsum3= group_by(Dflow, Year) %>%
  summarize(X2 = mean(X2), OUT = mean(OUT), EXPORTS = mean(EXPORTS)) %>% 
  left_join(yeartypes) %>%
  filter(!is.na(Drought), Drought != "N")


ggplot(DFsum2, aes(x = Season, y = OUT)) + geom_boxplot()+ facet_wrap(~Drought)
ggplot(DFsum3, aes(x = Drought, fill = Drought, y = OUT)) + geom_boxplot()+ ylab("Mean Delta Outflow") +
  scale_x_discrete(labels = c("Multi-year \n Drought", "Multi-year \n Wet")) +
  scale_fill_discrete(guide = F)

ggplot(DFsum3, aes(x = Drought, fill = Drought, y = EXPORTS)) + geom_boxplot()+ ylab("Mean Exports") +
  scale_x_discrete(labels = c("Multi-year \n Drought", "Multi-year \n Wet")) +
  scale_fill_discrete(guide = F)

ggplot(DFsum3, aes(x = Year, color = Drought, y = EXPORTS)) + geom_point()+ ylab("Mean Exports") 

###############################################################################3
#Fall X2 plot that I don't remember why I made
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
