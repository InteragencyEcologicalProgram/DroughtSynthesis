#Salvage


library(tidyverse)
library(lubridate)
library(smonitr)
#I need to read in the Dayflow data from the CNRA portal
# https://data.cnra.ca.gov/dataset/dayflow
#Still needs a little fiddling, but much better!.

Dayflow = get_odp_data(pkg_id = "dayflow", fnames = "Dayflow Results")

DF1929_1939 = Dayflow$`Dayflow Results 1929 - 1939`%>%
  mutate(Date = as.Date(Date, format = "%m/%d/%Y"), OUT = OUT1) %>%
  dplyr::select(Date, OUT, SAC)

DF1940_1949 = Dayflow$`Dayflow Results 1940 - 1949` %>%
  mutate( Date = as.Date(Date, format = "%m/%d/%Y"), OUT = OUT2) %>%
  dplyr::select(Date, OUT)

DF1950_1955= Dayflow$`Dayflow Results 1950 - 1955` %>%
  mutate( Date = as.Date(Date, format = "%m/%d/%Y"), OUT = OUT2) %>%
  dplyr::select(Date, OUT, SAC)

DF1956_1969 = Dayflow$`Dayflow Results 1956 - 1969` %>%
  mutate( Date = as.Date(Date, format = "%m/%d/%Y")) %>%
  dplyr::select(Date, OUT, SAC)

DF1970_1983 = Dayflow$`Dayflow Results 1970 - 1983` %>%
  mutate( Date = as.Date(Date, format = "%m/%d/%Y")) %>%
  dplyr::select(Date, OUT, SAC)

DF1984_1996 = Dayflow$`Dayflow Results 1984 - 1996` %>%
  mutate( Date = as.Date(Date, format = "%m/%d/%Y")) %>%
  dplyr::select(Date, OUT, SAC)

DF1997_2019 =  Dayflow$`Dayflow Results 1997 - 2020` %>%
  mutate( Date = as.Date(Date, format = "%m/%d/%Y")) %>%
  dplyr::select(Date, OUT, SAC)

#now I can put them all together!
DF = bind_rows(DF1929_1939, DF1940_1949, DF1950_1955, DF1956_1969, DF1970_1983, DF1984_1996, DF1997_2019)
DF = mutate(DF, julian = yday(Date), Year = year(Date))

#Salvage data

salvage = read_csv("data/salvagecatch3.txt") %>%
  mutate(OrganismCode = as.character(OrganismCode))
orgcodes = read_excel("data/OrganismsLookUp.xlsx")

salvage = left_join(salvage, orgcodes)

#add in zeros
sal2 = pivot_wider(salvage, id_cols = c(SampleDate, AcreFeet, MinutesPumping, SampleTimeLength,
                                        SampleMethod, BuildingCode), 
                   names_from = CommonName, values_from = Count, values_fill = 0,
                   values_fn = sum) %>%
  pivot_longer(cols = c(Jacksmelt:last_col()), names_to = "Species", values_to = "count") %>%
  mutate(Year = year(SampleDate), Month = month(SampleDate))

carp = filter(sal2, Species == "Common Carp") %>%
  mutate(julian = yday(SampleDate))


species = group_by(sal2, Species) %>%
  summarize(total = sum(count, na.rm = T), percent = total/sum(sal2020$count, na.rm = T))
rare = species$Species[which(species$percent <.01)]


annual = mutate(sal2, Species2 = case_when(
  Species %in% rare ~ "other",
  TRUE ~ Species
)) %>%
  group_by(Year, Species2) %>%
  summarize(catch = sum(count, na.rm = T))

sal2020 = filter(sal2, Year == 2020) %>%
  mutate(julian = yday(SampleDate))

sal2020day = group_by(sal2020, julian, Species) %>%
  summarize(count = sum(count, na.rm = T), effort = sum(SampleTimeLength), AcreFeet = sum(AcreFeet))

ggplot(carp, aes(x = julian, y = count))+ 
  geom_point()+geom_smooth()+scale_y_log10()+
  ylab("Number of carp caught per sample")+
  xlab("Day of year")

ggplot(sal2020day, aes(x = julian, y = count, fill = Species)) +
  geom_area(position = "fill")

species = group_by(sal2020, Species) %>%
  summarize(total = sum(count, na.rm = T), percent = total/sum(sal2020$count, na.rm = T))
rare = species$Species[which(species$total <10)]

sal2020day = mutate(sal2020day, Species2 = case_when(
  Species %in% rare ~ "other",
  TRUE ~ Species
))
ggplot(sal2020day, aes(x = julian, y = count, fill = Species2)) +
  geom_area(position = "fill")+ylab("relative abundance")+
  xlab("day of year")+ ggtitle("Salvage trends of 2020")


shad = filter(sal2, Species == "Threadfin Shad") %>%
  mutate(julian = yday(SampleDate),
         cpue = count/SampleTimeLength,
         cpue2 = count/SampleTimeLength*AcreFeet)

ggplot(shad, aes(x = julian, y = count)) + geom_point()
ggplot(shad, aes(x = julian, y = cpue)) + geom_point()+ 
  ylab("Shad per Minute") + xlab("Day of Year")

shad2 = group_by(shad, julian) %>%
  summarize(cpue = mean(cpue, na.rm=T))

ggplot(shad2, aes(x = julian, y = cpue)) + geom_point()+ 
  ylab("Shad per Minute") + xlab("Day of Year")


ggplot(shad, aes(x = julian, y = count))+ 
  geom_point()+geom_smooth()+scale_y_log10()+
  ylab("Number of shad caught per sample")+
  xlab("Day of year")

ggplot(annual, aes(x=Year, y = catch, fill = Species2)) + geom_col() +
  scale_fill_manual(values = mypal)+ylab("total annual catch")+ggtitle("Fish Collected in salvage")


library(RColorBrewer)
mypal = c(brewer.pal(8, "Dark2"), brewer.pal(8, "Set2"), brewer.pal(12, "Set3"), "black", "grey")


Chin2011 = filter(sal2, Year %in% c(2011, 2006, 2017, 2019), Species == "Chinook Salmon") %>%
  mutate(julian = yday(SampleDate), Week = week(SampleDate), CPUE = count/SampleTimeLength)

ggplot(Chin2011, aes(x = julian, y = count)) + geom_point() + xlab("Day of Year")+ylab("Catch of Chinook Salmon")
ggplot(Chin2011, aes(x = SampleDate, y = CPUE)) + geom_point() + xlab("Day of Year")+ylab("CPUE of Chinook Salmon (fish/minute)")
ggplot(Chin2011, aes(x = SampleDate, y = AcreFeet)) + 
  geom_point(aes(color = BuildingCode)) + xlab("Day of Year")+ylab("CPUE of Chinook Salmon (fish/minute)")


Chin20112 = group_by(Chin2011, julian, Week, Year) %>%
  summarize(exports = sum(unique(AcreFeet), na.rm = T), salmon = sum(count, na.rm = T),
            cpue = mean(CPUE, na.rm = T))

ggplot(Chin2011, aes(x = SampleDate, y = AcreFeet)) + 
  geom_point(aes(color = BuildingCode)) + xlab("Day of Year")+
  ylab("CPUE of Chinook Salmon (fish/minute)")

ggplot(Chin20112, aes(x = julian, y = cpue)) + 
  geom_point(aes(color = as.factor(Year))) + xlab("Day of Year")+
  ylab("CPUE of Chinook Salmon (fish/minute)")

ggplot(Chin20112, aes(x = julian, y = exports, color = as.factor(Year))) + 
  geom_line() + xlab("Day of Year")+
  ylab("total exports (AcreFeet per day)")

DFwet = filter(DF, Year %in% c(2006, 2011, 2017, 2019)) %>%
  mutate(Week = week(Date)) %>%
  group_by(Year, Week) %>%
  summarize(SAC = mean(SAC, na.rm = T))

EXwet = group_by(Chin20112, Year, Week) %>%
  summarize(Exports = mean(exports, na.rm = T), Salmon = mean(cpue))

ggplot(DFwet, aes(x = Week, y = SAC, color = as.factor(Year)))+
  geom_line() + ylab("weekly average sacrmento river flow (CFS)")

ggplot(EXwet, aes(x = Week, y = Exports, color = as.factor(Year)))+
  geom_line() + ylab("Weekly average exports (Acre-feet per day)")

ggplot(EXwet, aes(x = Week, y = Salmon, color = as.factor(Year)))+
  geom_line() + ylab("Weekly salmon cpue (fish per minute sampled)")


#weekly averages might be clearer

#need to use data queried by Ian's group for WY2021

library(zoo)
library(readxl)
#import NDOI calculated by O and M
DTO = read_csv("data/NDOI_WY2021.csv")

#import NDOI standards from D1641 and the 2021 TUCP
D1641 = read_excel("data/D1641_NDOI standards3.csv.xlsx")
D1641 = mutate(D1641, Date = as.Date(Date))

D1641dry = filter(D1641, WYT == "Critical")
#Calculate 7-day rolling average

DTO = mutate(DTO, Date = mdy(Date), weekmean = rollmean(NDOIcfs, k = 14, fill = NA))

ggplot() + geom_line(data = DTO, aes(x = Date, y = weekmean))+
  coord_cartesian(xlim = c(as.Date("2021-06-01"), as.Date("2021-09-01")), ylim = c(2000, 5000))+
  geom_line(data = D1641dry, aes(x = Date, y = Value, color = Regulation)) +
  ylab("Net Delta Outflow Index (CFS)") +xlab("date of WY 2021")+ theme_bw()
