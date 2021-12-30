# just want to quickly graph total nitrogen and total phosphorus over time.

library(tidyverse)
library(smonitr)
library(lubridate)  

nutsx = get_edi_data(458, "SACSJ_delta_water_quality_1975_2020.csv", guess_max = 1000000)
nuts = nutsx[[1]] %>%
  mutate(Date = mdy(Date)) %>%
  select(DissAmmonia, DissNitrateNitrite, TotPhos, Station, Date, Chla, Pheophytin, DOC, TOC, DON, TKN, DissOrthophos, TotPhos)
str(nuts)
Nuts2021 = read_csv("data/EMP_delta_water_quality_Oct20-Nov21.csv", guess_max = 1000000) %>%
  mutate(Date = mdy(Date))%>%
  select(DissAmmonia, DissNitrateNitrite, TotPhos, Station, Date, Chla, Pheophytin, DOC, TOC, DON, TKN, DissOrthophos, TotPhos) %>%
  mutate(
DissAmmonia = as.numeric(case_when(
  DissAmmonia == "ND" ~"0",
  TRUE ~ DissAmmonia)),
DissOrthophos =  as.numeric(case_when(
  DissOrthophos == "ND" ~'0',
  TRUE ~ DissOrthophos)), 
DissNitrateNitrite =  as.numeric(case_when(
  DissNitrateNitrite == "ND" ~'0',
  TRUE ~ DissNitrateNitrite)), 
DOC =  as.numeric(case_when(
  DOC == "ND" ~'0',
  TRUE ~ DOC))
)
str(Nuts2021)



nuts = mutate(nuts, TKN = as.numeric(case_when(
  TKN == "ND" ~"0",
  TRUE ~ TKN)),  
  DissAmmonia = as.numeric(case_when(
    DissAmmonia == "ND" ~"0",
    TRUE ~ DissAmmonia)),
  DissOrthophos =  as.numeric(case_when(
    DissOrthophos == "ND" ~'0',
    TRUE ~ DissOrthophos)), 
  DissNitrateNitrite =  as.numeric(case_when(
    DissNitrateNitrite == "ND" ~'0',
    TRUE ~ DissNitrateNitrite)), 
  TotPhos =  as.numeric(case_when(
    TotPhos == "ND" ~'0',
    TRUE ~ TotPhos)),
  DOC =  as.numeric(case_when(
    DOC == "ND" ~'0',
    TRUE ~ DOC))
                )

nutsall = bind_rows(nuts, Nuts2021)

nutsmeanSummer = nutsall %>%
  mutate(Year = year(Date), Month = month(Date)) %>%
  #filter(Month %in% c(5,6,7,8,9)) %>%
  group_by(Year) %>%
  summarize(TKN = mean(TKN, na.rm = T), DissAmmonia = mean(DissAmmonia, na.rm = T),
            DissOrthophos = mean(DissOrthophos, na.rm = T), TotPhos = mean(TotPhos, na.rm = T),
            DissNN = mean(DissNitrateNitrite, na.rm = T), DOC = mean(DOC, na.rm = T))

nutsmean2 = pivot_longer(nutsmeanSummer, 
                         cols = c(TKN, DissAmmonia, DissOrthophos, TotPhos,
                                  DissNN), names_to = "Analyte",
                         values_to = "Restult")

ggplot(nutsmeanSummer, aes(x = Year, y = TKN)) + geom_line()
ggplot(nutsmeanSummer, aes(x = Year, y = DissAmmonia)) + geom_line()
ggplot(nutsmeanSummer, aes(x = Year, y = DissOrthophos)) + geom_line()

ggplot(filter(nutsmean2, Analyte != "TON"), aes(x = Year, y = Restult, color = Analyte)) + 
  geom_line() + geom_point()+ylab("Mean Annual Concentration mg/L") + theme_bw()


nuts2 = pivot_longer(nuts, 
                         cols = c(TKN, DissAmmonia, DissOrthophos, TotPhos,
                                  DissNitrateNitrite, TON), names_to = "Analyte",
                         values_to = "Result") %>%
  mutate(Year = year(Date), Month = month(Date))


ggplot(filter(nuts2, Analyte != "TON"), aes(x = Date, y = log(Result), color = Analyte)) + geom_point() + geom_smooth(k = 6) + 
  facet_grid(Month~Analyte, scales = "free_y")


###################
#monthly means
nutsmeanmonth = nuts %>%
  mutate(Year = year(Date), Month = month(Date), Yearmon = Year + 1/Month) %>%
  #filter(Month %in% c(5,6,7,8,9)) %>%
  group_by(Year, Month, Yearmon) %>%
  summarize(TKN = mean(TKN, na.rm = T), DissAmmonia = mean(DissAmmonia, na.rm = T),
            DissOrthophos = mean(DissOrthophos, na.rm = T), TotPhos = mean(TotPhos, na.rm = T),
            DissNN = mean(DissNitrateNitrite, na.rm = T), DOC = mean(DOC, na.rm = T))

nutsmont = pivot_longer(nutsmeanmonth, 
                         cols = c(TKN, DissAmmonia, DissOrthophos, TotPhos,
                                  DissNN, DOC), names_to = "Analyte",
                         values_to = "Result")

ggplot(nutsmont, aes(x = Yearmon, y = log(Result), color = Analyte)) + geom_point() + geom_smooth() #+ 
#  facet_grid(Month~Analyte, scales = "free_y")

ggplot(filter(nutsmont, Analyte != "DOC"), aes(x = Yearmon, y = Result, color = Analyte)) + geom_point() + geom_line() #+
#  facet_wrap(~Month)

#################################
#regions and seasons
regions = read_csv("IEP_StationswRegions_19OCT2021.csv") %>%
  rename(Station = StationCode)
nutswreg = left_join(nutsall, regions)%>%
  mutate(Year = year(Date), Month = month(Date), Yearmon = Year + 1/Month, 
         season = case_when(
           Month %in% c(12,1,2) ~ "winter",
           Month %in% c(3,4,5) ~ "spring",
           Month %in% c(6,7,8) ~ "summer",
           Month %in% c(9,10,11) ~ "fall"
         )) 

nutssum = nutswreg %>%
  #filter(Month %in% c(5,6,7,8,9)) %>%
  group_by(Year, season, Region) %>%
  summarize(TKN = mean(TKN, na.rm = T), DissAmmonia = mean(DissAmmonia, na.rm = T),
            DissOrthophos = mean(DissOrthophos, na.rm = T), TotPhos = mean(TotPhos, na.rm = T),
            DissNN = mean(DissNitrateNitrite, na.rm = T), DOC = mean(DOC, na.rm = T))

nutsseason = pivot_longer(nutswreg, 
                        cols = c(TKN, DissAmmonia, DissOrthophos, TotPhos,
                                 DissNitrateNitrite, DOC), names_to = "Analyte",
                        values_to = "Result")


ggplot(filter(nutsseason, Analyte != "DOC", Region != "Suisun Marsh"), aes(x = Year, y = Result, color = Analyte)) + geom_point() + geom_line() +
  facet_grid(Region~season)


##############################################
#Just 2021

nuts2021 = filter(nutswreg, Year == 2021, !is.na(Region)) %>%
  pivot_longer(
               cols = c(TKN, DissAmmonia, DissOrthophos, TotPhos,
                        DissNitrateNitrite), names_to = "Analyte",
               values_to = "Result")

ggplot(nuts2021, aes(x = Region, y = Result)) +
  geom_boxplot()+
  facet_grid(Analyte~season, scales = "free_y")

# south-central levels from the past ten years

nutsrecent = filter(nutswreg, Year > 2013, Region =="SouthCentral") %>%
  pivot_longer(
    cols = c(TKN, DissAmmonia, DissOrthophos, TotPhos,
             DissNitrateNitrite), names_to = "Analyte",
    values_to = "Result") %>%
  mutate(season = factor(season, levels = c("spring", "summer", "fall", "winter")))

ggplot(nutsrecent, aes(x = as.factor(Year), y = Result)) +
  geom_boxplot()+
  facet_grid(Analyte~season, scales = "free_y")

#Uh, that one outlier is really irritating

ggplot(filter(nutsrecent, Result < 10), aes(x = as.factor(Year), y = Result)) +
  geom_boxplot()+
  facet_grid(Analyte~season, scales = "free_y")


#hmmm

nutsrecent2 = filter(nutsrecent, Result < 10) %>%
  mutate(yearmon = Year + Month/12)

ggplot(nutsrecent2, aes(x = Month, y = Result)) +
  geom_point()+ geom_smooth()+
  facet_grid(Analyte~Year, scales = "free_y")


ggplot(nutsrecent2, aes(x = yearmon, y = Result)) +
  geom_point()+ geom_smooth()+
  facet_grid(Analyte~., scales = "free_y")

nutssummary = group_by(nutsrecent2, Year, season, Analyte) %>%
  summarize(Resultm = mean(Result, na.rm = T), SE = sd(Result, na.rm = T)/sqrt(n()), SD = sd(Result, na.rm = T), max = max(Result), min = min(Result)) %>%
  mutate(Analyte = factor(Analyte, levels = c("DissAmmonia", "DissNitrateNitrite", "TKN","DissOrthophos",  "TotPhos"),
                          labels = c("Dissolved\nAmmonia", "Dissolved\nNitrate Nitrite", "Total Kjeldahl\nNitrogen",
                                     "Dissolved \nOrthophosphate", "Total Phosphorus")))

ggplot(nutssummary, aes(x = Year, y = Resultm, fill = Analyte)) + geom_bar(stat = "identity", alpha = 0.5)+
  geom_errorbar(aes(ymin = Resultm - SD, ymax = Resultm + SD)) +
  facet_grid(Analyte~season, scales = "free_y")+ 
  ylab("Concentration (mg/L)")+ theme_bw()


ggplot(nutssummary, aes(x = Year, y = Resultm)) + geom_bar(stat = "identity", alpha = 0.5)+
  geom_errorbar(aes(ymin = min, ymax = max)) +
  facet_grid(Analyte~season, scales = "free_y")
