# Chinook Drought Response ----

# (possibly applicable to Threadfin Shad, American Shad, Delta Smelt,
# and Longfin Smelt, Striped Bass (Age0) too)

# working with 1976-2020 data (data are incomplete for 2021)
# from EDI Data Portal, downloaded ca Nov 11, 2021
# https://portal.edirepository.org/nis/metadataviewer?packageid=edi.244.8

# Interagency Ecological Program: Over four decades of juvenile fish monitoring
# data from the San Francisco Estuary, collected by the Delta Juvenile Fish 
# Monitoring Program, 1976-2021

# DJFMP beach seine data -----
# https://portal.edirepository.org/nis/dataviewer?packageid=edi.244.8&entityid=147fd5e2c7db15913b2ffa44410dc7f9

# prep -----

rm(list = ls(all.names = TRUE)) # clear R environment
gc()

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

# data & set-up -----

yeartypes <- read_csv("yearassignments.csv")

#DJFMP <- read_csv("C:/Users/pnelson/OneDrive - California Department of Water Resources/3-Projects/05-drought monitoring/data/Salmon/1976-2021_DJFMP_beach_seine_fish_and_water_quality_data.csv")
# warnings limited to 352 records where "Expressions" were not as expected: 
# ignore, unimportant

# 'massage' raw data; tweak variable types, add year_wk and unique set identifier
djfmp <- DJFMP %>% 
  transmute(date = mdy(SampleDate),
            time = chron(times = SampleTime),
            date_time = as_datetime(paste(date, time),
                                    tz = "US/Pacific"),
            count = Count,
            stage = StageCode,
            fl = ForkLength,
            race_tag = RaceByTag,
            location = as_factor(Location),
            region = as_factor(RegionCode),
            station = as_factor(StationCode),
            weather = as_factor(WeatherCode),
            race_fl = as_factor(RaceByLength),
            race_tag = as_factor(RaceByTag),
            temp = WaterTemp,
            volume = Volume,
            sp = as_factor(OrganismCode)) %>% 
  mutate(year_wk = yearweek(date),
         setID = paste(date, station))

print(djfmp, width = Inf) # check...
# note that 'year' is based on calendar year; added water year below

## data manipulation ----
# chn totals for sets with chn (no chn=0 sets)
chn_sets <- 
  djfmp %>% 
  group_by(year_wk, setID, sp) %>% 
  summarise(count_by_set = sum(count)) %>% 
  filter(sp == "CHN")

# unique sets
sets <- 
  djfmp[, c(3, 17, 4, 16, 8:11)] %>% 
  distinct(setID, .keep_all = TRUE)

# join all sets w chn set totals
chn1 <- 
  left_join(sets, chn_sets[,2:4], by = "setID")

# replace count_by_set NAs w '0'
chn1$count_by_set <- replace_na(chn1$count_by_set, 0)

## regularize ####
# convert df of sets (w chn counts) to tsibble (time series)
# compute weekly number of chn and sets; use these to compute weekly cpue
# add water year (Oct-Sep)
chn2 <- tsibble(chn1, index = date_time, key = setID)

chn2 <- chn2 %>% 
  index_by(year_wk = year_wk) %>%
  summarise(chn_wk = sum(count_by_set, na.rm = TRUE),
            sets_wk = n_distinct(setID)) %>% # effort = number of sets per week
  fill_gaps(chn_wk = NA, sets_wk = NA) %>%   # regularize the time series
  mutate(cpue = chn_wk/sets_wk,              # calc cpue catch/effort
         Date = date(year_wk))               # temporary: allows addWaterYear()

chn2 <- chn2 %>% addWaterYear()

## calc wkly cpue ####
# note: 'year' in chn3 refers to water year!
chn3 <- 
  chn2 %>% 
  mutate(day1 = Date,
         cpue_wk = chn_wk/sets_wk,
         year = as_factor(waterYear)) %>% # temp use of 'year'...
  select(year, year_wk, day1, chn_wk, sets_wk, cpue_wk)

## add yeartypes ####

yeartypes <- yeartypes %>% 
  mutate(year = as_factor(Year),
         index = Index,
         yr_type = as_factor(Yr_type),
         drought = as_factor(Drought),
         short_term = as_factor(ShortTerm),
         sprndoi = SprNDOI,
         .keep = "unused")
yeartypes2 <- yeartypes %>% 
  mutate(water_yr = year)

chn4 <- left_join(as_tibble(chn3), # all rows in y for each matching row in x
                  yeartypes,
                  by = "year") %>% 
  mutate(water_yr = year) # restore proper variable name

chn4 <- chn4 %>%
  mutate(year = as_factor(year(day1))) # convert to calendar year (df has both)

## cumulative totals ####
### cpue ####
chn5 <- 
  chn4 %>% 
  group_by(water_yr) %>% 
  mutate(cum_chn = cumsum(replace_na(chn_wk, 0)),
         cum_set = cumsum(replace_na(sets_wk, 0)),
         wk_cpue = replace_na(cum_chn/cum_set, 0),
         wy_wk = seq(1:n()),
         std_chn = replace_na(cum_chn/last(cum_chn)),
         cum_cpue = replace_na(cum_chn/last(cum_set)),
         std_cpue = replace_na(cum_chn/last(cum_set)/max(cum_cpue))) 
# cum_chn & cum_sets now continuous through all weeks for each water year
# is the date for each year's max cum_cpue a reflection of env conditions?
# how about the magnitude of each year's max cum_cpue??

# rearrange variables & correct some numbers...
chn5 <- chn5[,c(1, 12, 2, 16, 3:6, 13:15, 17:19, 7:11)]
chn5[1:30,4] <- c(23:52) # assign correct water-year week number to first records

# re-order factors for plotting
chn5$yr_type <- 
  factor(chn5$yr_type,
         levels = c("Critical", "Dry", "Below Normal", "Above Normal", "Wet"))

# preserve backup object
chn <- chn5
write_csv(chn5, "chn5_backup.csv")

## annual totals #####
# for both chn and sets

chn_ann1 <-
  chn %>% 
  group_by(water_yr) %>% 
  summarise(chn_ann = max(cum_chn),
            set_ann = max(cum_set),
            cpue_ann = last(wk_cpue),
            max_cpue = max(wk_cpue))

chn_ann <- 
  left_join(chn_ann1, yeartypes2, by = "water_yr", .keep = "none")

chn_ann$yr_type <- 
  factor(chn_ann$yr_type,
         levels = c("Critical", "Dry", "Below Normal", "Above Normal", "Wet"))

chn_ann_backup <- chn_ann

write_csv(chn_ann_backup, "chn_ann_backup.csv")

# figures ####

## time series ####
p <- ggplot(chn %>% 
              filter(day1 < "2021-01-01", # 2021 doesn't have year type...
                     !is.na(cpue_wk)), 
            aes(x = year_wk, y = cpue_wk)) +
  geom_line(color = "steelblue") +
  geom_point() +
  theme_bw() +
  xlab("") +
  ylab("CPUE (# CHN/set each week)") +
  scale_color_discrete(name = "year type",
                       labels = c("dry", "neutral", "wet"))
p + geom_point(aes(color = factor(drought)))

## yearly accumulations ####
### raw counts ####
# not controlled for effort
ggplot(chn) +
  aes(x = wy_wk, y = cum_chn, group = water_yr) +
  xlim(0, 52) + # there are 53 weeks in some years!
  geom_line(aes(color = drought), size = 1.3) +
  xlab("Water-Year Week (Oct - Sep)") +
  ylab("Cumulative Catch (raw counts, CHN)") +
  theme_bw() +
  scale_color_manual(values = c("#F8766D", "#7CAE00", "#00BFC4"),
                     name = "year type",
                     labels = c("Dry", "Neutral", "Wet", "no data"))
# there are no missing values in wy_wk or cum_chn; don't know why the warning

### std chn ####
# accumulated counts of chn standardized by the total chn for each year
ggplot(chn) +
  aes(x = wy_wk, y = std_chn, group = water_yr) +
  xlim(0, 52) + # there are 53 weeks in some years!
  geom_line(aes(color = drought), size = 1.3) +
  xlab("Water-Year Week (Oct - Sep)") +
  ylab("Standardized Cumulative CHN (# weekly CHN/total CHN)") +
  theme_bw() +
  scale_color_manual(values = c("#F8766D", "#7CAE00", "#00BFC4"),
                     name = "year type",
                     labels = c("Dry", "Neutral", "Wet", "no data"))

### cum cpue ####
# weekly CHN counts relative to total annual effort (# of sets)
ggplot(chn) +
  aes(x = wy_wk, y = cum_cpue, group = water_yr) +
  xlim(0, 52) + # there are 53 weeks in some years!
  geom_line(aes(color = drought), size = 1.3) +
  xlab("Water-Year Week (Oct - Sep)") +
  ylab("Cumulative CPUE (# CHN/total sets)") +
  theme_bw() +
  scale_color_manual(values = c("#F8766D", "#7CAE00", "#00BFC4"),
                     name = "year type",
                     labels = c("Dry", "Neutral", "Wet", "no data"))
# there are no missing values in wy_wk or cum_cpue; don't know why the warning

### std cpue #####
ggplot(chn) +
  aes(x = wy_wk, y = std_cpue, group = water_yr) +
  xlim(0, 52) + # there are 53 weeks in some years!
  geom_line(aes(color = drought), size = 1.3) +
  xlab("Water-Year Week (Oct - Sep)") +
  ylab("Standardized Cumulative CPUE (weekly CPUE/max CPUE)") +
  theme_bw() +
  scale_color_manual(values = c("#F8766D", "#7CAE00", "#00BFC4"),
                     name = "year type",
                     labels = c("Dry", "Neutral", "Wet", "no data"))

#############################################################################
#What about the trawl data?

Trawl1 = read_csv("data/1976-2001_DJFMP_trawl_fish_and_water_quality_data.csv")
Trawl2 = read_csv("data/2002-2021_DJFMP_trawl_fish_and_water_quality_data.csv")
Trawls = bind_rows(Trawl1, Trawl2)%>%
  select(Location, StationCode, SampleDate, MethodCode, TowNumber, TowDuration, Volume, IEPFishCode, Count)
TrawlsCH = pivot_wider(Trawls, names_from = IEPFishCode, values_from = Count, values_fn = sum, values_fill = 0) %>%
  select(Location, StationCode, SampleDate, MethodCode, TowNumber, TowDuration, Volume, CHISAL) %>% 
  mutate(year_wk = yearweek(SampleDate),
         setID = paste(SampleDate, StationCode),
         CPUE = CHISAL/Volume)
#calculate cummulative CPUE for Chipps
Chipps = filter(TrawlsCH, Location == "Chipps Island") %>%
  arrange(SampleDate) %>%
mutate(Year = year(SampleDate), julian = yday(SampleDate), wyjulian = case_when(
  julian > 274 ~ julian -274,
  TRUE ~ julian +91
))%>%
  rename(Date = SampleDate) %>%
  addWaterYear() %>%
  group_by(waterYear) %>%
    mutate(cumcatch = cumsum(CHISAL), cumcpue = cumsum(CPUE), 
           percentcatch = cumcatch/max(cumcatch, na.rm = T)) 

ggplot(Chipps, aes(x=wyjulian, y = cumcatch, group = waterYear)) + geom_line()+
  coord_cartesian(xlim = c(0,320))


ggplot(Chipps, aes(x=wyjulian, y = percentcatch, group = waterYear, color = as.factor(waterYear))) + geom_line()+
  coord_cartesian(xlim = c(0,320))+
  ylab("Percent of Annual Chinook Salmon Catch")+
  xlab("day of water year")

Chipps50 = Chipps %>%
filter(percentcatch >= 0.5) %>% 
  group_by(waterYear) %>% 
  summarise(a50_day = min(wyjulian))

chipdate2 = left_join(Chipps50, WYs, by = c("waterYear" = "Year"))
ggplot(chipdate2, aes(x=Drought, y = a50_day)) + geom_boxplot()

ggplot(chipdate2, aes(x=Yr_type, y = a50_day)) + geom_boxplot()

ggplot(chipdate2, aes(x= Index, y = a50_day)) + geom_point() + geom_smooth(method = "lm")



ggplot(filter(Chipps, waterYear>2013), aes(x=wyjulian, y = percentcatch, group = waterYear, color = as.factor(waterYear))) + geom_line()+
  coord_cartesian(xlim = c(0,320))+
  ylab("Percent of Annual Chinook Salmon Catch")+
  xlab("day of water year")


####################################################
#sherwood harbor

#calculate cummulative CPUE for Chipps
swh = filter(TrawlsCH, Location == "Sherwood Harbor") %>%
  arrange(SampleDate) %>%
  mutate(Year = year(SampleDate), julian = yday(SampleDate), wyjulian = case_when(
    julian > 274 ~ julian -274,
    TRUE ~ julian +91
  ))%>%
  rename(Date = SampleDate) %>%
  addWaterYear() %>%
  group_by(waterYear) %>%
  mutate(cumcatch = cumsum(CHISAL), cumcpue = cumsum(CPUE), 
         percentcatch = cumcatch/max(cumcatch, na.rm = T)) 

ggplot(swh, aes(x=wyjulian, y = cumcatch, group = waterYear)) + geom_line()+
  coord_cartesian(xlim = c(0,320))


ggplot(swh, aes(x=wyjulian, y = percentcatch, group = waterYear, color = as.factor(waterYear))) + geom_line()+
  coord_cartesian(xlim = c(0,320))+
  ylab("Percent of Annual Chinook Salmon Catch")+
  xlab("day of water year")


ggplot(filter(swh, waterYear>2013), aes(x=wyjulian, y = percentcatch, group = waterYear, color = as.factor(waterYear))) + geom_line()+
  coord_cartesian(xlim = c(0,320))+
  ylab("Percent of Annual Chinook Salmon Catch")+
  xlab("day of water year")

Test = data.frame(Julian = 1:365, wyJulian = NA)
Test = mutate(Test, wyjulian = case_when(
                  Julian > 275 ~ Julian -273,
                  TRUE ~ Julian +90
                ))
