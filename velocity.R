#merge all station data for pub

library(readr)
library(dplyr)
library(lubridate)
library(DroughtData)
library(glue)

#import 4 data sets

Cache <- read_rds("data-raw/Hydrology/dv_cache.rds")

Jersey <- read_rds("data-raw/Hydrology/dv_jersey.rds")

Middle <- read_rds("data-raw/Hydrology/dv_middle.rds")

Old <- read_rds("data-raw/Hydrology/dv_old.rds")

#round dates down to week

Cache_weekly <- Cache

Cache_weekly$week <- floor_date(Cache_weekly$Date, "week")

Jersey_weekly <- Jersey

Jersey_weekly$week <- floor_date(Jersey_weekly$Date, "week")

Middle_weekly <- Middle

Middle_weekly$week <- floor_date(Middle_weekly$Date, "week")

Old_weekly <- Old

Old_weekly$week <- floor_date(Old_weekly$Date, "week")

#downstep to week
Cache_week <- Cache_weekly%>%
  group_by(week) %>%
  summarize(max_tidal_vel = max(max_tidal),
            min_tidal_vel = min(min_tidal),
            max_net_vel = max(max_net),
            min_net_vel = min(min_net),
            max_abs_tidal = max(max_abs_tidal),
            mean_net_vel = mean(mean_net))

Jersey_week <- Jersey_weekly%>%
  group_by(week) %>%
  summarize(max_tidal_vel = max(max_tidal),
            min_tidal_vel = min(min_tidal),
            max_net_vel = max(max_net),
            min_net_vel = min(min_net),
            max_abs_tidal = max(maxabs_tidal),
            mean_net_vel = mean(mean_net))

Middle_week <- Middle_weekly%>%
  group_by(week) %>%
  summarize(max_tidal_vel = max(max_tidal),
            min_tidal_vel = min(min_tidal),
            max_net_vel = max(max_net),
            min_net_vel = min(min_net),
            max_abs_tidal = max(maxabs_tidal),
            mean_net_vel = mean(mean_net))

Old_week <- Old_weekly%>%
  group_by(week) %>%
  summarize(max_tidal_vel = max(max_tidal),
            min_tidal_vel = min(min_tidal),
            max_net_vel = max(max_net),
            min_net_vel = min(min_net),
            max_abs_tidal = max(maxabs_tidal),
            mean_net_vel = mean(mean_net))

#add station column and sign column back to dfs

Cache_week <- Cache_week %>%
  mutate(net_sign=case_when(abs(min_net_vel) > max_net_vel ~ "-", abs(min_net_vel) < max_net_vel ~ "+"),
         tide_sign=case_when(abs(min_tidal_vel) > max_tidal_vel ~ "-", abs(min_tidal_vel) < max_tidal_vel ~ "+"))

Cache_week$station <- "Cache"

Jersey_week <- Jersey_week %>%
  mutate(net_sign=case_when(abs(min_net_vel) > max_net_vel ~ "-", abs(min_net_vel) < max_net_vel ~ "+"),
         tide_sign=case_when(abs(min_tidal_vel) > max_tidal_vel ~ "-", abs(min_tidal_vel) < max_tidal_vel ~ "+"))

Jersey_week$station <- "Jersey"

Middle_week <- Middle_week %>%
  mutate(net_sign=case_when(abs(min_net_vel) > max_net_vel ~ "-", abs(min_net_vel) < max_net_vel ~ "+"),
         tide_sign=case_when(abs(min_tidal_vel) > max_tidal_vel ~ "-", abs(min_tidal_vel) < max_tidal_vel ~ "+"))

Middle_week$station <- "Middle"

Old_week <- Old_week %>%
  mutate(net_sign=case_when(abs(min_net_vel) > max_net_vel ~ "-", abs(min_net_vel) < max_net_vel ~ "+"),
         tide_sign=case_when(abs(min_tidal_vel) > max_tidal_vel ~ "-", abs(min_tidal_vel) < max_tidal_vel ~ "+"))

Old_week$station <- "Old"

#bind all dfs to one weekly df

vel_weekly_pub <- rbind(Jersey_week, Old_week, Middle_week, Cache_week)

#make raw_hydro into weekly mean

week_hydro <- subset(raw_hydro_1975_2021, Date > as.Date('2007-09-30'))

week_hydro$week <- floor_date(week_hydro$Date, "week")

week_hydro <- week_hydro%>%
  group_by(week) %>%
  summarize(X2 = mean(X2),
            Export = mean(Export),
            Outflow = mean(Outflow),
            Inflow = mean(InflowTotal))

#merge dayflow params and WY type

vel_weekly_pub <- merge(vel_weekly_pub, week_hydro, by = 'week')

lt_seas <- lt_avg_hydro[c(1, 4:5)]

lt_seas <- unique(lt_seas)

#vel_daily_pub <- merge(vel_daily_pub, lt_seas, by = "YearAdj")

vel_weekly_pub <- vel_weekly_pub %>%
  # Add variables for adjusted calendar year and season
  # Adjusted calendar year: December-November, with December of the previous calendar year
  # included with the following year
  mutate(
    Month = month(week),
    YearAdj = if_else(Month == 12, year(week) + 1, year(week)),
    Season = case_when(
      Month %in% 3:5 ~ "Spring",
      Month %in% 6:8 ~ "Summer",
      Month %in% 9:11 ~ "Fall",
      Month %in% c(12, 1, 2) ~ "Winter"))

vel_weekly_pub <- merge(vel_weekly_pub, lt_seas, by = "YearAdj")

vel_weekly_pub$Log_Outflow <- log(vel_weekly_pub$Outflow)

#add wateryear to weekly data frame

wtr_yr <- function(Date, start_month=10) {
  # Convert dates into POSIXlt
  dates.posix = as.POSIXlt(Date)
  # Year offset
  offset = ifelse(dates.posix$mon >= start_month - 1, 1, 0)
  # Water year
  adj.year = dates.posix$year + 1900 + offset
  # Return the water year
  adj.year
}

vel_weekly_WY_pub <- vel_weekly_pub %>%
  mutate(wtr_yr = wtr_yr(week))

vel_weekly_WY_pub <- vel_weekly_WY_pub %>%
  group_by(wtr_yr) %>%
  mutate(wtr_day = (as.integer(difftime(week,ymd(paste0(wtr_yr - 1 ,'-09-30')), units = "days"))))

vel_weekly <- vel_weekly_WY_pub%>%
  #filter(!is.na(mean_vel))%>%
  mutate(across(c(Drought, YearType),
                list(`20_21`=~case_when(YearAdj==2021 ~ "2021",YearAdj==2020 ~ "2020",TRUE ~ as.character(.x)))),
         across(c(YearType, YearType_20_21), ~factor(.x, levels=c("2020", "2021", "Critical", "Dry", "Below Normal", "Above Normal", "Wet"))),
         Season=factor(Season, levels=c("Winter", "Spring", "Summer", "Fall")))

vel_metric <- vel_weekly %>%
  #select(c(1,2,6,7,9,11:24)) %>%
  mutate(max_tidal_vel_m = max_tidal_vel/3.2808399,
         min_tidal_vel_m = min_tidal_vel/3.2808399,
         max_net_vel_m = max_net_vel/3.2808399,
         min_net_vel_m = min_net_vel/3.2808399,
         max_abs_tidal_vel_m = max_abs_tidal/3.2808399,
         mean_net_vel_m = mean_net_vel/3.2808399,
         Export_m = Export/35.315,
         Outflow_m = Outflow/35.315,
         Inflow_m = Inflow/35.315,
         Log_Outflow_m = log(Outflow_m),
         Log_Inflow_m = log(Inflow_m),
         Export_ft = Export,
         Outflow_ft = Outflow,
         Inflow_ft = Inflow,
         Log_Outflow_ft = Log_Outflow,
         Log_Inflow_ft = log(Inflow))

vel_metric <- vel_metric %>%
  select(-c(3:8, 13:15, 20, 36:40))

write_rds(vel_metric, glue("data/velocity.rds"))

