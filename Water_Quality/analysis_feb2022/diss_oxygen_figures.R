# Drought Synthesis - Figures for continuous dissolved oxygen
# Purpose: Create figures of continuous dissolved oxygen data for the February
  # 2022 Drought Synthesis report.
# Author: Liz Stumpner and Dave Bosworth
# Contact: estumpner@usgs.gov, David.Bosworth@water.ca.gov


# Load packages
library(tidyverse)
library(lubridate)
library(DroughtData)
library(here)

# Check if we are in the correct working directory
i_am("Water_Quality/analysis_feb2022/diss_oxygen_figures.R")

# Prepare dissolved oxygen data for plots
cont_do_daily_c <- cont_do_daily %>% 
  # Add placeholder rows for data gaps that are greater than 7 days to prevent
    # interpolation of large data gaps in the figures
  group_by(Station) %>% 
  complete(Date = seq.Date(min(Date), max(Date), by = "day")) %>% 
  arrange(Date) %>% 
  mutate(
    na_val = is.na(AvgDO),
    na_val_run_total = sequence(rle(na_val)$lengths)
  ) %>% 
  filter(!(na_val == TRUE & na_val_run_total < 8)) %>% 
  ungroup() %>%
  select(!starts_with("na_val")) %>% 
  # Add water year and water year day to dissolved oxygen data
  mutate(
    Year = year(Date),
    WtrYear = if_else(month(Date) %in% 10:12, Year + 1, Year),
    WtrYearDay = as.integer(difftime(Date, ymd(paste0(WtrYear - 1 ,'-09-30')), units = "days"))
  )

# Plot entire DO time series (2014-2021) for both stations: Sacramento River
  # near Decker Island (DEC) and Yolo Bypass Toe Drain (TOE)
plt_do_dec_toe <- cont_do_daily_c %>%
  ggplot(aes(x = Date, y = AvgDO, color = Station)) +
  geom_line(alpha = 0.6) +
  labs(x = "Date", y = "Mean DO (mg/L)") +
  scale_color_brewer(palette = "Dark2") +
  theme_bw()

plt_do_dec_toe

# Export figure
ggsave(
  plot = plt_do_dec_toe,
  filename = here("Water_Quality/DissOxy_DEC_TOE_ts.png"),
  height = 5.5,
  width = 6.5,
  units = "in"
)
  
# Plot DO data by Water Year for the Sacramento River near Decker Island stations (DEC)
plt_do_wy_dec <- cont_do_daily_c %>%
  filter(Station == "DEC") %>%
  mutate(WtrYear = as.character(WtrYear)) %>%
  ggplot(aes(x = WtrYearDay, y = AvgDO, color = WtrYear)) +
  geom_line() +
  labs(x = "WY Day", y = "Mean DO (mg/L)") +
  scale_color_brewer(name = "Water Year", palette = "BuGn") +
  theme_bw()

plt_do_wy_dec

# Export figure
ggsave(
  plot = plt_do_wy_dec,
  filename = here("Water_Quality/DissOxy_DEC_WY_ts.png"),
  height = 5.5,
  width = 6.5,
  units = "in"
)

# Plot DO data by Water Year for the Yolo Bypass Toe Drain stations (TOE)
plt_do_wy_toe <- cont_do_daily_c %>%
  filter(Station == "TOE") %>%
  mutate(WtrYear = as.character(WtrYear)) %>%
  ggplot(aes(x = WtrYearDay, y = AvgDO, color = WtrYear)) +
  geom_line() +
  labs(x = "WY Day", y = "Mean DO (mg/L)") +
  scale_color_brewer(name = "Water Year", palette = "YlOrRd") +
  theme_bw()

plt_do_wy_toe

# Export figure
ggsave(
  plot = plt_do_wy_toe,
  filename = here("Water_Quality/DissOxy_TOE_WY_ts.png"),
  height = 5.5,
  width = 6.5,
  units = "in"
)

# All exported figures were moved to the Drought Synthesis SharePoint site

