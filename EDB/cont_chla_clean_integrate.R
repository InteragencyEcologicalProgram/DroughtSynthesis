# Emergency Drought Barrier - continuous chlorophyll data
# Purpose: Import, clean, integrate, and calculate daily averages and medians of
  # the continuous chlorophyll data for the 2021 EDB monitoring report
# Author: Dave Bosworth
# Contact: David.Bosworth@water.ca.gov

# Load packages
library(tidyverse)
library(dataRetrieval)
library(lubridate)
library(plotly)
library(ODWGtools)
library(patchwork)
library(sf)
library(readxl)

# Set System Timezone as "Etc/GMT+8" (PST) to make it consistent with all df's 
Sys.setenv(TZ = "Etc/GMT+8")


# 1. Import Data ----------------------------------------------------------

# Define file path for continuous chlorophyll stored on the EDB Report SharePoint site
edb_abs_sp_path <- function(fp_rel = NULL) {
  fp_edb <- "California Department of Water Resources/DWR-EXT-2021 Emer Drought BarrierReport - General"
  
  if (is.null(fp_rel)) {
    fp_abs <- normalizePath(file.path(Sys.getenv('USERPROFILE'), fp_edb))
  } else {
    fp_abs <- normalizePath(file.path(Sys.getenv('USERPROFILE'), fp_edb, fp_rel))
  }
  
  return(fp_abs)
}

fp_edb_chla <- edb_abs_sp_path("Water Quality/Chlorophyll")

# Download and save continuous chlorophyll collected by USGS
# Set download to TRUE if need to download USGS continuous chlorophyll data
download <- FALSE

# Download USGS continuous chlorophyll data to the EDB Report SharePoint if necessary
if (download == TRUE) {
  # Create vectors for parameters, start and end dates
  start_date <- "2020-01-01" 
  end_date <- "2021-12-31" 
  params <- "32316"  # Chlorophyll concentration estimated from reference material (ug/L)
  
  # Download data for each station individually since doing it all at once doesn't work correctly
  LIB <- readNWISuv("11455315", params, start_date, end_date, tz = "Etc/GMT+8")
  MDM <- readNWISuv("11312676", params, start_date, end_date, tz = "Etc/GMT+8")
  RYF <- readNWISuv("11455385", params, start_date, end_date, tz = "Etc/GMT+8")
  SJJ <- readNWISuv("11337190", params, start_date, end_date, tz = "Etc/GMT+8")
  
  # Export raw data as .csv files for each site
  lst(LIB, MDM, RYF, SJJ) %>% 
    map(as_tibble) %>% 
    iwalk(
      .f = ~write_csv(
        .x, 
        file = file.path(fp_edb_chla, paste0(.y, "_Chlor_2020-2021.csv")),
        na = ""
      )
    )
}

# Create a vector of all file paths for the continuous chlorophyll data
dir_chla <- sort(dir(fp_edb_chla, full.names = T))

# Remove FAL (2020 and 2021), HLT (2020), and OSJ (2020) since their formats are
  # so different and need to be imported separately
dir_chla2 <- str_subset(dir_chla, "FAL_|HLT_.+2020|OSJ_.+2020", negate = TRUE)
dir_chla_fal_hlt_osj2020 <- str_subset(dir_chla, "FAL_.+2020|HLT_.+2020|OSJ_.+2020")
dir_chla_fal2021 <- str_subset(dir_chla, "FAL_.+2021")

# Import continuous chlorophyll data from dir_chla2 into a nested dataframe
ndf_chla_orig1 <- 
  tibble(
    Station = map_chr(dir_chla2, ~str_sub(.x, start = 136, end = 138)),
    df_data = map(dir_chla2, ~read_csv(.x, col_types = cols(.default = "c")))
  )

# Function to import the csv files with different formats
import_csv_diff <- function(dir_csv, skip_num) {
  read_csv(
    file = dir_csv,
    col_names = c("DateTime", "value", "Quality"), 
    skip = skip_num,
    col_types = cols(.default = "c")
  )
}

# Import continuous chlorophyll data for FAL (2020), HLT (2020), and OSJ (2020)
ndf_chla_fal_hlt_osj2020 <- 
  tibble(
    Station = map_chr(dir_chla_fal_hlt_osj2020, ~str_sub(.x, start = 136, end = 138)),
    df_data = map(dir_chla_fal_hlt_osj2020, ~import_csv_diff(.x, skip_num = 9))
  )

# Import continuous chlorophyll data for FAL (2021)
ndf_chla_fal2021 <- 
  tibble(
    Station = map_chr(dir_chla_fal2021, ~str_sub(.x, start = 136, end = 138)),
    df_data = map(dir_chla_fal2021, ~import_csv_diff(.x, skip_num = 3))
  )

# Combine all nested dataframes together
ndf_chla_orig2 <- 
  bind_rows(ndf_chla_orig1, ndf_chla_fal_hlt_osj2020, ndf_chla_fal2021) %>% 
  arrange(Station)

# Import coordinates for stations
df_coords_orig <- 
  read_excel(
    edb_abs_sp_path("Water Quality/Station Maps and Coordinates/Table1_EDBStationCoordinates.xlsx"),
    sheet = "Sheet1"
  )

# Import the polygon shapefile for the EDB regions
sf_edb_reg <- read_sf("EDB/Spatial_data/EDB_Regions.shp")


# 2. Clean and Integrate Data ---------------------------------------------

# Inspect structure of each dataframe
for (i in 1:nrow(ndf_chla_orig2)) {
  print(ndf_chla_orig2$Station[i])
  glimpse(ndf_chla_orig2$df_data[[i]])
}

# Print variable names of each dataframe
for (i in 1:nrow(ndf_chla_orig2)) {
  print(ndf_chla_orig2$Station[i])
  print(names(ndf_chla_orig2$df_data[[i]]))
}

# Create a vector of keywords for the variable names to keep in the dataframes
vec_vars_keep <- 
  c(
    "Date",
    "time",
    "value",
    "32316_00000",
    "Quality",
    "qaqc",
    "Flag"
  )

# Start with some general formatting and cleaning
df_chla_clean1 <- ndf_chla_orig2 %>% 
  # Standardize variable names
  mutate(
    df_data = map(
      df_data, 
      ~select(.x, contains(vec_vars_keep)) %>% 
        rename(DateTime = contains(c("date", "time"), ignore.case = FALSE)) %>% 
        rename(Chla = matches("value|32316_00000$")) %>% 
        rename(Qual = matches("Qual|Flag|qaqc|_cd$"))
    )
  ) %>% 
  # Unnest nested dataframes now that all variable names are standardized
  unnest(df_data) %>% 
  # Convert variable types making them new variables to check for parsing errors
  mutate(
    DateTime2 = parse_date_time(DateTime, orders = c("Ymd T", "mdY T", "mdY R"), tz = "Etc/GMT+8"),
    Chla2 = as.numeric(Chla)
  )

# Check for parsing errors in DateTime and Chla
anyNA(df_chla_clean1$DateTime)
anyNA(df_chla_clean1$DateTime2)
df_chla_test1 <- df_chla_clean1 %>% filter(is.na(Chla))
df_chla_test2 <- df_chla_clean1 %>% filter(is.na(Chla2))
identical(df_chla_test1, df_chla_test2)
# No parsing errors identified

# Finish with general formatting and cleaning
df_chla_clean2 <- df_chla_clean1 %>% 
  select(-c(DateTime, Chla)) %>% 
  rename(
    DateTime = DateTime2,
    Chla = Chla2
  ) %>% 
  # Add Year variable
  mutate(Year = year(DateTime)) %>% 
  # Remove NA values and records with Qual code of "X" (Bad data)
  filter(!is.na(Chla)) %>% 
  filter(Qual != "X" | is.na(Qual))


# 3. Run Quality Checks ---------------------------------------------------

# Look for duplicated time stamps
df_chla_clean2 %>%
  mutate(DateTime = round_date(DateTime, unit = "15 minute")) %>%
  count(Station, DateTime) %>%
  filter(n > 1)
# No duplicated time stamps present in data set

# Check out periods of record for each station
df_chla_clean2 %>% 
  count(Station, Year) %>% 
  arrange(Year) %>% 
  pivot_wider(names_from = Year, values_from = n) %>%
  arrange(Station)

# Look at min and max values for each station
qc_min_max <- df_chla_clean2 %>% 
  group_by(Station, Year) %>% 
  summarize(
    min_value = min(Chla),
    max_value = max(Chla)
  ) %>% 
  ungroup()

View(qc_min_max)
# 4 stations have some values equal to zero, no values less than zero
# 3 stations have some values greater than 100, should investigate these further

# Function to create basic interactive line plots of a single station's time-series data
plot_interactive_ts <- function(df, sta_code) {
  plt <- df %>% 
    filter(Station == sta_code) %>% 
    ggplot(aes(x = DateTime, y = Chla)) +
    geom_line()
  
  ggplotly(plt)
}

# BLP:
plot_interactive_ts(df_chla_clean2, "BLP")
# Data is noisy at times, could benefit from some outlier screening

# ORI:
plot_interactive_ts(df_chla_clean2, "ORI")
# Need to remove four points on 8/10/2018 equal to 499.99 at the edge of a large data gap
# Data is also noisy at times, could benefit from some outlier screening

# WCI
plot_interactive_ts(df_chla_clean2, "WCI")
# Need to remove two points on 2/14/2019 equal to 499.99
# Data is also noisy at times, could benefit from some outlier screening

# Remove six points equal to 499.99 from the data set
df_chla_clean3 <- df_chla_clean2 %>% filter(Chla < 400)

# Run a few outlier screening tests
qc_outlier_screen <- df_chla_clean3 %>% 
  # Round DateTime to nearest 15-minute interval and fill in missing time stamps
  mutate(DateTime = round_date(DateTime, unit = "15 minute")) %>% 
  group_by(Station, Year) %>% 
  complete(DateTime = seq.POSIXt(min(DateTime), max(DateTime), by = "15 min")) %>% 
  mutate(
    # Spike test - fixed thresholds c(15, 30)
    qual_spike = rtqc_spike(Chla, c(15, 30)),
    # Rate of change test (using 3 standard deviations, SD is of the prior 25-hour period)
    qual_rate = rtqc_rate(Chla, 3, 100),
    # Flag if both tests are suspect or fail and value is greater than 50 ug/L
    qual_final = if_else(
      qual_spike %in% c("suspect", "fail") & qual_rate %in% c("suspect", "fail") & Chla > 50,
      "fail",
      "pass"
    )
  ) %>% 
  ungroup()

# Visualize results of outlier screening tests
# Remove NA values and flagged values in qual_final
qc_outlier_screen2 <- qc_outlier_screen %>% filter(!is.na(Chla), qual_final != "fail")

# Which stations had flagged values in qual_final?
qc_outlier_screen %>% 
  filter(!is.na(Chla), qual_final == "fail") %>% 
  distinct(Station)

# Function to visualize results of outlier screening tests
vis_outl_res <- function(sta_code) {
  yaxis_val <- df_chla_clean3 %>% 
    filter(Station == sta_code) %>% 
    summarize(
      min_val = min(Chla),
      max_val = max(Chla)
    )
  
  plt_orig <- df_chla_clean3 %>% 
    filter(Station == sta_code) %>% 
    ggplot(aes(x = DateTime, y = Chla)) +
    geom_line() +
    ggtitle(paste0("Original Data - ", sta_code)) +
    scale_y_continuous(limits = c(yaxis_val$min_val, yaxis_val$max_val))
  
  plt_outl_rm <- qc_outlier_screen2 %>% 
    filter(Station == sta_code) %>% 
    ggplot(aes(x = DateTime, y = Chla)) +
    geom_line() +
    ggtitle(paste0("Outliers removed - ", sta_code)) +
    scale_y_continuous(limits = c(yaxis_val$min_val, yaxis_val$max_val))
  
  plt_orig / plt_outl_rm
}

# Visualize results of outlier screening tests for BLP, FAL, MIR, and WCI
tmp_vis_outl_res("BLP")
tmp_vis_outl_res("FAL")
tmp_vis_outl_res("MIR")
tmp_vis_outl_res("WCI")
# The outlier screening tests didn't perform as well as I preferred - 
  # may use daily medians to visualize trends in continuous chlorophyll data
  # instead of daily means


# 4. Aggregate Values -----------------------------------------------------

# Assign EDB regions to each station
df_region_assign <- df_coords_orig %>% 
  select(
    Station = `CDEC code`,
    Latitude,
    Longitude,
    Latitude_rev = `Revised WGS84`,
    Longitude_rev = ...6
  ) %>% 
  mutate(
    Latitude = if_else(is.na(Latitude_rev), Latitude, Latitude_rev),
    Longitude = if_else(is.na(Longitude_rev), Longitude, Longitude_rev)
  ) %>% 
  select(!ends_with("_rev")) %>% 
  # Convert to sf object
  st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326) %>% 
  st_join(st_make_valid(sf_edb_reg), join = st_intersects) %>% 
  # Drop sf geometry column since it's no longer needed
  st_drop_geometry() %>% 
  select(-notes) %>% 
  # Assign "Outside" to stations without region assignments
  replace_na(list(Regions = "Outside")) %>% 
  rename(Region = Regions)

# Calculate daily means and medians of continuous chlorophyll data for each station
df_chla_aggr <- df_chla_clean3 %>% 
  mutate(Date = date(DateTime)) %>% 
  group_by(Station, Year, Date) %>% 
  summarize(
    Chla_avg = mean(Chla),
    Chla_med = median(Chla)
  ) %>% 
  ungroup() %>% 
  # Add regions
  left_join(df_region_assign, by = "Station")

# Check if there are any NA values to see if join worked correctly
anyNA(df_chla_aggr)
# Looks good! It's ready to be exported

# Export Data
df_chla_aggr %>% write_csv("EDB/cont_chla_daily_values.csv")

