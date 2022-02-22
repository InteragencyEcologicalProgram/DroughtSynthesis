# Emergency Drought Barrier - discrete chlorophyll data
# Purpose: Import, clean, and integrate the 2021 discrete chlorophyll data for
  # OSJ, FAL, and D19 to be used in figures comparing it to the continuous data
  # for the 2021 EDB monitoring report
# Author: Dave Bosworth
# Contact: David.Bosworth@water.ca.gov

# Load packages
library(tidyverse)
library(readxl)
library(lubridate)
library(here)

# Check if we are in the correct working directory
i_am("EDB/d_chla_clean_integrate.R")


# 1. Import Data ----------------------------------------------------------

# Define file path for discrete chlorophyll stored on the EDB Report SharePoint site
edb_abs_sp_path <- function(fp_rel = NULL) {
  fp_edb <- "California Department of Water Resources/DWR-EXT-2021 Emer Drought BarrierReport - General"
  
  if (is.null(fp_rel)) {
    fp_abs <- normalizePath(file.path(Sys.getenv('USERPROFILE'), fp_edb))
  } else {
    fp_abs <- normalizePath(file.path(Sys.getenv('USERPROFILE'), fp_edb, fp_rel))
  }
  
  return(fp_abs)
}

fp_edb_chla <- edb_abs_sp_path("Water Quality/Discrete Data")

# Import discrete chlorophyll data
df_chla_fal_osj_orig <- 
  read_excel(
    file.path(fp_edb_chla, "CD N Drought 2021 Results.xlsx"), 
    sheet = "Sheet1"
  )

df_chla_d19_orig <- 
  read_excel(
    file.path(fp_edb_chla, "EMP_D7D19D22NZ028_March-November 2021 Discrete WQ Data Request.xlsx"), 
    sheet = "Sheet1"
  )


# 2. Clean and Integrate Data ---------------------------------------------

# Clean the discrete chlorophyll data for FAL and OSJ
df_chla_fal_osj_clean <- df_chla_fal_osj_orig %>% 
  select(
    Station = `Station Name`,
    DateTime = `Sample Date`,
    Chla = `Chlorophyll a ug/L Std Method 10200 H 274 [1]*`
  ) %>% 
  filter(Station %in% c("False River near Oakley", "Old River near Frank's Tract")) %>% 
  mutate(
    Station = case_when(
      str_detect(Station, "^False") ~ "FAL",
      str_detect(Station, "^Old") ~ "OSJ"
    ),
    DateTime = mdy_hm(DateTime, tz = "Etc/GMT+8"),
    Date = date(DateTime),
    Chla = as.numeric(Chla)
  ) %>% 
  select(Station, Date, Chla)

# Clean the discrete chlorophyll data for D19
df_chla_d19_clean <- df_chla_d19_orig %>% 
  select(
    Station = `Station Number`,
    Date,
    Chla = `Chlorophyll a ug/L Std Method 10200 H 274 [1]*`
  ) %>% 
  mutate(Date = date(Date)) %>% 
  filter(year(Date) == 2021)

# Combine discrete chlorophyll data for all stations
df_chla_all <- bind_rows(df_chla_fal_osj_clean, df_chla_d19_clean)

# Export Data
df_chla_all %>% write_csv(here("EDB/d_chla_fal_osj_d19.csv"))

