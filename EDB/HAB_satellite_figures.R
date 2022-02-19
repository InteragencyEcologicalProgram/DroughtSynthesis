# Emergency Drought Barrier - HAB satellite data
# Purpose: Create figures of the HAB satellite data for the EDB report:
  # 1) Area plot of pixel counts of Cyano Index categories for Franks Tract and
    # Mildred Island in 2020 and 2021
  # 2) A few maps of the HAB satellite data for a couple of days of interest in 2021
# Author: Dave Bosworth
# Contact: David.Bosworth@water.ca.gov


# 1. Global Code ----------------------------------------------------------

# Load packages
library(tidyverse)
library(lubridate)
library(scales)
library(EDBdata)
library(sf)
library(deltamapr)
library(curl)
library(glue)
library(stars)
library(patchwork)

# Create a vector for the factor labels of the Cyano Index categories
ci_cat_labels <-
  c(
    "Non Detect", 
    "Low", 
    "Moderate", 
    "High", 
    "Very High"
  )


# 2. Area Plot ------------------------------------------------------------

# 2.1 Prepare Count Data --------------------------------------------------

# Create a vector for the factor levels of the Cyano Index categories
ci_cat_levels <-
  c(
    "Non_detect", 
    "Low", 
    "Moderate", 
    "High", 
    "Very_high"
  )

# Prepare HAB satellite data for stacked area plot
df_hab_sat_plt <- hab_sat_fr_mil %>%
  # Restructure data to long format
  select(-Invalid_or_missing) %>% 
  pivot_longer(
    cols = Non_detect:Very_high,
    names_to = "CIcategory",
    values_to = "CIcount"
  ) %>% 
  # Restrict data to Jun-Oct for both years
  filter(month(Date) >= 6 & month(Date) <= 10) %>% 
  # Add a variable for year and apply factor order to the Cyano Index categories
  mutate(
    CIcategory = factor(CIcategory, levels = ci_cat_levels, labels = ci_cat_labels),
    Year = year(Date)
  ) %>% 
  # Add placeholder rows for data gaps that are greater than 7 days to prevent
    # interpolation of large data gaps in the plot
  group_by(Year, Name, CIcategory) %>% 
  complete(Date = seq.Date(min(Date), max(Date), by = "day")) %>% 
  arrange(Date) %>% 
  mutate(
    na_val = is.na(CIcount),
    na_val_run_total = sequence(rle(na_val)$lengths)
  ) %>% 
  filter(!(na_val == TRUE & na_val_run_total < 8)) %>% 
  ungroup() %>% 
  select(!starts_with("na_val")) %>% 
  replace_na(list(CIcount = 0))

# 2.2 Create Area Plot ----------------------------------------------------

plt_hab_sat <- df_hab_sat_plt %>% 
  ggplot(aes(x = Date, y = CIcount, fill = CIcategory)) +
  geom_area(position = "fill") +
  facet_grid(
    rows = vars(Name),
    cols = vars(Year),
    scales = "free"
  ) +
  scale_x_date(
    name = "Date",
    breaks = breaks_pretty(5),
    labels = label_date_short(c(NA, "%b", "%d", NA))
  ) +
  scale_y_continuous(
    name = "Percent of valid pixels within each Cyano Index Category",
    labels = percent_format()
  ) +
  scale_fill_viridis_d(
    name = "Cyano Index Category",
    option = "plasma",
    end = 0.95
  ) +
  theme_light() +
  theme(
    strip.text = element_text(color = "black"),
    legend.position = "top"
  )

# Export plot as a .jpg
ggsave(
  "EDB/CI_category_area_plot.jpg",
  plot = plt_hab_sat,
  width = 6.5,
  height = 5.5,
  units = "in"
)


# 3. HAB Satellite Data Map -----------------------------------------------

# 3.1 Prepare Spatial Data ------------------------------------------------

# Download HAB satellite data for July and August 2021:
# Set download to TRUE if need to download harmful algal bloom (HAB) satellite data
download <- FALSE

# Download HAB satellite data to local computer if necessary
if (download == TRUE) {
  # Define subfolder directory to store .tif files
  dir_hab_sat <- "EDB/Spatial_data"
  
  # Function to download and unzip harmful algal bloom (HAB) satellite data (cyanobacteria abundance)
    # from the https://fhab.sfei.org/ website
  download_hab <- function(hab_yr, hab_month) {
    hab_url <- glue("https://fhab.sfei.org/lib/download.php?request=download&dltype=month&year={hab_yr}&month={hab_month}&product=Mosaic")
    out_path <- file.path(dir_hab_sat, glue("mosaic_{hab_yr}_{hab_month}.zip"))
    
    curl_download(hab_url, out_path)
    unzip(out_path, exdir = dir_hab_sat)
    Sys.sleep(5)
  }
  
  # Download data for July and August 2021
  hab_2021 <- c(7, 8)
  for (i in hab_2021) {download_hab(2021, i)}
  
  # Remove .zip files
  invisible(file.remove(dir(path = dir_hab_sat, pattern = "zip$", full.names = TRUE)))
}

# Define file path that contains HAB satellite data
fp_hab_sat <- "EDB/Spatial_data"

# Import HAB satellite data for the beginning, peak, and end of the cyano bloom in Franks Tract in 2021:
# Create a nested data frame to prepare the HAB satellite data for maps
df_hab_sat <-
  tibble(
    date_chr = c("July 14, 2021", "July 29, 2021", "Aug 14, 2021"),
    fp = c(
      file.path(fp_hab_sat, "sentinel-3a.2021195.0714.L3.CA_mosaic.v950V20193_1_2.CIcyano.tif"),
      file.path(fp_hab_sat, "sentinel-3a.2021210.0729.L3.CA_mosaic.v950V20193_1_2.CIcyano.tif"),
      file.path(fp_hab_sat, "sentinel-3a.2021226.0814.L3.CA_mosaic.v950V20193_1_2.CIcyano.tif")
    ),
    strs_prx_obj = map(fp, read_stars, proxy = TRUE)
  ) %>%
  select(-fp)

# Import shapefile for Franks Tract and Mildred Island
sf_franks_mildred <- read_sf("EDB/Spatial_data/Franks_Mildred.shp")

# Transform crs of Franks-Mildred and WW_Delta shapefiles to the crs of the HAB satellite data
crs_hab_sat <- st_crs(df_hab_sat$strs_prx_obj[[1]])
sf_franks_mildred_32611 <- st_transform(sf_franks_mildred, crs = crs_hab_sat)
WW_Delta_32611 <- st_transform(WW_Delta, crs = crs_hab_sat)

# Create a bounding box of the Franks-Mildred shapefile which will be used to crop the satellite data
  # and extend it by 5% on each side
bbox_fr_mil <- st_bbox(sf_franks_mildred_32611)
bbox_fr_mil_xrange <- bbox_fr_mil$xmax - bbox_fr_mil$xmin
bbox_fr_mil_yrange <- bbox_fr_mil$ymax - bbox_fr_mil$ymin
bbox_fr_mil[1] <- bbox_fr_mil[1] - (bbox_fr_mil_xrange * 0.05)
bbox_fr_mil[3] <- bbox_fr_mil[3] + (bbox_fr_mil_xrange * 0.05)
bbox_fr_mil[2] <- bbox_fr_mil[2] - (bbox_fr_mil_yrange * 0.05)
bbox_fr_mil[4] <- bbox_fr_mil[4] + (bbox_fr_mil_yrange * 0.05)

# Prepare HAB satellite data for maps
df_hab_sat_clean <- df_hab_sat %>%
  mutate(
    strs_obj_f =
      # Crop HAB satellite data to bounding box of the Franks-Mildred shapefile
      map(strs_prx_obj, ~st_crop(.x, bbox_fr_mil) %>%
        # rename attribute to be more descriptive
        setNames("pixel_val") %>%
        # Convert pixel values to Cyano Index categories
        mutate(
          pixel_val = as.numeric(as.character(pixel_val)),
          pixel_val = case_when(
            pixel_val == 0 ~ 1,
            pixel_val <= 41 ~ 2,
            pixel_val <= 99 ~ 3,
            pixel_val <= 183 ~ 4,
            pixel_val <= 250 ~ 5,
            TRUE ~ NA_real_
          ),
          pixel_val = factor(pixel_val, levels = c(1:5), labels = ci_cat_labels)
        ) %>%
        # Convert to stars object
        st_as_stars()
    )
  ) %>%
  select(-strs_prx_obj)

# 3.2 Create Map of HAB Satellite Data ------------------------------------

# Function to create maps of HAB satellite data
create_hab_map <- function(strs_obj, map_title, x_txt_lab) {
  p <- ggplot() +
    geom_stars(data = strs_obj, na.rm = TRUE) +
    scale_fill_viridis_d(
      name = "Cyano Index\nCategory",
      drop = FALSE,
      na.translate = FALSE,
      option = "plasma",
      end = 0.95
    ) +
    geom_sf(
      data = WW_Delta_32611, 
      alpha = 0, 
      size = 0.1
    ) +
    geom_sf(
      data = sf_franks_mildred_32611, 
      alpha = 0, 
      color = "green", 
      size = 0.7
    ) +
    coord_sf(
      xlim = c(bbox_fr_mil$xmin, bbox_fr_mil$xmax),
      ylim = c(bbox_fr_mil$ymin, bbox_fr_mil$ymax)
    ) + 
    theme_bw() +
    ggtitle(map_title) +
    xlab(NULL) +
    ylab(NULL)
  
  # Only include x-axis tick labels for the bottom most map
  if (x_txt_lab == FALSE) {
    p <- p + theme(axis.text.x = element_blank())
  }
  
  return(p)
}

# Create maps of HAB satellite data for each day
map_hab_sat <- df_hab_sat_clean %>% 
  mutate(
    x_txt = c(FALSE, FALSE, TRUE),
    hab_map = pmap(
      list(strs_obj_f, date_chr, x_txt),
      create_hab_map
    )
  )

# Combine maps for each day into one
map_hab_sat_c <- wrap_plots(map_hab_sat$hab_map, ncol = 1, guides = "collect")

# Export map as a .jpg
ggsave(
  "EDB/CI_category_map_2021.jpg",
  plot = map_hab_sat_c,
  width = 6.5,
  height = 8.25,
  units = "in"
)

