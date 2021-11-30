# Emergency Drought Barrier - HAB satellite data
# Purpose: Create figures of the HAB satellite data for the EDB report:
  # 1) Area plot of pixel counts of Cyano Index categories for Franks Tract and
    # Mildred Island in 2020 and 2021
  # 2) A few maps of the HAB satellite data for a couple of days of interest in 2021
# Author: Dave Bosworth
# Contact: David.Bosworth@water.ca.gov

# Load packages
library(tidyverse)
library(lubridate)
library(scales)
library(EDBdata)


# 1. Prepare Count Data ---------------------------------------------------

# Create a vector for the factor levels and labels of the Cyano Index categories
ci_cat_levels <-
  c(
    "Non_detect", 
    "Low", 
    "Moderate", 
    "High", 
    "Very_high"
  )

ci_cat_labels <-
  c(
    "Non Detect", 
    "Low", 
    "Moderate", 
    "High", 
    "Very High"
  )

# Prepare HAB satellite data for stacked area plot
df_hab_sat_plt <- sat_ci_count_fr_mil %>%
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


# 2. Create Area Plot -----------------------------------------------------

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
  width = 6.5,
  height = 5.5,
  units = "in"
)

