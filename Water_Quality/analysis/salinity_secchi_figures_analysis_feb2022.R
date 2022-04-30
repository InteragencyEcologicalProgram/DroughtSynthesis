# Drought Synthesis - Figures and Analysis for water quality parameters
# Purpose: Create figures and run analyses of the water quality parameters
  # (Salinity and Secchi Depth) for the February 2022 Drought Synthesis report.
  # The code for the water temperature figures and analysis can be found here:
  # https://github.com/sbashevkin/FLOATDrought/blob/main/Analyses/Temperature%20analyses.Rmd
# Author: Tyler Salman and Dave Bosworth
# Contact: Tyler.Salman@water.ca.gov, David.Bosworth@water.ca.gov


# 1. Global Code ----------------------------------------------------------

# Load packages
library(tidyverse)
library(DroughtData)
library(car)
library(here)

# Check if we are in the correct working directory
i_am("Water_Quality/analysis/salinity_secchi_figures_analysis_feb2022.R")

# Source figures and analysis functions
source("Water_Quality/analysis/figures_analysis_feb2022_funcs.R")

# Define factor levels: 
# Season
season_lev <- c("Winter", "Spring", "Summer", "Fall")
# Region
region_lev <- c("Suisun Marsh", "Suisun Bay", "Confluence", "SouthCentral", "North")
# Drought Index
drt_lev <- c("D", "N", "W")
# Water Year Index
wtr_yr_lev <- c("Critical", "Dry", "Below Normal", "Above Normal", "Wet")

# Prepare seasonal average data for salinity and secchi depth
df_wq_seas <- lt_seasonal %>% 
  mutate(
    Season = factor(Season, levels = season_lev),
    Year_fac = factor(YearAdj),
    Drought = factor(Drought, levels = drt_lev)
  ) %>% 
  dplyr::select(YearAdj, Year_fac, Season, Drought, Salinity, Secchi)

# Prepare regional average data for salinity and secchi depth
df_wq_reg <- lt_regional %>% 
  mutate(
    Region = factor(Region, levels = region_lev),
    Year_fac = factor(YearAdj),
    Drought = factor(Drought, levels = drt_lev)
  ) %>% 
  dplyr::select(YearAdj, Year_fac, Region, Drought, Salinity, Secchi)

# Prepare raw data for salinity and secchi depth
df_wq_raw <- raw_wq_1975_2021 %>% 
  drt_add_yr_assign() %>% 
  mutate(
    across(
      c(Drought, YearType),
      list(
        "20_21" = ~ case_when(
          YearAdj == 2020 ~ "2020",
          YearAdj == 2021 ~ "2021",
          TRUE ~ as.character(.x)
        )
      )
    ),
    across(starts_with("Drought"), ~ factor(.x, levels = c("2020", "2021", drt_lev))),
    across(starts_with("YearType"), ~ factor(.x, levels = c("2020", "2021", wtr_yr_lev))),
    across(c(Drought, YearType), fct_drop),
    Region = factor(Region, levels = region_lev),
    Season = factor(Season, levels = season_lev)
  ) %>% 
  dplyr::select(
    Region, 
    Season, 
    YearAdj,
    starts_with("YearType"), 
    starts_with("Drought"), 
    Salinity,
    Secchi
  )


# 2. Salinity -------------------------------------------------------------

# 2.1 Seasonal Analysis ---------------------------------------------------

# Prepare seasonal salinity data
df_sal_seas <- df_wq_seas %>% filter(!is.na(Salinity))

# Plot seasonal salinity data by year
plt_sal_seas_yr <- df_sal_seas %>% 
  ggplot(aes(x = YearAdj, y = Salinity, fill = Drought)) +
  geom_point(shape = 21, color = "black") +
  facet_wrap(vars(Season)) +
  drt_color_pal_drought() +
  xlab("Year") +
  ylab("Salinity (PSU)") +
  theme_bw()

plt_sal_seas_yr

# Plot seasonal salinity data by drought index
plt_sal_seas_drt <- df_sal_seas %>%
  ggplot(aes(x = Drought, y = Salinity, fill = Drought)) +
  geom_boxplot() +
  facet_wrap(vars(Season)) +
  drt_color_pal_drought() +
  ylab("Salinity (PSU)") +
  theme_bw()

plt_sal_seas_drt


# Seasonal-Drought Index model:
mod_sal_seas_drt <- aov(Salinity ~ Drought + Season, data = df_sal_seas)

# Check assumptions
df_sal_seas %>% model_plotter(Salinity, "PSU", mod_sal_seas_drt)

# Print ANOVA table
anova_sal_seas_drt <- Anova(mod_sal_seas_drt, type = 2)
anova_sal_seas_drt

# Run post hoc test
tukplt_sal_seas_drt <- df_sal_seas %>% 
  tukey_plotter(
    Salinity,
    unit_label = "PSU",
    data_type = "Season",
    model = mod_sal_seas_drt,
    model_type = "Drought"
  )

tukplt_sal_seas_drt


# Seasonal-Year model:
mod_sal_seas_yr <- aov(Salinity ~ Year_fac + Season, data = df_sal_seas)

# Check assumptions
df_sal_seas %>% model_plotter(Salinity, "PSU", mod_sal_seas_yr)

# Print ANOVA table
anova_sal_seas_yr <- Anova(mod_sal_seas_yr, type = 2)
anova_sal_seas_yr

# Run post hoc test
tukplt_sal_seas_yr <- df_sal_seas %>% 
  tukey_plotter(
    Salinity,
    unit_label = "PSU",
    data_type = "Season",
    model = mod_sal_seas_yr,
    model_type = "Year_fac"
  )

tukplt_sal_seas_yr

# 2.2 Regional Analysis ---------------------------------------------------

# Prepare regional salinity data
df_sal_reg <- df_wq_reg %>% filter(!is.na(Salinity))

# Plot regional salinity data by year
plt_sal_reg_yr <- df_sal_reg %>% 
  ggplot(aes(x = YearAdj, y = Salinity, fill = Drought)) +
  geom_point(shape = 21, color = "black") +
  facet_wrap(vars(Region), scales = "free_y") +
  drt_color_pal_drought() +
  xlab("Year") +
  ylab("Salinity (PSU)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

plt_sal_reg_yr

# Plot regional salinity data by drought index
plt_sal_reg_drt <- df_sal_reg %>%
  ggplot(aes(x = Drought, y = Salinity, fill = Drought)) +
  geom_boxplot() +
  facet_wrap(vars(Region), scales = "free_y") +
  drt_color_pal_drought() +
  ylab("Salinity (PSU)") +
  theme_bw()

plt_sal_reg_drt


# Regional-Drought Index model:
mod_sal_reg_drt <- aov(Salinity ~ Drought + Region, data = df_sal_reg)

# Check assumptions
df_sal_reg %>% model_plotter(Salinity, "PSU", mod_sal_reg_drt)
# stick with non transformed salinity

# Print ANOVA table
anova_sal_reg_drt <- Anova(mod_sal_reg_drt, type = 2)
anova_sal_reg_drt

# Run post hoc test
tukplt_sal_reg_drt <- df_sal_reg %>% 
  tukey_plotter(
    Salinity,
    unit_label = "PSU",
    data_type = "Region",
    model = mod_sal_reg_drt,
    model_type = "Drought"
  )

tukplt_sal_reg_drt


# Regional-Year model:
mod_sal_reg_yr <- aov(Salinity ~ Year_fac + Region, data = df_sal_reg)

# Check assumptions
df_sal_reg %>% model_plotter(Salinity, "PSU", mod_sal_reg_yr)

# Print ANOVA table
anova_sal_reg_yr <- Anova(mod_sal_reg_yr, type = 2)
anova_sal_reg_yr

# Run post hoc test
tukplt_sal_reg_yr <- df_sal_reg %>% 
  tukey_plotter(
    Salinity,
    unit_label = "PSU",
    data_type = "Region",
    model = mod_sal_reg_yr,
    model_type = "Year_fac"
  )

tukplt_sal_reg_yr

# 2.3 Compare 2021 to Prior Years -----------------------------------------

# Prepare raw salinity data
df_sal_raw <- df_wq_raw %>% filter(!is.na(Salinity))

# How does 2021 compare to Drought, Normal, and Wet periods?
plt_sal_2021_comp_drt <- df_sal_raw %>% 
  ggplot(aes(x = Drought_20_21, y = Salinity, fill = Drought)) +
  geom_boxplot() +
  drt_color_pal_drought() +
  xlab("Drought") +
  ylab("Salinity (PSU)") +
  theme_bw()

plt_sal_2021_comp_drt

# Does that change regionally or seasonally?
df_sal_raw %>% 
  ggplot(aes(x = Drought_20_21, y = Salinity, fill = Drought)) +
  geom_boxplot() +
  drt_color_pal_drought() +
  facet_grid(
    rows = vars(Season),
    cols = vars(Region), 
    scales = "free_y"
  ) +
  xlab("Drought") +
  ylab("Salinity (PSU)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

# How does 2021 compare to each water year type?
plt_sal_2021_comp_yt <- df_sal_raw %>% 
  ggplot(aes(x = YearType_20_21, y = Salinity, fill = YearType)) +
  geom_boxplot() +
  drt_color_pal_yrtype() +
  xlab("Year Type") +
  ylab("Salinity (PSU)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

plt_sal_2021_comp_yt

# Does that change regionally or seasonally?
df_sal_raw %>%
  ggplot(aes(x = YearType_20_21, y = Salinity, fill = YearType)) +
  geom_boxplot() +
  drt_color_pal_yrtype() +
  facet_grid(
    rows = vars(Season),
    cols = vars(Region), 
    scales = "free_y"
  ) +
  xlab("Year Type") +
  ylab("Salinity (PSU)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

# 2.4 Export Figures and ANOVA Results ------------------------------------

# Figures:
# Seasonal data by Drought Index and Year
walk2(
  list(plt_sal_seas_drt, plt_sal_seas_yr),
  c("drt_boxplot", "yr_ts_plot"),
  ~ ggsave(
    plot = .x,
    filename = here("Water_Quality", paste0("Salinity_season_", .y, ".png")),
    height = 6,
    width = 6,
    units = "in"
  )
)

# Regional data by Drought Index and Year
walk2(
  list(plt_sal_reg_drt, plt_sal_reg_yr),
  c("drt_boxplot", "yr_ts_plot"),
  ~ ggsave(
    plot = .x,
    filename = here("Water_Quality", paste0("Salinity_region_", .y, ".png")),
    height = 6,
    width = 6.5,
    units = "in"
  )
)

# Tukey plots for the Drought Index models
walk2(
  list(tukplt_sal_seas_drt, tukplt_sal_reg_drt),
  c("season", "region"),
  ~ ggsave(
    plot = .x,
    filename = here("Water_Quality", paste0("Salinity_", .y, "_drt_model.png")),
    height = 7,
    width = 6,
    units = "in"
  )
)

# Tukey plots for the Year models
walk2(
  list(tukplt_sal_seas_yr, tukplt_sal_reg_yr),
  c("season", "region"),
  ~ ggsave(
    plot = .x,
    filename = here("Water_Quality", paste0("Salinity_", .y, "_year_model.png")),
    height = 10,
    width = 9,
    units = "in"
  )
)

# 2021 comparison plot by Drought Index
ggsave(
  plot = plt_sal_2021_comp_drt,
  filename = here("Water_Quality/Salinity_comp_20_21_drt.png"),
  height = 4,
  width = 5,
  units = "in"
)

# 2021 comparison plot by Water Year Type
ggsave(
  plot = plt_sal_2021_comp_yt, 
  filename = here("Water_Quality/Salinity_comp_20_21_yr_type.png"),
  height = 4, 
  width = 6, 
  units = "in"
)

# ANOVA results:
# Combine results into a data frame and format for export
anova_sal_comb <-
  list(
    "Regional_Drought" = anova_sal_reg_drt,
    "Regional_Year" = anova_sal_reg_yr,
    "Seasonal_Drought" = anova_sal_seas_drt,
    "Seasonal_Year" = anova_sal_seas_yr
  ) %>% 
  map(~ as_tibble(.x, rownames = "Parameter")) %>% 
  bind_rows(.id = "Model") %>%
  mutate(
    `Pr(>F)` = if_else(`Pr(>F)` < 0.001, "< 0.001", as.character(round(`Pr(>F)`, 4))),
    Parameter = if_else(Parameter == "Year_fac", "Year", Parameter)
  ) %>% 
  dplyr::select(Model, Parameter, `Sum Sq`, Df, `F value`, `Pr(>F)`)

anova_sal_comb %>% write_csv(here("Water_Quality/Salinity_anova_results.csv"), na = "")

# All exported figures and tables were moved to the Drought Synthesis SharePoint site


# 3. Secchi Depth ---------------------------------------------------------

# 3.1 Seasonal Analysis ---------------------------------------------------

# Prepare seasonal secchi depth data
df_secc_seas <- df_wq_seas %>% filter(!is.na(Secchi))

# Plot seasonal secchi depth data by year
plt_secc_seas_yr <- df_secc_seas %>% 
  ggplot(aes(x = YearAdj, y = Secchi, fill = Drought)) +
  geom_point(shape = 21, color = "black") +
  facet_wrap(vars(Season)) +
  drt_color_pal_drought() +
  xlab("Year") +
  ylab("Secchi Depth (cm)") +
  theme_bw()

plt_secc_seas_yr

# Plot seasonal secchi depth data by drought index
plt_secc_seas_drt <- df_secc_seas %>%
  ggplot(aes(x = Drought, y = Secchi, fill = Drought)) +
  geom_boxplot() +
  facet_wrap(vars(Season)) +
  drt_color_pal_drought() +
  ylab("Secchi Depth (cm)") +
  theme_bw()

plt_secc_seas_drt


# Seasonal-Drought Index model:
mod_secc_seas_drt <- aov(Secchi ~ Drought + Season, data = df_secc_seas)

# Check assumptions
df_secc_seas %>% model_plotter(Secchi, "cm", mod_secc_seas_drt)
# Histogram of residuals somewhat non-normal, try log transformation of secchi depth

df_secc_seas <- df_secc_seas %>% mutate(log_Secchi = log(Secchi))

# Create model with log-transformed secchi depth
mod_log_secc_seas_drt <- aov(log_Secchi ~ Drought + Season, data = df_secc_seas)

# Check assumptions
df_secc_seas %>% model_plotter(log_Secchi, "cm", mod_log_secc_seas_drt, log_trans = TRUE)
# log-transformed secchi depth seems to be the better way to go

# Print ANOVA table
anova_log_secc_seas_drt <- Anova(mod_log_secc_seas_drt, type = 2)
anova_log_secc_seas_drt

# Run post hoc test
tukplt_log_secc_seas_drt <- df_secc_seas %>% 
  tukey_plotter(
    log_Secchi,
    param_label = "Secchi Depth",
    unit_label = "cm",
    data_type = "Season",
    model = mod_log_secc_seas_drt, 
    model_type = "Drought",
    log_trans = TRUE
  )

tukplt_log_secc_seas_drt


# Seasonal-Year model:
mod_secc_seas_yr <- aov(Secchi ~ Year_fac + Season, data = df_secc_seas)

# Check assumptions
df_secc_seas %>% model_plotter(Secchi, "cm", mod_secc_seas_yr)

# Print ANOVA table
anova_secc_seas_yr <- Anova(mod_secc_seas_yr, type = 2)
anova_secc_seas_yr

# Run post hoc test
tukplt_secc_seas_yr <- df_secc_seas %>% 
  tukey_plotter(
    Secchi,
    param_label = "Secchi Depth",
    unit_label = "cm",
    data_type = "Season",
    model = mod_secc_seas_yr, 
    model_type = "Year_fac"
  )

tukplt_secc_seas_yr

# 2.2 Regional Analysis ---------------------------------------------------

# Prepare regional secchi depth data
df_secc_reg <- df_wq_reg %>% filter(!is.na(Secchi))

# Plot regional secchi depth data by year
plt_secc_reg_yr <- df_secc_reg %>% 
  ggplot(aes(x = YearAdj, y = Secchi, fill = Drought)) +
  geom_point(shape = 21, color = "black") +
  facet_wrap(vars(Region)) +
  drt_color_pal_drought() +
  xlab("Year") +
  ylab("Secchi Depth (cm)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

plt_secc_reg_yr

# Plot regional secchi depth data by drought index
plt_secc_reg_drt <- df_secc_reg %>%
  ggplot(aes(x = Drought, y = Secchi, fill = Drought)) +
  geom_boxplot() +
  facet_wrap(vars(Region)) +
  drt_color_pal_drought() +
  ylab("Secchi Depth (cm)") +
  theme_bw()

plt_secc_reg_drt


# Regional-Drought Index model:
mod_secc_reg_drt <- aov(Secchi ~ Drought + Region, data = df_secc_reg)

# Check assumptions
df_secc_reg %>% model_plotter(Secchi, "cm", mod_secc_reg_drt)
# Histogram of residuals non-normal and variance not constant
# Try log transformation of secchi depth

df_secc_reg <- df_secc_reg %>% mutate(log_Secchi = log(Secchi))

# Create model with log-transformed secchi depth
mod_log_secc_reg_drt <- aov(log_Secchi ~ Drought + Region, data = df_secc_reg)

# Check assumptions
df_secc_reg %>% model_plotter(log_Secchi, "cm", mod_log_secc_reg_drt, log_trans = TRUE)
# log-transformed secchi depth seems to be the better way to go

# Print ANOVA table
anova_log_secc_reg_drt <- Anova(mod_log_secc_reg_drt, type = 2)
anova_log_secc_reg_drt

# Run post hoc test
tukplt_log_secc_reg_drt <- df_secc_reg %>% 
  tukey_plotter(
    log_Secchi,
    param_label = "Secchi Depth",
    unit_label = "cm",
    data_type = "Region",
    model = mod_log_secc_reg_drt, 
    model_type = "Drought",
    log_trans = TRUE
  )

tukplt_log_secc_reg_drt


# Regional-Year model:
mod_secc_reg_yr <- aov(Secchi ~ Year_fac + Region, data = df_secc_reg)

# Check assumptions
df_secc_reg %>% model_plotter(Secchi, "cm", mod_secc_reg_yr)
# Histogram of residuals non-normal, try log transformation of secchi depth

# Create model with log-transformed secchi depth
mod_log_secc_reg_yr <- aov(log_Secchi ~ Year_fac + Region, data = df_secc_reg)

# Check assumptions
df_secc_reg %>% model_plotter(log_Secchi, "cm", mod_log_secc_reg_yr, log_trans = TRUE)
# log-transformed secchi depth seems to be the better way to go

# Print ANOVA table
anova_log_secc_reg_yr <- Anova(mod_log_secc_reg_yr, type = 2)
anova_log_secc_reg_yr

# Run post hoc test
tukplt_log_secc_reg_yr <- df_secc_reg %>% 
  tukey_plotter(
    log_Secchi,
    param_label = "Secchi Depth",
    unit_label = "cm",
    data_type = "Region",
    model = mod_log_secc_reg_yr, 
    model_type = "Year_fac",
    log_trans = TRUE
  )

tukplt_log_secc_reg_yr

# 2.3 Compare 2021 to Prior Years -----------------------------------------

# Prepare raw secchi depth data
df_secc_raw <- df_wq_raw %>% filter(!is.na(Secchi))

# How does 2021 compare to Drought, Normal, and Wet periods?
plt_secc_2021_comp_drt <- df_secc_raw %>% 
  ggplot(aes(x = Drought_20_21, y = Secchi, fill = Drought)) +
  geom_boxplot() +
  drt_color_pal_drought() +
  xlab("Drought") +
  ylab("Secchi Depth (cm)") +
  theme_bw()

plt_secc_2021_comp_drt

# Does that change regionally or seasonally?
df_secc_raw %>% 
  ggplot(aes(x = Drought_20_21, y = Secchi, fill = Drought)) +
  geom_boxplot() +
  drt_color_pal_drought() +
  facet_grid(
    rows = vars(Season),
    cols = vars(Region), 
    scales = "free_y"
  ) +
  xlab("Drought") +
  ylab("Secchi Depth (cm)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

# How does 2021 compare to each water year type?
plt_secc_2021_comp_yt <- df_secc_raw %>% 
  ggplot(aes(x = YearType_20_21, y = Secchi, fill = YearType)) +
  geom_boxplot() +
  drt_color_pal_yrtype() +
  xlab("Year Type") +
  ylab("Secchi Depth (cm)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

plt_secc_2021_comp_yt

# Does that change regionally or seasonally?
df_secc_raw %>%
  ggplot(aes(x = YearType_20_21, y = Secchi, fill = YearType)) +
  geom_boxplot() +
  drt_color_pal_yrtype() +
  facet_grid(
    rows = vars(Season),
    cols = vars(Region), 
    scales = "free_y"
  ) +
  xlab("Year Type") +
  ylab("Secchi Depth (cm)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

# 2.4 Export Figures and ANOVA Results ------------------------------------

# Figures:
# Seasonal data by Drought Index and Year
walk2(
  list(plt_secc_seas_drt, plt_secc_seas_yr),
  c("drt_boxplot", "yr_ts_plot"),
  ~ ggsave(
    plot = .x,
    filename = here("Water_Quality", paste0("Secchi_season_", .y, ".png")),
    height = 6,
    width = 6,
    units = "in"
  )
)

# Regional data by Drought Index and Year
walk2(
  list(plt_secc_reg_drt, plt_secc_reg_yr),
  c("drt_boxplot", "yr_ts_plot"),
  ~ ggsave(
    plot = .x,
    filename = here("Water_Quality", paste0("Secchi_region_", .y, ".png")),
    height = 6,
    width = 6.5,
    units = "in"
  )
)

# Tukey plots for the Drought Index models
walk2(
  list(tukplt_log_secc_seas_drt, tukplt_log_secc_reg_drt),
  c("season", "region"),
  ~ ggsave(
    plot = .x,
    filename = here("Water_Quality", paste0("Secchi_", .y, "_drt_model.png")),
    height = 7,
    width = 6,
    units = "in"
  )
)

# Tukey plots for the Year models
walk2(
  list(tukplt_secc_seas_yr, tukplt_log_secc_reg_yr),
  c("season", "region"),
  ~ ggsave(
    plot = .x,
    filename = here("Water_Quality", paste0("Secchi_", .y, "_year_model.png")),
    height = 10,
    width = 9,
    units = "in"
  )
)

# 2021 comparison plot by Drought Index
ggsave(
  plot = plt_secc_2021_comp_drt,
  filename = here("Water_Quality/Secchi_comp_20_21_drt.png"),
  height = 4,
  width = 5,
  units = "in"
)

# 2021 comparison plot by Water Year Type
ggsave(
  plot = plt_secc_2021_comp_yt, 
  filename = here("Water_Quality/Secchi_comp_20_21_yr_type.png"),
  height = 4, 
  width = 6, 
  units = "in"
)

# ANOVA results:
# Combine results into a data frame and format for export
anova_secc_comb <-
  list(
    "Regional_Drought" = anova_log_secc_reg_drt,
    "Regional_Year" = anova_log_secc_reg_yr,
    "Seasonal_Drought" = anova_log_secc_seas_drt,
    "Seasonal_Year" = anova_secc_seas_yr
  ) %>% 
  map(~ as_tibble(.x, rownames = "Parameter")) %>% 
  bind_rows(.id = "Model") %>%
  mutate(
    `Pr(>F)` = if_else(`Pr(>F)` < 0.001, "< 0.001", as.character(round(`Pr(>F)`, 4))),
    Parameter = if_else(Parameter == "Year_fac", "Year", Parameter)
  ) %>% 
  dplyr::select(Model, Parameter, `Sum Sq`, Df, `F value`, `Pr(>F)`)

anova_secc_comb %>% write_csv(here("Water_Quality/Secchi_anova_results.csv"), na = "")

# All exported figures and tables were moved to the Drought Synthesis SharePoint site

