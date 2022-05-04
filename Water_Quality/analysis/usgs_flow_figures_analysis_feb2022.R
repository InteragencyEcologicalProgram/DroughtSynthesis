# Drought Synthesis - Figures and Analysis for USGS Flow metrics
# Purpose: Create figures and run short-term analyses (2011-2021) of the USGS
  # flow metrics (USGS combined total outflow and Cache Slough flow) for the
  # February 2022 Drought Synthesis report. Script also contains the short-term
  # analysis (2011-2021) of the Dayflow Delta Outflow data and a comparison
  # between the USGS combined total outflow and the Dayflow Delta Outflow data.
# Author: Liz Stumpner and Dave Bosworth
# Contact: estumpner@usgs.gov, David.Bosworth@water.ca.gov


# 1. Global Code ----------------------------------------------------------

# Load packages
library(tidyverse)
library(magrittr)
library(scales)
library(DroughtData)
library(car)
library(ggpubr)
library(here)

# Check if we are in the correct working directory
i_am("Water_Quality/analysis/usgs_flow_figures_analysis_feb2022.R")

# Source figures and analysis functions
source("Water_Quality/analysis/figures_analysis_feb2022_funcs.R")

# Define factor levels: 
# Season
season_lev <- c("Winter", "Spring", "Summer", "Fall")
# Drought Index
drt_lev <- c("D", "N", "W")

# Calculate seasonal averages of the USGS combined total outflow, Cache Slough
  # flow, and Dayflow Delta Outflow and prepare for plotting and analysis
df_flow_seas_avg <- raw_hydro_1975_2021 %>% 
  # Restrict years for short-term analysis
  filter(YearAdj %in% 2011:2021) %>% 
  group_by(YearAdj, Season) %>% 
  summarize(across(contains("flow"), mean, na.rm = TRUE)) %>% 
  ungroup() %>% 
  drt_add_yr_assign() %>% 
  mutate(
    Season = factor(Season, levels = season_lev),
    Year_fac = factor(YearAdj),
    Drought = factor(Drought, levels = drt_lev)
  ) %>% 
  dplyr::select(
    YearAdj,
    Year_fac,
    Season, 
    YearType, 
    Drought, 
    Outflow, 
    TotalUSGSOutflow, 
    CacheFlow
  )


# 2. USGS Combined Total Outflow ------------------------------------------

# Prepare USGS combined total outflow data - seasonal averages
df_comb_usgs_seas <- df_flow_seas_avg %>% filter(!is.na(TotalUSGSOutflow))

# Plot seasonal USGS combined total outflow data by year
plt_comb_usgs_yr <- df_comb_usgs_seas %>% 
  ggplot(aes(x = YearAdj, y = TotalUSGSOutflow, fill = Drought)) +
  geom_point(shape = 21, color = "black") +
  facet_wrap(vars(Season), scales = "free_y") +
  drt_color_pal_drought() +
  xlab("Year") +
  scale_y_continuous(name = "Combined USGS Outflow (cfs)", labels = scales::label_comma()) +
  theme_bw()

plt_comb_usgs_yr

# Plot seasonal USGS combined total outflow by drought index
plt_comb_usgs_drt <- df_comb_usgs_seas %>%
  ggplot(aes(x = Drought, y = TotalUSGSOutflow, fill = Drought)) +
  geom_boxplot() +
  facet_wrap(vars(Season), scales = "free_y") +
  drt_color_pal_drought() +
  scale_y_continuous(name = "Combined USGS Outflow (cfs)", labels = scales::label_comma()) +
  theme_bw()

plt_comb_usgs_drt

# 2.1 ANOVA Analysis ------------------------------------------------------

# Seasonal-Drought Index model:
mod_comb_usgs_drt <- aov(TotalUSGSOutflow ~ Drought + Season, data = df_comb_usgs_seas)

# Check assumptions
df_comb_usgs_seas %>% model_plotter(TotalUSGSOutflow, "cfs", mod_comb_usgs_drt)
# Histogram of residuals non-normal and variance not constant
# Try log transformation of USGS combined total outflow

df_comb_usgs_seas %<>% mutate(log_TotalUSGSOutflow = log(TotalUSGSOutflow))

# Create model with log-transformed USGS combined total outflow
mod_log_comb_usgs_drt <- aov(log_TotalUSGSOutflow ~ Drought + Season, data = df_comb_usgs_seas)

# Check assumptions
df_comb_usgs_seas %>% model_plotter(log_TotalUSGSOutflow, "cfs", mod_log_comb_usgs_drt, log_trans = TRUE)
# log-transformed USGS combined total outflow seems to be the better way to go

# Print ANOVA table
anova_log_comb_usgs_drt <- Anova(mod_log_comb_usgs_drt, type = 2)
anova_log_comb_usgs_drt

# Run post hoc test
tukplt_log_comb_usgs_drt <- df_comb_usgs_seas %>% 
  tukey_plotter(
    log_TotalUSGSOutflow,
    param_label = "Combined USGS Outflow",
    unit_label = "cfs",
    data_type = "Season",
    model = mod_log_comb_usgs_drt,
    model_type = "Drought",
    log_trans = TRUE
  )

tukplt_log_comb_usgs_drt


# Seasonal-Year model:
mod_comb_usgs_yr <- aov(TotalUSGSOutflow ~ Year_fac + Season, data = df_comb_usgs_seas)

# Check assumptions
df_comb_usgs_seas %>% model_plotter(TotalUSGSOutflow, "cfs", mod_comb_usgs_yr)
# Histogram of residuals non-normal and variance not constant
# Try log transformation of USGS combined total outflow

# Create model with log-transformed USGS combined total outflow
mod_log_comb_usgs_yr <- aov(log_TotalUSGSOutflow ~ Year_fac + Season, data = df_comb_usgs_seas)

# Check assumptions
df_comb_usgs_seas %>% model_plotter(log_TotalUSGSOutflow, "cfs", mod_log_comb_usgs_yr, log_trans = TRUE)
# log-transformed USGS combined total outflow seems to be the better way to go

# Print ANOVA table
anova_log_comb_usgs_yr <- Anova(mod_log_comb_usgs_yr, type = 2)
anova_log_comb_usgs_yr

# Run post hoc test
tukplt_log_comb_usgs_yr <- df_comb_usgs_seas %>% 
  tukey_plotter(
    log_TotalUSGSOutflow,
    param_label = "Combined USGS Outflow",
    unit_label = "cfs",
    data_type = "Season",
    model = mod_log_comb_usgs_yr,
    model_type = "Year_fac",
    log_trans = TRUE
  )

tukplt_log_comb_usgs_yr

# 2.2 Export Figures and ANOVA Results ------------------------------------

# Figures:
# Seasonal data by Drought Index and Year
walk2(
  list(plt_comb_usgs_drt, plt_comb_usgs_yr),
  c("drt_boxplot", "yr_ts_plot"),
  ~ ggsave(
    plot = .x,
    filename = here("Water_Quality", paste0("USGS_outflow_season_", .y, ".png")),
    height = 4.5,
    width = 6,
    units = "in"
  )
)

# Tukey plot for the Drought Index model
ggsave(
  plot = tukplt_log_comb_usgs_drt,
  filename = here("Water_Quality/USGS_outflow_season_drt_model.png"),
  height = 7,
  width = 6,
  units = "in"
)

# Tukey plot for the Year model
ggsave(
  plot = tukplt_log_comb_usgs_yr,
  filename = here("Water_Quality/USGS_outflow_season_year_model.png"),
  height = 10,
  width = 9,
  units = "in"
)

# ANOVA results:
# Combine results into a data frame and format for export
anova_comb_usgs_comb <-
  list(
    "Seasonal_Drought" = anova_log_comb_usgs_drt,
    "Seasonal_Year" = anova_log_comb_usgs_yr
  ) %>% 
  map(~ as_tibble(.x, rownames = "Parameter")) %>% 
  bind_rows(.id = "Model") %>%
  mutate(
    `Pr(>F)` = if_else(`Pr(>F)` < 0.001, "< 0.001", as.character(round(`Pr(>F)`, 4))),
    Parameter = if_else(Parameter == "Year_fac", "Year", Parameter)
  ) %>% 
  dplyr::select(Model, Parameter, `Sum Sq`, Df, `F value`, `Pr(>F)`)

anova_comb_usgs_comb %>% write_csv(here("Water_Quality/USGS_outflow_anova_results.csv"), na = "")

# All exported figures and tables were moved to the Drought Synthesis SharePoint site


# 3. Dayflow Delta Outflow - short-term analysis --------------------------

# We ran this analysis to compare with the results of the USGS combined total outflow

# Prepare Dayflow Delta outflow data - seasonal averages
df_dflow_out_seas <- df_flow_seas_avg %>% filter(!is.na(Outflow))

# Plot seasonal Dayflow Delta outflow data by year
plt_dflow_out_yr <- df_dflow_out_seas %>% 
  ggplot(aes(x = YearAdj, y = Outflow, fill = Drought)) +
  geom_point(shape = 21, color = "black") +
  facet_wrap(vars(Season), scales = "free_y") +
  drt_color_pal_drought() +
  xlab("Year") +
  scale_y_continuous(name = "Outflow (cfs)", labels = scales::label_comma()) +
  theme_bw()

plt_dflow_out_yr

# Plot seasonal Dayflow Delta outflow data by drought index
plt_dflow_out_drt <- df_dflow_out_seas %>%
  ggplot(aes(x = Drought, y = Outflow, fill = Drought)) +
  geom_boxplot() +
  facet_wrap(vars(Season), scales = "free_y") +
  drt_color_pal_drought() +
  scale_y_continuous(name = "Outflow (cfs)", labels = scales::label_comma()) +
  theme_bw()

plt_dflow_out_drt

# 3.1 ANOVA Analysis ------------------------------------------------------

# Seasonal-Drought Index model:
mod_dflow_out_drt <- aov(Outflow ~ Drought + Season, data = df_dflow_out_seas)

# Check assumptions
df_dflow_out_seas %>% model_plotter(Outflow, "cfs", mod_dflow_out_drt)
# Histogram of residuals non-normal and variance not constant
# Try log transformation of Dayflow Delta outflow

df_dflow_out_seas %<>% mutate(log_Outflow = log(Outflow))

# Create model with log-transformed Dayflow Delta outflow
mod_log_dflow_out_drt <- aov(log_Outflow ~ Drought + Season, data = df_dflow_out_seas)

# Check assumptions
df_dflow_out_seas %>% model_plotter(log_Outflow, "cfs", mod_log_dflow_out_drt, log_trans = TRUE)
# log-transformed Dayflow Delta outflow seems to be the better way to go

# Print ANOVA table
anova_log_dflow_out_drt <- Anova(mod_log_dflow_out_drt, type = 2)
anova_log_dflow_out_drt

# Run post hoc test
tukplt_log_dflow_out_drt <- df_dflow_out_seas %>% 
  tukey_plotter(
    log_Outflow,
    param_label = "Outflow",
    unit_label = "cfs",
    data_type = "Season",
    model = mod_log_dflow_out_drt,
    model_type = "Drought",
    log_trans = TRUE
  )

tukplt_log_dflow_out_drt


# Seasonal-Year model:
mod_dflow_out_yr <- aov(Outflow ~ Year_fac + Season, data = df_dflow_out_seas)

# Check assumptions
df_dflow_out_seas %>% model_plotter(Outflow, "cfs", mod_dflow_out_yr)
# Histogram of residuals non-normal and variance not constant
# Try log transformation of Dayflow Delta outflow

# Create model with log-transformed Dayflow Delta outflow
mod_log_dflow_out_yr <- aov(log_Outflow ~ Year_fac + Season, data = df_dflow_out_seas)

# Check assumptions
df_dflow_out_seas %>% model_plotter(log_Outflow, "cfs", mod_log_dflow_out_yr, log_trans = TRUE)
# log-transformed Delta outflow seems to be the better way to go

# Print ANOVA table
anova_log_dflow_out_yr <- Anova(mod_log_dflow_out_yr, type = 2)
anova_log_dflow_out_yr

# Run post hoc test
tukplt_log_dflow_out_yr <- df_dflow_out_seas %>% 
  tukey_plotter(
    log_Outflow,
    param_label = "Outflow",
    unit_label = "cfs",
    data_type = "Season",
    model = mod_log_dflow_out_yr,
    model_type = "Year_fac",
    log_trans = TRUE
  )

tukplt_log_dflow_out_yr

# 3.2 Export Figures and ANOVA Results ------------------------------------

# Figures:
# Seasonal data by Drought Index and Year
walk2(
  list(plt_dflow_out_drt, plt_dflow_out_yr),
  c("drt_boxplot", "yr_ts_plot"),
  ~ ggsave(
    plot = .x,
    filename = here("Water_Quality", paste0("ST_Dayflow_outflow_season_", .y, ".png")),
    height = 4.5,
    width = 6,
    units = "in"
  )
)

# Tukey plot for the Drought Index model
ggsave(
  plot = tukplt_log_dflow_out_drt,
  filename = here("Water_Quality/ST_Dayflow_outflow_season_drt_model.png"),
  height = 7,
  width = 6,
  units = "in"
)

# Tukey plot for the Year model
ggsave(
  plot = tukplt_log_dflow_out_yr,
  filename = here("Water_Quality/ST_Dayflow_outflow_season_year_model.png"),
  height = 10,
  width = 9,
  units = "in"
)

# ANOVA results:
# Combine results into a data frame and format for export
anova_dflow_out_comb <-
  list(
    "Seasonal_Drought" = anova_log_dflow_out_drt,
    "Seasonal_Year" = anova_log_dflow_out_yr
  ) %>% 
  map(~ as_tibble(.x, rownames = "Parameter")) %>% 
  bind_rows(.id = "Model") %>%
  mutate(
    `Pr(>F)` = if_else(`Pr(>F)` < 0.001, "< 0.001", as.character(round(`Pr(>F)`, 4))),
    Parameter = if_else(Parameter == "Year_fac", "Year", Parameter)
  ) %>% 
  dplyr::select(Model, Parameter, `Sum Sq`, Df, `F value`, `Pr(>F)`)

anova_dflow_out_comb %>% write_csv(here("Water_Quality/ST_Dayflow_outflow_anova_results.csv"), na = "")

# All exported figures and tables were moved to the Drought Synthesis SharePoint site


# 4. USGS Total Outflow and Dayflow Outflow Comparison --------------------

# Prepare all flow data for 2011-2021
df_flow_comp <- raw_hydro_1975_2021 %>% 
  filter(YearAdj %in% 2011:2021) %>% 
  filter(!if_any(c(Outflow, TotalUSGSOutflow), is.na)) %>% 
  dplyr::select(YearAdj, Date, Outflow, TotalUSGSOutflow)

# Plot flow data comparison for all data (2011-2021)
plt_dayflow_usgs_comp <- df_flow_comp %>% 
  ggplot(aes(x = Outflow, y = TotalUSGSOutflow)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x) +
  stat_regline_equation(label.x.npc = "left", label.y = 300000) +
  stat_cor(
    aes(label = ..rr.label..), 
    label.x.npc = "left", 
    label.y = 275000
  ) +
  scale_x_continuous(name = "Dayflow Outflow (cfs)", labels = label_comma()) +
  scale_y_continuous(name = "Combined USGS Outflow (cfs)", labels = label_comma()) +
  geom_rect(
    aes(xmin = 0 - 0.5, xmax = 10000 + 0.5, ymin = -10000 - 0.5, ymax = 22000 + 0.5), 
    alpha = 0,
    color = "purple", 
    size = 1.5
  ) +
  theme_bw() +
  annotate(
    geom = "text", 
    x = 0, 
    y = 360000, 
    size = 10, 
    label = "A"
  )

plt_dayflow_usgs_comp


# Exclude to Dayflow Delta outflow less than 10,000 cfs
df_flow_comp_low <- df_flow_comp %>% filter(Outflow < 10000)

# Plot flow data comparison where Dayflow Delta outflow is less than 10,000 cfs
plt_dayflow_usgs_comp_low <- df_flow_comp_low %>% 
  ggplot(aes(x = Outflow, y = TotalUSGSOutflow)) +
  geom_point() +
  geom_smooth(method = "lm", formula = y ~ x) +
  stat_regline_equation(label.x = 300, label.y = 16000) +
  stat_cor(aes(label = ..rr.label..), label.x = 300, label.y = 14000) +
  scale_x_continuous(name = "Dayflow Outflow (cfs)", labels = label_comma()) +
  scale_y_continuous(name = "Combined USGS Outflow (cfs)", labels = label_comma()) +
  geom_rect(
    aes(xmin = 0 - 0.5, xmax = 10000 + 0.5, ymin = -10000 - 0.5, ymax = 22000 + 0.5), 
    alpha = 0, 
    color = "purple", 
    size = 1.5
  ) +
  theme_bw() +
  annotate(
    geom = "text", 
    x = 500, 
    y = 20000, 
    size = 10, 
    label = "B"
  )

plt_dayflow_usgs_comp_low


# Calculate percent Dayflow Delta Outflow values less than 10,000 cfs
raw_hydro_1975_2021 %>% 
  filter(
    YearAdj %in% 2011:2021,
    !is.na(Outflow)
  ) %>% 
  mutate(OutflowPercentile = cume_dist(Outflow)) %>% 
  filter(Outflow > 9950 & Outflow < 10050) %>% 
  arrange(Outflow)
# approximately 59%

# Calculate RMSE
# All flow data:
lm_flow_comp <- lm(TotalUSGSOutflow ~ Outflow, data = df_flow_comp)
sqrt(mean(resid(lm_flow_comp)^2))
# 6743.786

# Excluded to Dayflow Delta outflow less than 10,000 cfs:
lm_flow_comp_low <- lm(TotalUSGSOutflow ~ Outflow, data = df_flow_comp_low)
sqrt(mean(resid(lm_flow_comp_low)^2))
# 3793.764

# 4.1 Export Figures ------------------------------------------------------

# Flow data comparison for all data (2011-2021)
ggsave(
  plot = plt_dayflow_usgs_comp, 
  filename = here("Water_Quality/Dayflow_USGS_outflow_comp_all_flow.png"),
  height = 4.5, 
  width = 6, 
  units = "in"
)

# Flow data comparison where Dayflow Delta outflow is less than 10,000 cfs
ggsave(
  plot = plt_dayflow_usgs_comp_low, 
  filename = here("Water_Quality/Dayflow_USGS_outflow_comp_low_flow.png"),
  height = 4.5, 
  width = 6, 
  units = "in"
)

# All exported figures were moved to the Drought Synthesis SharePoint site


# 5. Cache Slough at Ryer Flow --------------------------------------------

# Prepare Cache Slough at Ryer flow data - seasonal averages
df_cache_seas <- df_flow_seas_avg %>% filter(!is.na(CacheFlow))

# Plot seasonal Cache Slough at Ryer flow data by year
plt_cache_yr <- df_cache_seas %>% 
  ggplot(aes(x = YearAdj, y = CacheFlow, fill = Drought)) +
  geom_point(shape = 21, color = "black") +
  facet_wrap(vars(Season), scales = "free_y") +
  drt_color_pal_drought() +
  xlab("Year") +
  scale_y_continuous(name = "Cache Slough Flow (cfs)", labels = scales::label_comma()) +
  theme_bw()

plt_cache_yr

# Plot seasonal Cache Slough at Ryer flow by drought index
plt_cache_drt <- df_cache_seas %>%
  ggplot(aes(x = Drought, y = CacheFlow, fill = Drought)) +
  geom_boxplot() +
  facet_wrap(vars(Season), scales = "free_y") +
  drt_color_pal_drought() +
  scale_y_continuous(name = "Cache Slough Flow (cfs)", labels = scales::label_comma()) +
  theme_bw()

plt_cache_drt

# 5.1 ANOVA Analysis ------------------------------------------------------

# Seasonal-Drought Index model:
mod_cache_drt <- aov(CacheFlow ~ Drought + Season, data = df_cache_seas)

# Check assumptions
df_cache_seas %>% model_plotter(CacheFlow, "cfs", mod_cache_drt)
# Histogram of residuals non-normal and variance not constant
# Try log transformation of Cache Slough at Ryer flow. We'll need to calculate
  # an offset of the flow data before log transformation since there are a few
  # negative values.

df_cache_seas %<>% mutate(log_CacheFlow_offset = log(CacheFlow + abs(min(CacheFlow)) + 1))

# Create model with log-transformed offset Cache Slough at Ryer flow
mod_log_cache_drt <- aov(log_CacheFlow_offset ~ Drought + Season, data = df_cache_seas)

# Check assumptions
df_cache_seas %>% model_plotter(log_CacheFlow_offset, "cfs", mod_log_cache_drt, log_trans = TRUE)
# log-transforming the offset Cache Slough at Ryer flow may be too strong but it
  # seems to be the better way to go

# Print ANOVA table
anova_log_cache_drt <- Anova(mod_log_cache_drt, type = 2)
anova_log_cache_drt

# Run post hoc test
tukplt_log_cache_drt <- df_cache_seas %>% 
  tukey_plotter(
    log_CacheFlow_offset,
    param_label = "Offset Cache Slough Flow",
    unit_label = "cfs",
    data_type = "Season",
    model = mod_log_cache_drt,
    model_type = "Drought",
    log_trans = TRUE
  )

tukplt_log_cache_drt


# Seasonal-Year model:
mod_cache_yr <- aov(CacheFlow ~ Year_fac + Season, data = df_cache_seas)

# Check assumptions
df_cache_seas %>% model_plotter(CacheFlow, "cfs", mod_cache_yr)
# Histogram of residuals non-normal and variance not constant
# Try log transformation of offset Cache Slough at Ryer flow 

# Create model with log-transformed offset Cache Slough at Ryer flow 
mod_log_cache_yr <- aov(log_CacheFlow_offset ~ Year_fac + Season, data = df_cache_seas)

# Check assumptions
df_cache_seas %>% model_plotter(log_CacheFlow_offset, "cfs", mod_log_cache_yr, log_trans = TRUE)
# log-transforming the offset Cache Slough at Ryer flow may be too strong but it
  # seems to be the better way to go

# Print ANOVA table
anova_log_cache_yr <- Anova(mod_log_cache_yr, type = 2)
anova_log_cache_yr

# Run post hoc test
tukplt_log_cache_yr <- df_cache_seas %>% 
  tukey_plotter(
    log_CacheFlow_offset,
    param_label = "Offset Cache Slough Flow",
    unit_label = "cfs",
    data_type = "Season",
    model = mod_log_cache_yr,
    model_type = "Year_fac",
    log_trans = TRUE
  )

tukplt_log_cache_yr

# 5.2 Export Figures and ANOVA Results ------------------------------------

# Figures:
# Seasonal data by Drought Index and Year
walk2(
  list(plt_cache_drt, plt_cache_yr),
  c("drt_boxplot", "yr_ts_plot"),
  ~ ggsave(
    plot = .x,
    filename = here("Water_Quality", paste0("Cache_flow_season_", .y, ".png")),
    height = 4.5,
    width = 6,
    units = "in"
  )
)

# Tukey plot for the Drought Index model
ggsave(
  plot = tukplt_log_cache_drt,
  filename = here("Water_Quality/Cache_flow_season_drt_model.png"),
  height = 7,
  width = 6,
  units = "in"
)

# Tukey plot for the Year model
ggsave(
  plot = tukplt_log_cache_yr,
  filename = here("Water_Quality/Cache_flow_season_year_model.png"),
  height = 10,
  width = 9,
  units = "in"
)

# ANOVA results:
# Combine results into a data frame and format for export
anova_cache_comb <-
  list(
    "Seasonal_Drought" = anova_log_cache_drt,
    "Seasonal_Year" = anova_log_cache_yr
  ) %>% 
  map(~ as_tibble(.x, rownames = "Parameter")) %>% 
  bind_rows(.id = "Model") %>%
  mutate(
    `Pr(>F)` = if_else(`Pr(>F)` < 0.001, "< 0.001", as.character(round(`Pr(>F)`, 4))),
    Parameter = if_else(Parameter == "Year_fac", "Year", Parameter)
  ) %>% 
  dplyr::select(Model, Parameter, `Sum Sq`, Df, `F value`, `Pr(>F)`)

anova_cache_comb %>% write_csv(here("Water_Quality/Cache_flow_anova_results.csv"), na = "")

# All exported figures and tables were moved to the Drought Synthesis SharePoint site

