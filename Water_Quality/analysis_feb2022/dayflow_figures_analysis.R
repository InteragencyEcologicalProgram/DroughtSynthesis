# Drought Synthesis - Figures and Analysis for Dayflow metrics
# Purpose: Create figures and run analyses of the Dayflow metrics (Outflow,
  # Exports, and X2 position) for the February 2022 Drought Synthesis report.
# Author: Liz Stumpner and Dave Bosworth
# Contact: estumpner@usgs.gov, David.Bosworth@water.ca.gov


# 1. Global Code ----------------------------------------------------------

# Load packages
library(tidyverse)
library(scales)
library(DroughtData)
library(car)
library(here)

# Check if we are in the correct working directory
i_am("Water_Quality/analysis_feb2022/dayflow_figures_analysis.R")

# Source figures and analysis functions
source(here("Water_Quality/analysis_feb2022/wq_flow_figures_analysis_funcs.R"))

# Define factor levels: 
# Season
season_lev <- c("Winter", "Spring", "Summer", "Fall")
# Drought Index
drt_lev <- c("D", "N", "W")
# Water Year Index
wtr_yr_lev <- c("Critical", "Dry", "Below Normal", "Above Normal", "Wet")

# Prepare seasonal average data for Dayflow metrics
df_dayflow_seas <- lt_seasonal %>% 
  mutate(
    Season = factor(Season, levels = season_lev),
    Year_fac = factor(YearAdj),
    Drought = factor(Drought, levels = drt_lev)
  ) %>% 
  dplyr::select(YearAdj, Year_fac, Season, Drought, Outflow, Export, X2)

# Prepare raw data for Dayflow metrics
df_dayflow_raw <- raw_hydro_1975_2021 %>% 
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
    Season = factor(Season, levels = season_lev)
  ) %>% 
  dplyr::select(
    Season, 
    YearAdj,
    starts_with("YearType"), 
    starts_with("Drought"), 
    Outflow,
    Export,
    X2
  )


# 2. Delta Outflow --------------------------------------------------------

# Prepare Delta outflow data - seasonal averages
df_outflow_seas <- df_dayflow_seas %>% filter(!is.na(Outflow))

# Plot seasonal Delta outflow data by year
plt_outflow_yr <- df_outflow_seas %>% 
  ggplot(aes(x = YearAdj, y = Outflow, fill = Drought)) +
  geom_point(shape = 21, color = "black") +
  facet_wrap(vars(Season), scales = "free_y") +
  drt_color_pal_drought() +
  xlab("Year") +
  scale_y_continuous(name = "Outflow (cfs)", labels = scales::label_comma()) +
  theme_bw()

plt_outflow_yr

# Plot seasonal Delta outflow data by drought index
plt_outflow_drt <- df_outflow_seas %>%
  ggplot(aes(x = Drought, y = Outflow, fill = Drought)) +
  geom_boxplot() +
  facet_wrap(vars(Season), scales = "free_y") +
  drt_color_pal_drought() +
  scale_y_continuous(name = "Outflow (cfs)", labels = scales::label_comma()) +
  theme_bw()

plt_outflow_drt

# 2.1 ANOVA Analysis ------------------------------------------------------

# Seasonal-Drought Index model:
mod_outflow_drt <- aov(Outflow ~ Drought + Season, data = df_outflow_seas)

# Check assumptions
df_outflow_seas %>% model_plotter(Outflow, "cfs", mod_outflow_drt)
# Histogram of residuals non-normal and variance not constant
# Try log transformation of Delta outflow

df_outflow_seas <- df_outflow_seas %>% mutate(log_Outflow = log(Outflow))

# Create model with log-transformed Delta outflow
mod_log_outflow_drt <- aov(log_Outflow ~ Drought + Season, data = df_outflow_seas)

# Check assumptions
df_outflow_seas %>% model_plotter(log_Outflow, "cfs", mod_log_outflow_drt, log_trans = TRUE)
# log-transformed Delta outflow seems to be the better way to go

# Print ANOVA table
anova_log_outflow_drt <- Anova(mod_log_outflow_drt, type = 2)
anova_log_outflow_drt

# Run post hoc test
tukplt_log_outflow_drt <- df_outflow_seas %>% 
  tukey_plotter(
    log_Outflow,
    param_label = "Outflow",
    unit_label = "cfs",
    data_type = "Season",
    model = mod_log_outflow_drt,
    model_type = "Drought",
    log_trans = TRUE
  )

tukplt_log_outflow_drt


# Seasonal-Year model:
mod_outflow_yr <- aov(Outflow ~ Year_fac + Season, data = df_outflow_seas)

# Check assumptions
df_outflow_seas %>% model_plotter(Outflow, "cfs", mod_outflow_yr)
# Histogram of residuals non-normal and variance not constant
# Try log transformation of Delta outflow

# Create model with log-transformed Delta outflow
mod_log_outflow_yr <- aov(log_Outflow ~ Year_fac + Season, data = df_outflow_seas)

# Check assumptions
df_outflow_seas %>% model_plotter(log_Outflow, "cfs", mod_log_outflow_yr, log_trans = TRUE)
# log-transformed Delta outflow seems to be the better way to go

# Print ANOVA table
anova_log_outflow_yr <- Anova(mod_log_outflow_yr, type = 2)
anova_log_outflow_yr

# Run post hoc test
tukplt_log_outflow_yr <- df_outflow_seas %>% 
  tukey_plotter(
    log_Outflow,
    param_label = "Outflow",
    unit_label = "cfs",
    data_type = "Season",
    model = mod_log_outflow_yr,
    model_type = "Year_fac",
    log_trans = TRUE
  )

tukplt_log_outflow_yr

# 2.2 Compare 2021 to Prior Years -----------------------------------------

# Prepare raw Delta outflow data
df_outflow_raw <- df_dayflow_raw %>% filter(!is.na(Outflow)) 

# How does 2021 compare to Drought, Normal, and Wet periods?
plt_outflow_2021_comp_drt <- df_outflow_raw %>% 
  ggplot(aes(x = Drought_20_21, y = Outflow, fill = Drought)) +
  geom_boxplot() +
  drt_color_pal_drought() +
  xlab("Drought") +
  ylab("Outflow (cfs)") +
  scale_y_log10(labels = trans_format("log10", math_format(10^.x))) +
  annotation_logticks(sides = "l") +
  theme_bw() +
  theme(panel.grid.minor = element_blank())

plt_outflow_2021_comp_drt

# Does that change seasonally?
plt_outflow_2021_comp_drt_s <- df_outflow_raw %>% 
  ggplot(aes(x = Drought_20_21, y = Outflow, fill = Drought)) +
  geom_boxplot() +
  drt_color_pal_drought() +
  facet_grid(cols = vars(Season)) +
  xlab("Drought") +
  ylab("Outflow (cfs)") +
  scale_y_log10(labels = trans_format("log10", math_format(10^.x))) +
  annotation_logticks(sides = "l") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    panel.grid.minor = element_blank(),
    legend.position = "top"
  )

plt_outflow_2021_comp_drt_s

# How does 2021 compare to each water year type?
plt_outflow_2021_comp_yt <- df_outflow_raw %>% 
  ggplot(aes(x = YearType_20_21, y = Outflow, fill = YearType)) +
  geom_boxplot() +
  drt_color_pal_yrtype() +
  xlab("Year Type") +
  ylab("Outflow (cfs)") +
  scale_y_log10(labels = trans_format("log10", math_format(10^.x))) +
  annotation_logticks(sides = "l") +
  theme_bw() +
  theme(
    panel.grid.minor = element_blank(),
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5)
  )

plt_outflow_2021_comp_yt

# Does that change seasonally?
df_outflow_raw %>% 
  ggplot(aes(x = YearType_20_21, y = Outflow, fill = YearType)) +
  geom_boxplot() +
  drt_color_pal_yrtype() +
  facet_grid(cols = vars(Season)) +
  xlab("Year Type") +
  ylab("Outflow (cfs)") +
  scale_y_log10(labels = trans_format("log10", math_format(10^.x))) +
  annotation_logticks(sides = "l") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5),
    panel.grid.minor = element_blank()
  )

# 2.3 Export Figures and ANOVA Results ------------------------------------

# Figures:
# Seasonal data by Drought Index and Year
walk2(
  list(plt_outflow_drt, plt_outflow_yr),
  c("drt_boxplot", "yr_ts_plot"),
  ~ ggsave(
    plot = .x,
    filename = here("Water_Quality", paste0("Dayflow_outflow_season_", .y, ".png")),
    height = 5,
    width = 6,
    units = "in"
  )
)

# Tukey plot for the Drought Index model
ggsave(
  plot = tukplt_log_outflow_drt,
  filename = here("Water_Quality/Dayflow_outflow_season_drt_model.png"),
  height = 7,
  width = 6,
  units = "in"
)

# Tukey plot for the Year model
ggsave(
  plot = tukplt_log_outflow_yr,
  filename = here("Water_Quality/Dayflow_outflow_season_year_model.png"),
  height = 19,
  width = 16,
  units = "in"
)

# 2021 comparison plot by Drought Index
ggsave(
  plot = plt_outflow_2021_comp_drt,
  filename = here("Water_Quality/Dayflow_outflow_comp_20_21_drt.png"),
  height = 4,
  width = 5,
  units = "in"
)

# 2021 comparison plot by Drought Index facetted by Season
ggsave(
  plot = plt_outflow_2021_comp_drt_s,
  filename = here("Water_Quality/Dayflow_outflow_comp_20_21_drt_seas.png"),
  height = 5.5,
  width = 6.5,
  units = "in"
)

# 2021 comparison plot by Water Year Type
ggsave(
  plot = plt_outflow_2021_comp_yt, 
  filename = here("Water_Quality/Dayflow_outflow_comp_20_21_yr_type.png"),
  height = 4, 
  width = 6, 
  units = "in"
)

# ANOVA results:
# Combine results into a data frame and format for export
anova_outflow_comb <-
  list(
    "Seasonal_Drought" = anova_log_outflow_drt,
    "Seasonal_Year" = anova_log_outflow_yr
  ) %>% 
  map(~ as_tibble(.x, rownames = "Parameter")) %>% 
  bind_rows(.id = "Model") %>%
  mutate(
    `Pr(>F)` = if_else(`Pr(>F)` < 0.001, "< 0.001", as.character(round(`Pr(>F)`, 4))),
    Parameter = if_else(Parameter == "Year_fac", "Year", Parameter)
  ) %>% 
  dplyr::select(Model, Parameter, `Sum Sq`, Df, `F value`, `Pr(>F)`)

anova_outflow_comb %>% write_csv(here("Water_Quality/Dayflow_outflow_anova_results.csv"), na = "")

# All exported figures and tables were moved to the Drought Synthesis SharePoint site


# 3. Delta Exports --------------------------------------------------------

# Prepare Delta exports data - seasonal averages
df_export_seas <- df_dayflow_seas %>% filter(!is.na(Export))

# Plot seasonal Delta exports data by year
plt_export_yr <- df_export_seas %>% 
  ggplot(aes(x = YearAdj, y = Export, fill = Drought)) +
  geom_point(shape = 21, color = "black") +
  facet_wrap(vars(Season), scales = "free_y") +
  drt_color_pal_drought() +
  xlab("Year") +
  scale_y_continuous(name = "Export (cfs)", labels = scales::label_comma()) +
  theme_bw()

plt_export_yr

# Plot seasonal Delta exports data by drought index
plt_export_drt <- df_export_seas %>%
  ggplot(aes(x = Drought, y = Export, fill = Drought)) +
  geom_boxplot() +
  facet_wrap(vars(Season), scales = "free_y") +
  drt_color_pal_drought() +
  scale_y_continuous(name = "Export (cfs)", labels = scales::label_comma()) +
  theme_bw()

plt_export_drt

# 3.1 ANOVA Analysis ------------------------------------------------------

# Seasonal-Drought Index model:
mod_export_drt <- aov(Export ~ Drought + Season, data = df_export_seas)

# Check assumptions
df_export_seas %>% model_plotter(Export, "cfs", mod_export_drt)

# Print ANOVA table
anova_export_drt <- Anova(mod_export_drt, type = 2)
anova_export_drt

# Run post hoc test
tukplt_export_drt <- df_export_seas %>% 
  tukey_plotter(
    Export,
    unit_label = "cfs",
    data_type = "Season",
    model = mod_export_drt, 
    model_type = "Drought"
  )

tukplt_export_drt


# Seasonal-Year model:
mod_export_yr <- aov(Export ~ Year_fac + Season, data = df_export_seas)

# Check assumptions
df_export_seas %>% model_plotter(Export, "cfs", mod_export_yr)

# Print ANOVA table
anova_export_yr <- Anova(mod_export_yr, type = 2)
anova_export_yr

# Run post hoc test
tukplt_export_yr <- df_export_seas %>% 
  tukey_plotter(
    Export,
    unit_label = "cfs",
    data_type = "Season",
    model = mod_export_yr, 
    model_type = "Year_fac"
  )

tukplt_export_yr

# 3.2 Compare 2021 to Prior Years -----------------------------------------

# Prepare raw Delta exports data
df_export_raw <- df_dayflow_raw %>% filter(!is.na(Export))

# How does 2021 compare to Drought, Normal, and Wet periods?
plt_export_2021_comp_drt <- df_export_raw %>% 
  ggplot(aes(x = Drought_20_21, y = Export, fill = Drought)) +
  geom_boxplot() +
  drt_color_pal_drought() +
  xlab("Drought") +
  scale_y_continuous(name = "Export (cfs)", labels = scales::label_comma()) +
  theme_bw()

plt_export_2021_comp_drt

# Does that change seasonally?
df_export_raw %>% 
  ggplot(aes(x = Drought_20_21, y = Export, fill = Drought)) +
  geom_boxplot() +
  drt_color_pal_drought() +
  facet_grid(cols = vars(Season)) +
  xlab("Drought") +
  scale_y_continuous(name = "Export (cfs)", labels = scales::label_comma()) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

# How does 2021 compare to each water year type?
plt_export_2021_comp_yt <- df_export_raw %>% 
  ggplot(aes(x = YearType_20_21, y = Export, fill = YearType)) +
  geom_boxplot() +
  drt_color_pal_yrtype() +
  xlab("Year Type") +
  scale_y_continuous(name = "Export (cfs)", labels = scales::label_comma()) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

plt_export_2021_comp_yt

# Does that change seasonally?
df_export_raw %>% 
  ggplot(aes(x = YearType_20_21, y = Export, fill = YearType)) +
  geom_boxplot() +
  drt_color_pal_yrtype() +
  facet_grid(cols = vars(Season)) +
  xlab("Year Type") +
  scale_y_continuous(name = "Export (cfs)", labels = scales::label_comma()) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

# 3.3 Export Figures and ANOVA Results ------------------------------------

# Figures:
# Seasonal data by Drought Index and Year
walk2(
  list(plt_export_drt, plt_export_yr),
  c("drt_boxplot", "yr_ts_plot"),
  ~ ggsave(
    plot = .x,
    filename = here("Water_Quality", paste0("Export_season_", .y, ".png")),
    height = 5,
    width = 6,
    units = "in"
  )
)

# Tukey plot for the Drought Index model
ggsave(
  plot = tukplt_export_drt,
  filename = here("Water_Quality/Export_season_drt_model.png"),
  height = 7,
  width = 6,
  units = "in"
)

# Tukey plot for the Year model
ggsave(
  plot = tukplt_export_yr,
  filename = here("Water_Quality/Export_season_year_model.png"),
  height = 10,
  width = 9,
  units = "in"
)

# 2021 comparison plot by Drought Index
ggsave(
  plot = plt_export_2021_comp_drt,
  filename = here("Water_Quality/Export_comp_20_21_drt.png"),
  height = 4,
  width = 5,
  units = "in"
)

# 2021 comparison plot by Water Year Type
ggsave(
  plot = plt_export_2021_comp_yt, 
  filename = here("Water_Quality/Export_comp_20_21_yr_type.png"),
  height = 4, 
  width = 6, 
  units = "in"
)

# ANOVA results:
# Combine results into a data frame and format for export
anova_export_comb <-
  list(
    "Seasonal_Drought" = anova_export_drt,
    "Seasonal_Year" = anova_export_yr
  ) %>% 
  map(~ as_tibble(.x, rownames = "Parameter")) %>% 
  bind_rows(.id = "Model") %>%
  mutate(
    `Pr(>F)` = if_else(`Pr(>F)` < 0.001, "< 0.001", as.character(round(`Pr(>F)`, 4))),
    Parameter = if_else(Parameter == "Year_fac", "Year", Parameter)
  ) %>% 
  dplyr::select(Model, Parameter, `Sum Sq`, Df, `F value`, `Pr(>F)`)

anova_export_comb %>% write_csv(here("Water_Quality/Export_anova_results.csv"), na = "")

# All exported figures and tables were moved to the Drought Synthesis SharePoint site


# 4. X2 position ----------------------------------------------------------

# Prepare X2 position data - seasonal averages
df_x2_seas <- df_dayflow_seas %>% filter(!is.na(X2))

# Plot seasonal X2 position data by year
plt_x2_yr <- df_x2_seas %>% 
  ggplot(aes(x = YearAdj, y = X2, fill = Drought)) +
  geom_point(shape = 21, color = "black") +
  facet_wrap(vars(Season), scales = "free_y") +
  drt_color_pal_drought() +
  xlab("Year") +
  ylab("X2 (km)") +
  theme_bw()

plt_x2_yr

# Plot seasonal X2 position data by drought index
plt_x2_drt <- df_x2_seas %>%
  ggplot(aes(x = Drought, y = X2, fill = Drought)) +
  geom_boxplot() +
  facet_wrap(vars(Season), scales = "free_y") +
  drt_color_pal_drought() +
  ylab("X2 (km)") +
  theme_bw()

plt_x2_drt

# 4.1 ANOVA Analysis ------------------------------------------------------

# Seasonal-Drought Index model:
mod_x2_drt <- aov(X2 ~ Drought + Season, data = df_x2_seas)

# Check assumptions
df_x2_seas %>% model_plotter(X2, "km", mod_x2_drt)

# Print ANOVA table
anova_x2_drt <- Anova(mod_x2_drt, type = 2)
anova_x2_drt

# Run post hoc test
tukplt_x2_drt <- df_x2_seas %>% 
  tukey_plotter(
    X2,
    unit_label = "km",
    data_type = "Season",
    model = mod_x2_drt, 
    model_type = "Drought"
  )

tukplt_x2_drt


# Seasonal-Year model:
mod_x2_yr <- aov(X2 ~ Year_fac + Season, data = df_x2_seas)

# Check assumptions
df_x2_seas %>% model_plotter(X2, "km", mod_x2_yr)

# Print ANOVA table
anova_x2_yr <- Anova(mod_x2_yr, type = 2)
anova_x2_yr

# Run post hoc test
tukplt_x2_yr <- df_x2_seas %>% 
  tukey_plotter(
    X2,
    unit_label = "km",
    data_type = "Season",
    model = mod_x2_yr, 
    model_type = "Year_fac"
  )

tukplt_x2_yr

# 4.2 Compare 2021 to Prior Years -----------------------------------------

# Prepare raw X2 position data
df_x2_raw <- df_dayflow_raw %>% filter(!is.na(X2))

# How does 2021 compare to Drought, Normal, and Wet periods?
plt_x2_2021_comp_drt <- df_x2_raw %>% 
  ggplot(aes(x = Drought_20_21, y = X2, fill = Drought)) +
  geom_boxplot() +
  drt_color_pal_drought() +
  xlab("Drought") +
  ylab("X2 (km)") +
  theme_bw()

plt_x2_2021_comp_drt

# Does that change seasonally?
plt_x2_2021_comp_drt_s <- df_x2_raw %>% 
  ggplot(aes(x = Drought_20_21, y = X2, fill = Drought)) +
  geom_boxplot() +
  drt_color_pal_drought() +
  facet_grid(cols = vars(Season)) +
  xlab("Drought") +
  ylab("X2 (km)") +
  theme_bw() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    legend.position = "top"
  )

plt_x2_2021_comp_drt_s

# How does 2021 compare to each water year type?
plt_x2_2021_comp_yt <- df_x2_raw %>% 
  ggplot(aes(x = YearType_20_21, y = X2, fill = YearType)) +
  geom_boxplot() +
  drt_color_pal_yrtype() +
  xlab("Year Type") +
  ylab("X2 (km)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

plt_x2_2021_comp_yt

# Does that change seasonally?
df_x2_raw %>% 
  ggplot(aes(x = YearType_20_21, y = X2, fill = YearType)) +
  geom_boxplot() +
  drt_color_pal_yrtype() +
  facet_grid(cols = vars(Season)) +
  xlab("Year Type") +
  ylab("X2 (km)") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, hjust = 1, vjust = 0.5))

# 4.3 Export Figures and ANOVA Results ------------------------------------

# Figures:
# Seasonal data by Drought Index and Year
walk2(
  list(plt_x2_drt, plt_x2_yr),
  c("drt_boxplot", "yr_ts_plot"),
  ~ ggsave(
    plot = .x,
    filename = here("Water_Quality", paste0("X2_season_", .y, ".png")),
    height = 5,
    width = 6,
    units = "in"
  )
)

# Tukey plot for the Drought Index model
ggsave(
  plot = tukplt_x2_drt,
  filename = here("Water_Quality/X2_season_drt_model.png"),
  height = 7,
  width = 6,
  units = "in"
)

# Tukey plot for the Year model
ggsave(
  plot = tukplt_x2_yr,
  filename = here("Water_Quality/X2_season_year_model.png"),
  height = 10,
  width = 9,
  units = "in"
)

# 2021 comparison plot by Drought Index
ggsave(
  plot = plt_x2_2021_comp_drt,
  filename = here("Water_Quality/X2_comp_20_21_drt.png"),
  height = 4,
  width = 5,
  units = "in"
)

# 2021 comparison plot by Drought Index facetted by Season
ggsave(
  plot = plt_x2_2021_comp_drt_s,
  filename = here("Water_Quality/X2_comp_20_21_drt_seas.png"),
  height = 5.5,
  width = 6.5,
  units = "in"
)

# 2021 comparison plot by Water Year Type
ggsave(
  plot = plt_x2_2021_comp_yt, 
  filename = here("Water_Quality/X2_comp_20_21_yr_type.png"),
  height = 4, 
  width = 6, 
  units = "in"
)

# ANOVA results:
# Combine results into a data frame and format for export
anova_x2_comb <-
  list(
    "Seasonal_Drought" = anova_x2_drt,
    "Seasonal_Year" = anova_x2_yr
  ) %>% 
  map(~ as_tibble(.x, rownames = "Parameter")) %>% 
  bind_rows(.id = "Model") %>%
  mutate(
    `Pr(>F)` = if_else(`Pr(>F)` < 0.001, "< 0.001", as.character(round(`Pr(>F)`, 4))),
    Parameter = if_else(Parameter == "Year_fac", "Year", Parameter)
  ) %>% 
  dplyr::select(Model, Parameter, `Sum Sq`, Df, `F value`, `Pr(>F)`)

anova_x2_comb %>% write_csv(here("Water_Quality/X2_anova_results.csv"), na = "")

# All exported figures and tables were moved to the Drought Synthesis SharePoint site

