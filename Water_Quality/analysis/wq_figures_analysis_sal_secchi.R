# Drought Synthesis - Water Quality Figures and Analysis
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
library(patchwork)
library(car)
library(multcomp)
library(multcompView)
library(emmeans)
library(rlang)

# Create model plotter Function
model_plotter <- function(model, data, param_var, unit_name, log_trans = FALSE) {
  data <- data %>%
    mutate(
      Residuals = resid(model),
      Fitted = predict(model)
    )

  param_name <- as_name(ensym(param_var))
  
  if (log_trans == TRUE) {
    resid_label <- paste0("Residuals (log[", unit_name, "])")
  } else {
    resid_label <- paste0("Residuals (", unit_name, ")")
  }
  
  p_hist <- ggplot(data, aes(x = Residuals)) +
    geom_histogram() +
    xlab(resid_label) +
    theme_bw()

  p_res_fit <- ggplot(data, aes(x = Residuals, y = Fitted)) +
    geom_point() +
    ylab(paste0("Predicted ", param_name, " (", unit_name, ")")) +
    xlab(resid_label) +
    theme_bw()

  p_obs_fit <- ggplot(data, aes(x = {{ param_var }}, y = Fitted)) +
    geom_point() +
    geom_abline(slope = 1, intercept = 0, color = "red") +
    ylab(paste0("Predicted ", param_name, " (", unit_name, ")")) +
    xlab(paste0("Observed ", param_name, " (", unit_name, ")")) +
    theme_bw()

  out <- (p_hist + plot_layout(ncol = 1)) + (p_res_fit + p_obs_fit + plot_layout(ncol = 2)) + plot_layout(nrow = 2, widths = c(1, 0.5, 0.5))

  return(out)
}

# Create tukey plotter Function
tukey_plotter <- function(model, data, data_type, model_type) {
  tuk <- emmeans(model, list(data = data_type, model = model_type))

  tuk_data <- as_tibble(cld(tuk$data, sort = FALSE, Letters = letters)) %>%
    mutate(.group = str_remove_all(.group, fixed(" "))) %>%
    left_join(data %>%
      group_by(across(all_of(data_type))) %>%
      summarise(max_sal = max(Salinity), .groups = "drop"),
    by = data_type
    )

  tuk_model <- as_tibble(cld(tuk$model, sort = FALSE, Letters = letters)) %>%
    mutate(.group = str_remove_all(.group, fixed(" "))) %>%
    left_join(data %>%
      group_by(across(all_of(model_type))) %>%
      summarise(max_sal = max(Salinity), .groups = "drop"),
    by = model_type
    )

  p_data <- ggplot(tuk_data, aes(x = .data[[data_type]], y = emmean, ymin = lower.CL, ymax = upper.CL, label = .group)) +
    geom_boxplot(data = data, aes(x = .data[[data_type]], y = Salinity), inherit.aes = FALSE) +
    geom_pointrange(color = "red", position = position_nudge(x = 0.1)) +
    geom_text(aes(y = max_sal + (max(data$Salinity) - min(data$Salinity)) / 20), size = 6) +
    ylab("Salinity (PSU)") +
    theme_bw(base_size = 16)

  p_model <- ggplot(tuk_model, aes(x = .data[[model_type]], y = emmean, ymin = lower.CL, ymax = upper.CL, label = .group)) +
    geom_boxplot(data = data, aes(x = .data[[model_type]], y = Salinity), inherit.aes = FALSE) +
    geom_pointrange(color = "red", position = position_nudge(x = 0.1)) +
    geom_text(aes(y = max_sal + (max(data$Salinity) - min(data$Salinity)) / 20), angle = if_else(model_type == "Year_fac", 90, 0), hjust = if_else(model_type == "Year_fac", "left", NA_character_), vjust = 0.25, size = 6) +
    ylab("Salinity (PSU)") +
    theme_bw(base_size = 16) +
    {
      if (model_type == "Year_fac") {
        list(
          geom_tile(
            data = data,
            aes(
              x = Year_fac, y = min(Salinity) - (max(Salinity) - min(Salinity)) / 20,
              fill = Drought, height = (max(Salinity) - min(Salinity)) / 20
            ),
            inherit.aes = FALSE
          ),
          xlab("Year"),
          theme(axis.text.x = element_text(angle = 90, vjust = 0.5)),
          scale_y_continuous(expand = expansion(mult = c(0, 0.1))),
          drt_color_pal_drought()
        )
      }
    }

  out <- p_data / p_model + plot_annotation(tag_levels = "A")

  if (model_type == "Year_fac") {
    out <- out + plot_layout(heights = c(0.8, 1))
  }

  return(out)
}
