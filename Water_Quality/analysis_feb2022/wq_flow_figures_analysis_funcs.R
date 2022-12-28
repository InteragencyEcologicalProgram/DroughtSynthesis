# Drought Synthesis - Water Quality
# Purpose: Functions to be used for creating plots of ANOVA model diagnostics
  # and Tukey post-hoc results
# Author: Sam Bashevkin and Dave Bosworth
# Contacts: Sam.Bashevkin@deltacouncil.ca.gov, David.Bosworth@water.ca.gov

# Load packages
library(dplyr)
library(rlang)
library(ggplot2)
library(patchwork)
library(emmeans)
library(multcomp)
library(stringr)

# Model plotter function - ANOVA model diagnostics
model_plotter <- function(df_data, param_var, unit_label, model, log_trans = FALSE) {
  df_data <- df_data %>%
    mutate(
      Residuals = resid(model),
      Fitted = predict(model)
    )
  
  param_name <- as_name(ensym(param_var))
  
  if (log_trans == TRUE) {
    resid_label <- paste0("Residuals (log[", unit_label, "])")
  } else {
    resid_label <- paste0("Residuals (", unit_label, ")")
  }
  
  plt_hist <- ggplot(df_data, aes(x = Residuals)) +
    geom_histogram() +
    labs(
      title = "Residual Histogram", 
      x = "Residuals"
    ) +
    theme_bw()
  
  plt_qq <- ggplot(df_data, aes(sample = Residuals)) + 
    labs(
      title = "Residual Probability Plot", 
      x = "Normal Quantiles", 
      y = "Residuals"
    ) + 
    stat_qq() + 
    stat_qq_line(
      size = 1, 
      color = 'red'
    ) + 
    theme_bw()
  
  plt_res_fit <- ggplot(df_data, aes(x = Fitted, y = Residuals)) +
    geom_point() +
    labs(
      title = "Residuals vs. Fitted Values", 
      x = (paste0("Predicted ", param_name, " (", unit_label, ")")),
      y = resid_label
    ) +
    theme_bw()
  
  plt_obs_fit <- ggplot(df_data, aes(x = Fitted, y = {{ param_var}})) +
    geom_point() +
    geom_abline(slope = 1, intercept = 0, color = "red") +
    labs(
      title = "Observed vs. Fitted Values", 
      x = paste0("Predicted ", param_name, " (", unit_label, ")"),
      y = paste0("Observed ", param_name, " (", unit_label, ")")
    ) +
    theme_bw()
  
  (plt_hist + plt_qq) / (plt_res_fit + plt_obs_fit)
}

# Tukey plotter function - Tukey post-hoc results
tukey_plotter <- function(df_data, 
                          param_var, 
                          param_label = NULL,
                          unit_label, 
                          data_type = c("Region", "Season"),
                          model,
                          model_type = c("Drought", "Year_fac"), 
                          log_trans = FALSE) {
  # Argument checking
  data_type <- arg_match(data_type)
  model_type <- arg_match(model_type)
  
  # Run emmeans Tukey post-hoc for each main effect in the model
  emm_tuk <- emmeans(model, list(tuk_data_type = data_type, tuk_model_type = model_type))
  
  # Add significance grouping letters from the Tukey post-hoc results and
  # vertical positioning for each main effect
  lst_tuk_f <- emm_tuk %>% 
    map(
      ~ as_tibble(cld(.x, sort = FALSE, Letters = letters)) %>% 
        mutate(.group = str_remove_all(.group, fixed(" ")))
    ) %>% 
    map2(
      c(data_type, model_type),
      ~ left_join(
        .x,
        df_data %>% 
          group_by(.data[[.y]]) %>% 
          summarize(max_val = max({{ param_var }}), .groups = "drop"),
        by = .y
      )
    ) %>% 
    map(~ mutate(.x, max_val = if_else(upper.CL > max_val, upper.CL, max_val)))
  
  # Define min, max, and range of response variable to be used for plot formatting
  max_param_var <- df_data %>% summarize(max_var = max({{ param_var }})) %>% pull(max_var)
  min_param_var <- df_data %>% summarize(min_var = min({{ param_var }})) %>% pull(min_var)
  range_param_var <- max_param_var - min_param_var
  
  # Define the y-axis label for both plots
  if (is.null(param_label)) param_label <- as_name(ensym(param_var))
  
  if (log_trans == TRUE) {
    y_label <- paste0("log(", param_label, " [", unit_label, "])")
  } else {
    y_label <- paste0(param_label, " (", unit_label, ")")
  }
  
  # Create boxplot showing Tukey post-hoc results for the data_type (Regional or
  # Seasonal averages)
  plt_data <- lst_tuk_f$tuk_data_type %>% 
    ggplot(
      aes(
        x = .data[[data_type]], 
        y = emmean, 
        ymin = lower.CL, 
        ymax = upper.CL, 
        label = .group
      )
    ) +
    geom_boxplot(
      data = df_data, 
      aes(x = .data[[data_type]], y = {{ param_var }}), 
      inherit.aes = FALSE
    ) +
    geom_pointrange(
      color = "red", 
      position = position_nudge(x = 0.1),
      size = 0.3
    ) +
    geom_text(aes(y = max_val + range_param_var / 20)) +
    ylab(y_label) +
    theme_bw()
  
  # Create boxplot showing Tukey post-hoc results for the model_type (Drought
  # Index or Year)
  plt_model <- lst_tuk_f$tuk_model_type %>% 
    ggplot(
      aes(
        x = .data[[model_type]], 
        y = emmean, 
        ymin = lower.CL, 
        ymax = upper.CL, 
        label = .group
      )
    ) +
    geom_boxplot(
      data = df_data, 
      aes(x = .data[[model_type]], y = {{ param_var }}), 
      inherit.aes = FALSE
    ) +
    geom_pointrange(
      color = "red", 
      position = position_nudge(x = 0.1),
      size = 0.3
    ) +
    geom_text(
      aes(y = max_val + range_param_var / 20), 
      angle = if_else(model_type == "Year_fac", 90, 0), 
      hjust = if_else(model_type == "Year_fac", "left", NA_character_), 
      vjust = 0.25
    ) +
    ylab(y_label) +
    theme_bw()
  
  # Add tile plot to bottom of plot and apply additional formatting if model
  # type is Year
  if (model_type == "Year_fac") {
    # Define vertical positioning of the tile plot
    if (min(lst_tuk_f$tuk_model_type$lower.CL) < min_param_var) {
      y_pos_tile <- min(lst_tuk_f$tuk_model_type$lower.CL) - range_param_var / 20
    } else {
      y_pos_tile <- min_param_var - range_param_var / 20
    }
    
    plt_model <- plt_model +
      geom_tile(
        data = df_data,
        aes(
          x = Year_fac,
          y = y_pos_tile,
          fill = Drought, 
          height = range_param_var / 20
        ),
        inherit.aes = FALSE
      ) +
      xlab("Year") +
      theme(axis.text.x = element_text(angle = 90, vjust = 0.5)) +
      scale_y_continuous(expand = expansion(mult = c(0, 0.1))) +
      drt_color_pal_drought()
  }
  
  plt_f <- plt_data / plt_model + plot_annotation(tag_levels = "A")
  
  if (model_type == "Year_fac") {
    plt_f <- plt_f + plot_layout(heights = c(0.8, 1))
  }
  
  return(plt_f)
}

