`%>%` <- purrr::`%>%`
`%!in%` <- Negate(`%in%`)

# BOXPLOTS

# calc outliers
is_outlier <- function(x) {
  outlier <- (x < quantile(x, 0.25) - 1.5 * IQR(x) | x > quantile(x, 0.75) + 1.5 * IQR(x))
  
  return(outlier)
}

# calc data for boxplot
# (adapted from NADA function 'cenboxplot')
calc_bp_data <- function(obs, cen, group) {
  # change group to factor
  group_factor <- as.factor(group)
  # initalize empty vectors to populate
  data <- numeric()
  groups <- character()
  
  if (length(levels(group_factor)) > 0) { # if there's any data for any group
    
    for (i in levels(group_factor)) { # for each level of the group
      # define the boolean where T = censored and F = uncensored for the group
      uncen_data <- cen[group == i]
      if (length(uncen_data) > 0) {
        # if there's any data
        
        if (all(uncen_data)) {
          # if it's all censored data, skip
          next
        }
        else if (all(!uncen_data)) { # if all uncensored data
          # populate the vectors using all values
          data <- c(data, obs[group == i])
          grp <- rep(i, length(obs[group == i]))
          groups <- c(groups, grp)
        }
        else { # if mix between censored/uncensored data
          # populate the vectors using ROS model values
          mod <- suppressWarnings(NADA::cenros(obs[group == i], cen[group == i])$modeled)
          grp <- rep(i, length(mod))
          data <- c(data, mod)
          groups <- c(groups, grp)
        }
      }
    }
    
    # define df to return with all data/groups
    df <- data.frame(Data = data, Group = groups)
    return(df)
  }
}

# clean boxplot df
gen_bp_data <- function(df, analyte, group, metadat = NULL) {
  # clean up df so it works in the functions
  df <- df %>%
    dplyr::mutate(Group = .data[[group]]) %>%
    dplyr::filter(!is.na(.data[[analyte]]))

  sign_col <- paste0(analyte, '_Sign')
  
  # create RL df
  df_rl <- df %>%
    dplyr::group_by(Group) %>%
    dplyr::filter(.data[[sign_col]] == '<') %>%
    dplyr::mutate(RL = max(.data[[analyte]], na.rm = TRUE), .groups = 'drop') %>%
    dplyr::select(select = Group,RL) %>%
    dplyr::rename(Group = select) %>%
    unique() %>%
    dplyr::ungroup()

  # create metadata df
  df_metadat <- df %>%
    dplyr::group_by(Group) %>%
    dplyr::select(select = Group,metadat,group) %>%
    dplyr::rename(Group = select) %>%
    unique() %>%
    dplyr::ungroup()
  
  # check if RL data exists
  RL_dat <- nrow(df_rl) > 0
  
  # define vectors for cenfit function
  obs <- dplyr::pull(df, .data[[analyte]])
  cen <- dplyr::mutate(df, sign_col = .data[[sign_col]] == '<') %>% dplyr::pull(sign_col)
  group <- df$Group
  metadata <- df[metadat]

  # calculate values to use in boxplot (ie. account for non-detects)
  df_data <- calc_bp_data(obs, cen, group)

  # merge data
  if (is.null(df_data)) { # skip if no data
  }
  else{
    df_boxplt <- dplyr::left_join(df_data, df_metadat, by = 'Group')
    df_boxplt <- dplyr::left_join(df_boxplt, df_rl, by = 'Group')
    # add boolean outlier column
    df_boxplt <- df_boxplt %>%
      dplyr::group_by(Group) %>%
      dplyr::mutate(Outlier = ifelse(is_outlier(Data), Data, as.numeric(NA))) %>%
      dplyr::ungroup()
  }
  return(df_boxplt)
}

# plot boxplot
create_bp <- function(df_boxplt, x, fill = NA, wrap = NA, show_outliers = TRUE){
  # df_boxplt$Group <- factor(df_boxplt$Group)

  # plot boxplots
  bp <- ggplot2::ggplot()
  # add box plot data
  bp <- bp +
    ggplot2::geom_boxplot( # box plot
      df_boxplt,
      mapping = ggplot2::aes(x = .data[[x]], y = Data, group = Group, fill = .data[[fill]]), #TODO: fix fill issue (see em boxplot for soln)
      outlier.shape = NA
    )

  # add outliers if wanted
  if(show_outliers){
    bp <- bp +
      ggplot2::geom_point( # outlier points
        df_boxplt,
        mapping = ggplot2::aes(x = .data[[x]], y = Outlier, group = Group, fill = .data[[fill]]),
      )
  }
  
  # create RL df
  if(!is.na(wrap)){ # better way to do this?
    df_rl <- df_boxplt %>%
      dplyr::select(Group, RL, .data[[x]], .data[[wrap]]) %>%
      dplyr::rename(X = .data[[x]]) %>%
      unique()
  }
  else{
    df_rl <- df_boxplt %>%
      dplyr::select(Group, RL, .data[[x]]) %>%
      dplyr::rename(X = .data[[x]]) %>%
      unique()
  }

  x_max <- max(as.numeric(as.factor(df_rl$X)), na.rm = TRUE)

  df_shading <- data.frame(
    min = seq(from = 0.5, to = x_max, by = 1),
    max = seq(from = 1.5, to = x_max + 0.5, by = 1),
    ymax = df_rl$RL,
    Group = as.factor(df_rl$Group)
  )

  df_shading <- dplyr::left_join(df_shading, subset(df_boxplt, select = -c(Data,Outlier)), by = 'Group') %>% unique()
  df_shading$Group <- factor(df_shading$Group, levels = unique(df_boxplt$Group))
  RL_dat <- nrow(df_rl) > 0

  # add RL aesthetics
  if (RL_dat) {

    bp <- bp +
      ggplot2::geom_rect(
        data = df_shading,
        ggplot2::aes(xmin = min, xmax = max, ymin = -Inf, ymax = ymax, group = Group),
        fill = 'white',
        alpha = 0.75
      ) +
      ggplot2::geom_segment( # line at RL
        data = df_shading,
        ggplot2::aes(x = min, xend = max, y = ymax, yend = ymax, group = Group),
        size = 0.75,
        color = '#b85656'
      )

    if(!is.na(wrap)){
      bp <- bp + ggplot2::facet_wrap(~.data[[wrap]], scales = 'free')
    }
    
  }
  
  return(bp)
}

# RANDOM
assign_full_name <- function(short_name){
  if (short_name == 'DissAmmonia'){
    full_name <- 'Dissolved Ammonia'
  }
  else if (short_name == 'DissNitrateNitrite') {
    full_name <- 'Dissolved Nitrate Nitrite'
  }
  else if (short_name == 'DissOrthophos') {
    full_name <- 'Dissolved Orthophosphate'
  }
}

assign_cutoffs <- function(short_name){
  if (short_name == 'DissAmmonia'){
    cutoff <- 0.5
  }
  else if (short_name == 'DissNitrateNitrite') {
    cutoff <- 3.5
  }
  else if (short_name == 'DissOrthophos') {
    cutoff <- 0.4
  }
}

# -- EMM Plots --
emm_plotter <-
  function (model,
            df_data,
            analyte,
            grouping,
            emm_alpha = 0.05,
            y,
            fill = NA,
            fill_type = 'boxplot',
            rect_gap = 0,
            adjust = 'Sidak',
            pt_color = 'red',
            fill_alpha = 1,
            position_nudge = 0.1,
            text_size = 5,
            nudge_y = 0.05,
            line_size = .6,
            fatten = 2) {

    # check that fill type is valid argument
    if (fill_type %!in% c('boxplot', 'rect')) {
      stop('fill_rect must be in c("boxplot", "rect")')
    }
    
    # change fill to factor if not already
    if(!is.na(fill)) {
      if (!is.factor(pull(df_data, .data[[fill]]))) {
        df_data[fill] <- as.factor(dplyr::pull(df_data, .data[[fill]]))
      }
    }

    # calc data
    df_emm <- emm_data(model, df_data, analyte, grouping, emm_alpha, adjust = 'Sidak')

    # create plot (fills)
    plt <- ggplot2::ggplot()

    if (is.na(fill) | fill_type == 'rect') {
      plt <- plt +
        ggplot2::geom_boxplot(data = df_data,
                              mapping = aes(x = .data[[grouping]], y = .data[[analyte]]))
    } else if (!is.na(fill) & fill_type == 'boxplot') {
      plt <- plt +
        ggplot2::geom_boxplot(data = df_data,
                              mapping = aes(x = .data[[grouping]], y = .data[[analyte]], fill = .data[[fill]]),
                              alpha = fill_alpha)
    }
    
    if (fill_type == 'rect' & !is.na(fill)) {
      x_max <- max(as.numeric(as.factor(pull(df_data, .data[[grouping]]))), na.rm = TRUE)
      
      if (grouping == fill) {
        df_shading <- data.frame(
          xmin = seq(from = 0.5, to = x_max, by = 1),
          xmax = seq(from = 1.5, to = x_max + 0.5, by = 1),
          ymax = pull(df_data, .data[[analyte]]) %>% min(., na.rm = TRUE) - rect_gap,
          Fill = levels(pull(df_data, .data[[fill]]))
        )
      } else{
        df_shading <- data.frame(
          xmin = seq(from = 0.5, to = x_max, by = 1),
          xmax = seq(from = 1.5, to = x_max + 0.5, by = 1),
          ymax = pull(df_data, .data[[analyte]]) %>% min(., na.rm = TRUE) - rect_gap,
          Fill = pull(df_data, .data[[fill]])
        )
      }
      
      plt <- plt +
        ggplot2::geom_rect(data = df_shading,
                           mapping = aes(xmin = xmin, xmax = xmax, ymin = -Inf, ymax = ymax, fill = Fill))
    }
    
    # finish plot
    plt <- plt +
      ggplot2::geom_pointrange(
        data = df_emm,
        aes(x = .data[[grouping]], y = emmean, ymin = lower.CL, ymax = upper.CL),
        color = pt_color,
        size = line_size,
        fatten = fatten,
        position = position_nudge(x = position_nudge)
      ) +
      ggplot2::geom_text(
        data = df_emm,
        aes(x = .data[[grouping]], y = analyte_max, label = group),
        size = text_size,
        nudge_y = nudge_y
      )
    
    return(plt)
  }

# -- Year Barplots --
dual_year_plts <- function(plt_reg, plt_seas, type, angle = 0){
  legend <- cowplot::get_legend(plt_reg + guides(color = guide_legend(nrow = 1)) + theme(legend.position = 'top'))
  
  plt_reg <- plt_reg + ylab(NULL) + xlab(paste(type, '(Regional Averages)')) + theme(axis.text.x = element_text(angle = angle, hjust = 1), legend.position = 'none')
  plt_seas <- plt_seas + xlab(paste(type, '(Seasonal Averages)')) + theme(axis.text.x = element_text(angle = angle, hjust = 1), legend.position = 'none')
  
  int <- cowplot::plot_grid(plotlist = list(plt_seas, plt_reg), labels = 'auto', align = 'vh', hjust = -1, nrow = 1)
  
  plt_both <- cowplot::plot_grid(legend, int, ncol = 1, rel_heights = c(.1, .9))
}

quad_year_plts <- function(plt_lt_di, plt_st_yr, plt_lt_avg, plt_st_avg, type, txt_size = 10, label_size = 14, angle = 0){
  legend <- cowplot::get_legend(plt_lt_di + guides(color = guide_legend(nrow = 1)) + theme(legend.position = 'top', text = element_text(size = txt_size)))
  
  plt_lt_di <- plt_lt_di +
    xlab(paste0('Drought (', type, 'al Averages)')) +
    theme(
      axis.text.x = element_text(angle = angle, hjust = 1),
      legend.position = 'none',
      text = element_text(size = txt_size)
    )
  
  plt_st_yr <- plt_st_yr +
    ylab(NULL) +
    xlab(paste0('Year (', type, 'al Averages)')) +
    theme(
      axis.text.x = element_text(angle = angle, hjust = 1),
      legend.position = 'none',
      text = element_text(size = txt_size)
    )
  
  plt_lt_avg <- plt_lt_avg +
    xlab(paste(type, '(Long Term)')) +
    theme(
      axis.text.x = element_text(angle = angle, hjust = 1),
      legend.position = 'none',
      text = element_text(size = txt_size)
    )
  
  plt_st_avg <- plt_st_avg +
    ylab(NULL) + xlab(paste(type, '(Short Term)')) +
    theme(
      axis.text.x = element_text(angle = angle, hjust = 1),
      legend.position = 'none',
      text = element_text(size = txt_size)
    )
  
  plt_list <- list(plt_lt_di, plt_st_yr, plt_lt_avg, plt_st_avg)
  
  int <- cowplot::plot_grid(plotlist = plt_list, labels = 'auto', label_size = label_size, align = 'vh', hjust = -1, nrow = 2)
  
  plt_quad <- cowplot::plot_grid(legend, int, ncol = 1, rel_heights = c(.1, .9))
}