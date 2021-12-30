`%>%` <- purrr::`%>%`
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
    tidyr::unite(Group, group, remove = FALSE) %>%
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
create_bp <- function(df_boxplt, x, fill = NULL, wrap = NULL, show_outliers = TRUE){
  # plot boxplots
  bp <- ggplot2::ggplot()
  # add box plot data
  bp <- bp +
    ggplot2::geom_boxplot( # box plot
      df_boxplt,
      mapping = ggplot2::aes(x = .data[[x]], y = Data, group = Group, fill = .data[[fill]]),
      outlier.shape = NA
      # fill = '#ededed',
      # lwd = 0.8,
      # fatten = 1.5
    )
  
  # add outliers if wanted
  if(show_outliers){
    bp <- bp +
      ggplot2::geom_point( # outlier points
        df_boxplt,
        mapping = ggplot2::aes(x = .data[[x]], y = Outlier, group = Group, fill = .data[[fill]]),
        # size = 2.8,
        # shape = 21,
        # stroke = 1.15
      )
  }
  
  # create RL df
  if(length(wrap) > 0){ # better way to do this?
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
    Group = df_rl$Group
  )
  
  df_shading <- dplyr::left_join(df_shading, subset(df_boxplt, select = -c(Data,Outlier)), by = 'Group') %>% unique()
  
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
    if(length(wrap) > 0){
      bp <- ggplot2::facet_wrap(~.data[[wrap]], scales = 'free')
    }
      
  }
  
  return(bp)
}