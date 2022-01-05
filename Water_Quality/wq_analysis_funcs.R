# -- Replace RLs --
replace_rl <- function(df_wq, val, seed = 42) {
  # define variables
  col_val <- paste0('df_wq$', val)
  col_sign <- paste0('df_wq$', val, '_Sign')
  col_zip <- mapply(list, eval_txt(col_sign), eval_txt(col_val), SIMPLIFY=F)
  
  # replace unknown RLs with 0.01, since that's the closest RL chronologically
  col_rls <- lapply(col_zip, function(x) ifelse(x[[1]] == '<' & is.na(x[[2]]), 0.01, x[[2]]))
  col_rls <- unname(unlist(col_rls))
  df_wq[val] <- col_rls
  
  # replace RLs with simulated value
  set.seed(seed)
  col_new <- lapply(col_zip, function(x) ifelse(!is.na(x[[2]]) & x[[1]] == '<', unif_analyte(x[[2]]), x[[2]]))
  col_new <- unname(unlist(col_new))
  
  return(col_new)
}

library(emmeans)

# -- Model Plots (from Sam) --
model_plotter <- function(model, data){
  data<-data%>%
    mutate(Residuals=resid(model),
           Fitted=predict(model))
  
  p_hist<-ggplot(data, aes(x=Residuals))+
    geom_histogram()+
    # xlab("Residuals (°C)")+
    theme_bw()
  
  p_res_fit<-ggplot(data, aes(x=Residuals, y=Fitted))+
    geom_point()+
    # ylab("Predicted temperature (°C)")+
    # xlab("Residuals (°C)")+
    theme_bw()
  
  p_obs_fit<-ggplot(data, aes(x=Temperature, y=Fitted))+
    geom_point()+
    geom_abline(slope=1, intercept=0, color="red")+
    # ylab("Predicted temperature (°C)")+
    # xlab("Observed temperature (°C)")+
    theme_bw()
  
  out<-(p_hist+plot_layout(ncol=1))+(p_res_fit+p_obs_fit+plot_layout(ncol=2))+plot_layout(nrow=2, widths=c(1, 0.5, 0.5))
  
  return(out)
}

# -- EMM Func --
emm_data <- function(model, df, analyte, grouping, adjust = 'Tukey'){
  # run post-hoc test
  emm <- emmeans::emmeans(model, specs = stats::reformulate(grouping), adjust = adjust)
  
  # create df of emmeans output
  df_emm <- dplyr::tibble(multcomp::cld(emm, sort = FALSE, Letters = letters))
  
  # extract max vals from df for graphing purposes
  df <- df %>%
    dplyr::group_by(across(all_of(grouping))) %>%
    dplyr::summarize(analyte_max = max(.data[[analyte]], na.rm = TRUE))
  
  # clean up df_emm and merge w/ df
  df_emm <- df_emm %>%
    dplyr::mutate(.group = stringr::str_remove_all(.group, fixed(' '))) %>% # remove empty groups
    dplyr::left_join(df, by = grouping)
  
  df_emm <- df_emm %>% rename(group = .group)
  
  return(df_emm)
}

# -- Helper Funcs --
unif_analyte <- function(rl){
  val <- stats::runif(1, min = 0.001, max = rl)
  val <- round(val, 3)
  
  return(val)
}

eval_txt <- function(txt){eval(parse(text = txt))}

# -- Write AOV to File --
aov_as_df <- function(aov, response){
  df_aov <- as.data.frame(aov)
  df_aov$`Response Var` <- response
  df_aov$`Dependent Var` <- rownames(df_aov)
  df_aov <- df_aov %>% dplyr::relocate(`Response Var`, `Dependent Var`)
  df_aov <- df_aov %>% dplyr::mutate(
    Signif = dplyr::case_when(
      `Pr(>F)` < 0.001 ~ '< 0.001',
      `Pr(>F)` > 0.001 &
        `Pr(>F)` < 0.01 ~ '< 0.01',
      `Pr(>F)` > 0.01 & `Pr(>F)` < 0.05 ~ '< 0.05',
      `Pr(>F)` > 0.05 & `Pr(>F)` < 0.1 ~ '< 0.1',
      `Pr(>F)` > 0.1 ~ '> 0.1 (NS)'
    )
  )
  
  return(df_aov)
}

write_aov <- function(df_aov, filepath, space = FALSE, append = FALSE){
  col_names <- ifelse(append, FALSE, TRUE)
  
  if (space) {
    blank_row <- rep(' ', length(df_aov))
    df_aov <- rbind(blank_row, df_aov)
  }
  
  readr::write_csv(df_aov, filepath, append = append, col_names = col_names)
  return(NULL)
}