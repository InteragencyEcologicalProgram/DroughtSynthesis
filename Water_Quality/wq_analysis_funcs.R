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

# helper funcs
unif_analyte <- function(rl){
  val <- stats::runif(1, min = 0.001, max = rl)
  val <- round(val, 3)
  
  return(val)
}

eval_txt <- function(txt){eval(parse(text = txt))}