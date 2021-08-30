# Functions for WQ data
# Authors: Dave Bosworth, Sarah Perry

#' @importFrom magrittr %>%

# read in absolute fp
drought_abs_path <- function(fp_rel = NULL) {
  fp_fastr <- 'California Department of Water Resources/Drought Synthesis - Documents/'
  
  if (is.null(fp_rel)) {
    fp_abs <- normalizePath(file.path(Sys.getenv('USERPROFILE'), fp_fastr))
  } else {
    fp_abs <- normalizePath(file.path(Sys.getenv('USERPROFILE'), fp_fastr, fp_rel))
  }
  
  return(fp_abs)
}

add_season_col <- function(df) {
  assertthat::assert_that(
    assertthat::is.date(df$Date),
    msg = "add_season_col function\n'Date' variable must be in the date data type."
  )
  
  df <- df %>%
  dplyr::mutate(
    Season =
      case_when(
        month(Date) %in% c(12,1,2) ~ 'Winter',
        month(Date) %in% c(3,4,5) ~ 'Spring',
        month(Date) %in% c(6,7,8) ~ 'Summer',
        month(Date) %in% c(9,10,11) ~ 'Fall'
      )
  )
}