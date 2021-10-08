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

# add season col
add_season_col <- function(df) {
  assertthat::assert_that(
    assertthat::is.date(df$Date),
    msg = "add_season_col function\n'Date' variable must be in the date data type."
  )
  
  df <- df %>%
    dplyr::mutate(
      Season =
        case_when(
          lubridate::month(Date) %in% c(12,1,2) ~ 'Winter',
          lubridate::month(Date) %in% c(3,4,5) ~ 'Spring',
          lubridate::month(Date) %in% c(6,7,8) ~ 'Summer',
          lubridate::month(Date) %in% c(9,10,11) ~ 'Fall'
        )
    )
}

add_wy_col <- function(df) {
  assertthat::assert_that(
    assertthat::is.date(df$Date),
    msg = "add_season_col function\n'Date' variable must be in the date data type."
  )
  
  df <- df %>%
    dplyr::mutate(
      WY =
        case_when(
          lubridate::year(Date) %in% c(2011, 2017, 2019) ~ 'wet',
          lubridate::year(Date) %in% c(seq(1976,1977), seq(1987,1992), seq(2007,2009)) ~ 'drought',
          lubridate::year(Date) %in% c(1975, seq(1978,1986), seq(1993,2006), 2010) ~ 'not drought',
          lubridate::year(Date) %in% c(2013, 2014, 2020) ~ 'drought no barrier',
          lubridate::year(Date) %in% c(2015, 2021) ~ 'drought with barrier',
          lubridate::year(Date) %in% c(2012, 2016, 2018) ~ 'in between')
    )
}