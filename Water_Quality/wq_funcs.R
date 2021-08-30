# Functions for WQ data
# Authors: Dave Bosworth, Sarah Perry

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