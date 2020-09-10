#' Create An Area In An Antares Study
#'
#' @param opts file opts obtain with antaresRead::setSimulationPath
#' @param extname extention name for study duplicated
#' 
#' 
#' @examples
#' \dontrun{
#' 
#' library(antaresRead)
#' 
#' # Set simulation path
#' opts = setSimulationPath(path = "PATH/TO/SIMULATION", simulation = "input")
#' 
#' # Create a new area
#' copyOputput(opts, "_adq")
#' 
#' }
#' @import fs
#' 
#' @export
copyOputput <- function(opts, extname){
  fil <- paste0(opts$simPath, extname)
  dir.create(fil)
  
  sapply(list.files(opts$simPath), function(x){
    dd <- file.path(opts$simPath, x)
    if(fs::is_dir(dd)){
      fs::dir_copy(dd, fil)
    }else{
      fs::file_copy(dd, fil)
    }
  })
  
  cat("Done")
}