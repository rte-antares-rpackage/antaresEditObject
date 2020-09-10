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
#' 
#' @export
copyOputput <- function(opts, extname){
  fil <- paste0(opts$simPath, extname)
  dir.create(fil)
  sapply(list.files(opts$simPath), function(x){
    file.copy(file.path(opts$simPath, x), fil, recursive=TRUE, )
  })
  cat("Done")
}