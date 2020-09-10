library(antaresRead)
opts = setSimulationPath("C:/Users/TitouanRobert/Desktop/Projet/RTE/antares/etude/MAF2025_S3_FBCOREMOD/output/20200831-1727eco-test elia2")

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
copyOputput <- function(opts, extname){
  fil <- paste(opts$simPath, "toto")
  dir.create(fil)
  sapply(list.files(opts$simPath), function(x){
    file.copy(file.path(opts$simPath, x), fil, recursive=TRUE, )
  })
  cat("Done")
}