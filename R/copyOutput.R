#' Copy of the output files of an Antares study
#'
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#' @param extname Extension to be added to the name of the study, to be used as a name for the newly created folder.
#' @param mcYears mcYears to copy. Can be \code{"all"}.
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
#' copyOutput(opts, "_adq")
#' 
#' }
#' @import fs
#' 
#' @export
copyOutput <- function(opts, extname, mcYears = "all"){
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
  
  opts <- antaresRead::setSimulationPath(fil)
  .updateStudyName(opts, extname)
  
  cat("Copy done")
  
  opts
}




#' @noRd
.updateStudyName <- function(opts, ext)
{
  iniPath <- file.path(opts$simPath, "info.antares-output")
  infosIni <- readIniFile(iniPath)
  infosIni$general$name <- paste0(infosIni$general$name, ext)
  writeIni(listData = infosIni, pathIni =  iniPath, overwrite = TRUE)
  NULL
}