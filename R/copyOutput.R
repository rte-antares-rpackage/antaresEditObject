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
#' 
#' @export
copyOutput <- function(opts, extname, mcYears = "all"){
  if (!file.exists(opts$simPath))
    stop("Invalid simulation path, are you sure to have a simulation to copy?", call. = FALSE)
  fil <- paste0(opts$simPath, extname)
  dir.create(fil)
  
  result <- file.copy(
    from = list.files(
      path = opts$simPath,
      full.names = TRUE
    ),
    to = fil, 
    recursive = TRUE
  )
  if (all(result))
    cat("\u2713", "Copy done\n")
  
  suppressWarnings(opts <- antaresRead::setSimulationPath(fil))
  .updateStudyName(opts, extname)
  
  cat("\u2713", "Simulation options updated\n")
  
  invisible(opts)
}


.updateStudyName <- function(opts, ext) {
  iniPath <- file.path(opts$simPath, "info.antares-output")
  infosIni <- readIniFile(iniPath)
  infosIni$general$name <- paste0(infosIni$general$name, ext)
  writeIni(listData = infosIni, pathIni =  iniPath, overwrite = TRUE)
  NULL
}
