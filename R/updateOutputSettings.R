#' Update output parameters of an Antares study
#'
#' @param synthesis Logical. If TRUE, synthetic results will be stored in a
#'   directory Study_name/OUTPUT/simu_tag/Economy/mc-all. If FALSE, No general
#'   synthesis will be printed out.
#' @param storenewset Logical. See Antares General Reference Guide.
#' @param archives Character vector. Series to archive.
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#'
#' @return An updated list containing various information about the simulation.
#' @export
#'
#' @importFrom assertthat assert_that
#'
#' @examples
#' \dontrun{
#'
#' updateOutputSettings(synthesis = TRUE, storenewset = FALSE,
#'                      archives = c("load", "wind"))
#' 
#' }
#'
updateOutputSettings <- function(
  synthesis = NULL,
  storenewset = NULL,
  archives = NULL,
  opts = antaresRead::simOptions()
  ) {
  
  assertthat::assert_that(class(opts) == "simOptions")
  
  pathIni <- file.path(opts$studyPath, "settings", "generaldata.ini")
  general <- readIniFile(file = pathIni)
  
  outputs <- general$output
  outputs$synthesis <- synthesis
  outputs$storenewset <- storenewset
  outputs$archives <- paste(archives, collapse = ", ")
  general$output <- outputs
  
  writeIni(listData = general, pathIni = pathIni, overwrite = TRUE)

  # Maj simulation
  suppressWarnings({
    res <- antaresRead::setSimulationPath(path = opts$studyPath, simulation = "input")
  })
  
  invisible(res)
}
