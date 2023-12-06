#' @title Update output parameters of an Antares study
#' 
#' @description 
#' `r antaresEditObject:::badge_api_ok()`
#' 
#' Update output parameters of an Antares study
#' 
#'
#' @param synthesis Logical. If TRUE, synthetic results will be stored in a
#'   directory Study_name/OUTPUT/simu_tag/Economy/mc-all. If FALSE, No general
#'   synthesis will be printed out.
#' @param storenewset Logical. See Antares General Reference Guide.
#' @param archives Character vector. Series to archive.
#' @param result.format Character. Output format (txt-files or zip).
#' 
#' @template opts
#' 
#' @export
#'
#' @importFrom assertthat assert_that
#'
#' @examples
#' \dontrun{
#'
#' updateOutputSettings(
#'   synthesis = TRUE,
#'   storenewset = FALSE,
#'   archives = c("load", "wind"),
#'   result.format = "zip"
#' )
#' 
#' }
#'
updateOutputSettings <- function(synthesis = NULL,
                                 storenewset = NULL,
                                 archives = NULL,
                                 result.format = NULL,
                                 opts = antaresRead::simOptions()) {
  
  assertthat::assert_that(inherits(opts, "simOptions"))
  
  # API block
  if (is_api_study(opts)) {
    
    writeIni(
      listData = list(
        synthesis = synthesis,
        storenewset = storenewset,
        archives = paste(archives, collapse = ", "),
        `result-format` = result.format
      ),
      pathIni = "settings/generaldata/output",
      opts = opts
    )
    
    return(update_api_opts(opts))
  }
  
  pathIni <- file.path(opts$studyPath, "settings", "generaldata.ini")
  general <- readIniFile(file = pathIni)
  
  outputs <- general$output
  if (!is.null(synthesis))
    outputs$synthesis <- synthesis
  if (!is.null(storenewset))
    outputs$storenewset <- storenewset
  if (!is.null(archives))
    outputs$archives <- paste(archives, collapse = ", ")
  if (!is.null(result.format))
    outputs$`result-format` <- result.format
  general$output <- outputs
  
  writeIni(listData = general, pathIni = pathIni, overwrite = TRUE)

  # Maj simulation
  suppressWarnings({
    res <- antaresRead::setSimulationPath(path = opts$studyPath, simulation = "input")
  })
  
  invisible(res)
}
