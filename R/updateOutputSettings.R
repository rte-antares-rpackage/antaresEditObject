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
#'   synthesis will be printed out. See Antares General Reference Guide (see link below).
#' @param storenewset Logical. See Antares General Reference Guide (see link below).
#' @param archives Character vector. Series to archive. See Antares General Reference Guide (see link below).
#' @param result.format Character. Output format (txt-files or zip). See Antares General Reference Guide (see link below).
#' 
#' @template opts
#' 
#' @export
#'
#' @importFrom assertthat assert_that
#' @importFrom utils modifyList
#' @importFrom antaresRead readIniFile
#'
#' @seealso \href{https://antares-simulator.readthedocs.io/en/latest/user-guide/solver/04-parameters/}{Antares General Reference Guide}
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
  
  new_params <- list(
    synthesis = synthesis,
    storenewset = storenewset,
    archives = archives,
    result.format = result.format
  )
  
  new_params <- dropNulls(x = new_params)
  new_params <- lapply(X = new_params, FUN = .format_ini_rhs)
  names(new_params) <- sapply(names(new_params), dicoOutputSettings, USE.NAMES = FALSE)
  
  res <- update_generaldata_by_section(opts = opts, section = "output", new_params = new_params)
  
  invisible(res)
}


#' Correspondence between arguments of \code{updateOutputSettings} and actual Antares parameters.
#' 
#' @param arg An argument from function \code{updateOutputSettings}.
#'
#' @return The corresponding Antares general parameter.
#' 
#' @export
#' 
#' @examples 
#' dicoOutputSettings("result.format") # "result-format"
dicoOutputSettings <- function(arg) {
  
  if (length(arg) > 1) { 
    stop("'arg' must be length one")
  }
  
  antares_params <- as.list(c("synthesis", "storenewset", "archives", "result-format"))
  
  names(antares_params) <- c("synthesis", "storenewset", "archives", "result.format")
  
  return(antares_params[[arg]])
}
