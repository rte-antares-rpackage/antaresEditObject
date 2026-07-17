#' @title Update compatibility parameters of an Antares study
#' 
#' @description 
#' `r antaresEditObject:::badge_api_ok()`
#' 
#' Update compatibility parameters of an Antares study
#'
#' @param hydro.pmax daily or hourly
#' @param hydro.rule.curves single or scenarized
#'
#' @template opts
#'
#' @seealso \href{https://antares-simulator.readthedocs.io/en/latest/user-guide/04-migration-guides/#new-optional-feature-and-compatibility-flag-for-scenarized-hydro-reservoir-levels}{Compatibility parameters}
#'
#' @export
#'
#' @importFrom utils modifyList
#' @importFrom assertthat assert_that
#' @importFrom antaresRead setSimulationPath readIniFile
#'
#' @examples
#' \dontrun{
#' 
#' updateCompatibilitySettings(
#'   hydro.pmax = "hourly", 
#'   hydro.rule.curves = "scenarized"
#' )
#' 
#' }
updateCompatibilitySettings <- function(hydro.pmax = NULL,
                                        hydro.rule.curves = NULL,
                                        opts = antaresRead::simOptions()) {
  assertthat::assert_that(inherits(opts, "simOptions"))
  
  # check inputs

  if (!is.null(hydro.pmax)) {
    if (opts$antaresVersion < 920) {
      stop("updateCompatibilitySettings: hydro.pmax parameter is only available if using Antares >= 9.2", call. = FALSE)
    }
    .check_property_value_compatibility_settings(property = "hydro.pmax", value = hydro.pmax)    
  }
  
  if (!is.null(hydro.rule.curves)) {
    if (opts$antaresVersion < 101000) {
      stop("updateCompatibilitySettings: hydro.pmax parameter is only available if using Antares >= 10.1", call. = FALSE)
    }
    .check_property_value_compatibility_settings(property = "hydro.rule.curves", value = hydro.rule.curves)
  }
  
  new_params_compatibility <- dropNulls(list(
    hydro.pmax = hydro.pmax,
    hydro.rule.curves = hydro.rule.curves
  ))
  
  for (i in seq_along(new_params_compatibility)) {
    new_params_compatibility[[i]] <- as.character(new_params_compatibility[[i]])
    names(new_params_compatibility)[i] <- dicoCompatibilitySettings(names(new_params_compatibility)[i])[["property"]]
  }
  
  
  # API block
  if (is_api_study(opts)) {
    
    if (length(new_params_compatibility) > 0) {
      writeIni(
        listData = new_params_compatibility,
        pathIni = "settings/generaldata/compatibility",
        opts = opts
      )
    }
    
    return(update_api_opts(opts))
  }
  
  
  # read
  generaldatapath <- file.path(opts$studyPath, "settings", "generaldata.ini")
  generaldata <- readIniFile(file = generaldatapath)
  
  # previous parameters
  l_compatibility <- generaldata$compatibility
  
  l_compatibility <- utils::modifyList(x = l_compatibility, val = new_params_compatibility)
  
  generaldata$compatibility <- l_compatibility
  
  # write
  writeIni(
    listData = generaldata,
    pathIni = generaldatapath,
    overwrite = TRUE
  )
  
  # Maj simulation
  suppressWarnings({
    res <- antaresRead::setSimulationPath(path = opts$studyPath, simulation = "input")
  })
  
  invisible(res)
}


#' Correspondence between arguments of \code{updateCompatibilitySettings} and actual Antares parameters.
#'
#' @param arg An argument from function \code{updateCompatibilitySettings}.
#'
#' @return The corresponding Antares general parameter.
#'
#' @export
#'
#' @examples
#' dicoCompatibilitySettings("hydro.pmax") # "year-by-year"
dicoCompatibilitySettings <- function(arg) {
  
  if (length(arg) > 1) {
    stop("'arg' must be length one")
  }
  
  antares_params <- list(
    "hydro.pmax" = list("property" = "hydro-pmax",
                        "values" = c("daily", "hourly")
                        ),
    "hydro.rule.curves" = list("property" = "hydro-rule-curves",
                               "values" = c("single", "scenarized")
                               )
  )
  
  antares_params[[arg]]
}


#' @importFrom assertthat assert_that
.check_property_value_compatibility_settings <- function(property, value) {

  dico_compatibility <- dicoCompatibilitySettings(arg = property)
  assert_that(value %in% dico_compatibility[["values"]],
              msg = paste0(value, " is not an authorized value")
             )
}
