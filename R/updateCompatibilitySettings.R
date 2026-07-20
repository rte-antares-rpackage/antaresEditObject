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
  
  new_params <- dropNulls(list(
    hydro.pmax = hydro.pmax,
    hydro.rule.curves = hydro.rule.curves
  ))
  
  for (i in seq_along(new_params)) {
    new_params[[i]] <- as.character(new_params[[i]])
    names(new_params)[i] <- dicoCompatibilitySettings(names(new_params)[i])[["property"]]
  }
  
  res <- update_generaldata_by_section(opts = opts, section = "compatibility", new_params = new_params)
  
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
