

# RES utilities functions -------------------------------------------------

#' @title Activate RES in an Antares study
#' 
#' @description Helper to activate Renewables Energy Sources. This will
#'  update `renewable.generation.modelling` parameter and create 
#'  appropriate structure for RES clusters.
#' @param quietly Display or not a message to the user if success.
#'
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#'
#' @return An updated list containing various information about the simulation.
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' library(antaresEditObject)
#' tmp <- tempfile()
#' createStudy(path = tmp)
#' opts <- antaresRead::setSimulationPath(tmp)
#' activateRES()
#' 
#' # then you can use createClusterRES()...
#' 
#' }
activateRES <- function(opts = antaresRead::simOptions(), quietly = !interactive()) {
  assertthat::assert_that(inherits(opts, "simOptions"))
  updateOptimizationSettings(renewable.generation.modelling = "clusters")
  initialize_RES(opts)
  if (!isTRUE(quietly))
    cat("\u2713", "Renewables Energy Sources activated\n")
  invisible(opts)
}


initialize_RES <- function(opts) {
  if (is_api_study(opts)) {
    # no need in API mode
    return(invisible(TRUE))
  }
  inputPath <- opts$inputPath
  ren_dir <- file.path(inputPath, "renewables")
  dir.create(ren_dir, showWarnings = FALSE)
  dir.create(file.path(ren_dir, "clusters"), showWarnings = FALSE)
  areas <- antaresRead::getAreas(opts = opts)
  for (area in areas) {
    dir.create(file.path(inputPath, "renewables", "clusters", tolower(area)), showWarnings = FALSE)
    path_ini <- file.path(inputPath, "renewables", "clusters", tolower(area), "list.ini")
    if (!file.exists(path_ini))
      writeLines(character(0), con = path_ini)
  }
  dir.create(file.path(ren_dir, "series"), showWarnings = FALSE)
  return(invisible(TRUE))
}

is_active_RES <- function(opts) {
  if (is_api_study(opts)) {
    rgm <- api_get_raw_data(
      id = opts$study_id,
      path = "settings/generaldata/other preferences/renewable-generation-modelling",
      opts = opts
    )
    identical(rgm, "clusters")
  } else {
    generaldatapath <- file.path(opts$studyPath, "settings", "generaldata.ini")
    generaldata <- readIniFile(file = generaldatapath)
    rgm <- generaldata$`other preferences`$`renewable-generation-modelling`
    !is.null(rgm) && identical(rgm, "clusters")
  }
}

check_active_RES <- function(opts, check_dir = FALSE) {
  if (opts$antaresVersion < 810)
    stop("Renewable Energy Sources is only available if using Antares >= 8.1.0", call. = FALSE)
  if (!is_active_RES(opts)) {
    stop(
      "Renewable Energy Sources is not activated, you cannot create/edit a renewable cluster or update scenario builder for renewables for example.",
      "\nPlease use updateOptimizationSettings() to set `renewable-generation-modelling` parameter to 'clusters' or use activateRES().",
      call. = FALSE
    )
  }
  if (is_api_study(opts))
    check_dir <- FALSE
  if (isTRUE(check_dir)) {
    inputPath <- opts$inputPath
    ren_dir <- file.path(inputPath, "renewables")
    if (!dir.exists(ren_dir)) {
      stop(
        "There is no 'renewables' directory in the study, are you sure you have renewable clusters enabled?",
        call. = FALSE
      )
    }
  }
  return(invisible(TRUE))
}
