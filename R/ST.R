

# ST-storages utilities functions -------------------------------------------------

#' @title Activate ST-storages in an Antares study
#' 
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
#' activateST()
#' 
#' # then you can use createClusterST()...
#' 
#' }
activateST <- function(opts = antaresRead::simOptions(), quietly = !interactive()) {
  assertthat::assert_that(inherits(opts, "simOptions"))
  initialize_ST(opts)
  if (!isTRUE(quietly))
    cat("\u2713", "Short Term Storages Sources activated\n")
  invisible(opts)
}


initialize_ST <- function(opts) {
  if (is_api_study(opts)) {
    # no need in API mode
    return(invisible(TRUE))
  }
  inputPath <- opts$inputPath
  st_dir <- file.path(inputPath, "ST-storages")
  dir.create(st_dir, showWarnings = FALSE)
  dir.create(file.path(st_dir, "clusters"), showWarnings = FALSE)
  areas <- antaresRead::getAreas(opts = opts)
  for (area in areas) {
    dir.create(file.path(inputPath, "ST-storages", "clusters", tolower(area)), showWarnings = FALSE)
    path_ini <- file.path(inputPath, "ST-storages", "clusters", tolower(area), "list.ini")
    if (!file.exists(path_ini))
      writeLines(character(0), con = path_ini)
  }
  dir.create(file.path(st_dir, "series"), showWarnings = FALSE)
  return(invisible(TRUE))
}


check_active_ST <- function(opts, check_dir = FALSE) {
  if (opts$antaresVersion < 860)
    stop("Short Term Storage Sources is only available if using Antares >= 8.6.0", call. = FALSE)
  if (is_api_study(opts))
    check_dir <- FALSE
  if (isTRUE(check_dir)) {
    inputPath <- opts$inputPath
    st_dir <- file.path(inputPath, "ST-storages")
    if (!dir.exists(st_dir)) {
      stop(
        "There is no 'ST-storages' directory in the study.",
        call. = FALSE
      )
    }
  }
  return(invisible(TRUE))
}
