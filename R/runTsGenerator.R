
#' Run Time-Series Generator
#' 
#' @description 
#' `r antaresEditObject:::badge_api_no()`
#'
#' @param path_solver Character containing the Antares Solver path.
#' @param wait Logical, indicating whether the R interpreter should wait for the 
#'   simulation to finish, or run it asynchronously. 
#' @param show_output_on_console Logical, indicating whether to capture the ANTARES log and show 
#'   it on the R console.
#' @param opts List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}.
#'
#' @export
#' 
#' @importFrom antaresRead simOptions
#' @importFrom assertthat assert_that
#'
#' @examples
#' \dontrun{
#' library(antaresRead)
#' setSimulationPath(path = "path/to/study")
#' 
#' library(antaresEditObject)
#' runTsGenerator(
#'   path_solver = "path/to/antares-6.0-solver.exe", 
#'   show_output_on_console = TRUE
#' )
#' }
runTsGenerator <- function(path_solver = getOption("antares.solver"), 
                           wait = TRUE, 
                           show_output_on_console = FALSE, 
                           opts = antaresRead::simOptions()) {
  api_not_implemented(opts)
  if (is.null(path_solver)) {
    path_solver <- setSolverPath()
  }
  # a few checks
  assertthat::assert_that(file.exists(path_solver))

  
  ##Test version of antares solver
  solver <- unlist(gsub("-solver.exe", "", path_solver))
  solver <- strsplit(solver, "antares-")[[1]]
  solver <- solver[[length(solver)]]
  version_solver <- substr(solver, 1, 1)
  version_study <- substr(opts$antaresVersion,1,1)
  
  if(version_solver != version_study){
    stop(paste0(
      "Incompatibility between antares solver version (", version_solver, ") and study version (", version_study, ")"
    ), call. = FALSE)
  }
  
  # Launch TS generators
  cmd <- '"%s" "%s" --generators-only --no-output'
  cmd <- sprintf(cmd, path_solver, opts$studyPath)
  
  system(cmd, ignore.stdout = TRUE, wait = wait, show.output.on.console = show_output_on_console)
}

