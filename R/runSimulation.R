#' Run an Antares Simulation
#' 
#' \code{run_simulation} is a function which runs an ANTARES study
#' in economic mode
#' 
#' @param name
#'   Name of the simulation.
#' @param mode
#'   Simulation mode, can take value "economy", "adequacy" or "draft".
#' @param path_solver
#'   Character containing the Antares Solver path
#' @param show_output_on_console
#'   Logical, indicating whether to capture the ANTARES log and show 
#'   it on the R console.
#' @param wait
#'   Logical, indicating whether the R interpreter should wait for the 
#'   simulation to finish, or run it asynchronously. 
#' @param parallel
#'   Logical. If \code{TRUE} the ANTARES simulation will be run in parallel mode (Work
#'   only with ANTARES v6.0.0 or more). In that case, the number of cores used by the simulation
#'   is the one set in advanced_settings/simulation_cores (see ANTARES interface).
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#' 
#' @return 
#' The function does not return anything. It is  used to launch an 
#' ANTARES simulation
#' 
#' @importFrom assertthat assert_that
#' @importFrom antaresRead simOptions
#' @export
#' 
### Taken from antaresXpansion_0.5.2 ###
runSimulation <- function(name, 
                          mode = "economy", 
                          path_solver = getOption("antares.solver"), 
                          wait = TRUE, 
                          show_output_on_console = FALSE, 
                          parallel = TRUE, 
                          opts = antaresRead::simOptions()) {
  if (is.null(path_solver)) {
    path_solver <- setSolverPath()
  }
  # a few checks
  name <- tolower(name)
  assertthat::assert_that(file.exists(path_solver))
  assertthat::assert_that(mode %in% c("economy", "adequacy", "draft", "expansion"))
  
  
  ##Test version of antares solver
  solver <- unlist(gsub("-solver.exe", "", path_solver))
  solver <- strsplit(solver, "antares-")[[1]]
  solver <- solver[[length(solver)]]
  version_solver <- substr(solver, 1, 1)
  version_study <- substr(opts$antaresVersion,1,1)
  
  if(version_solver != version_study){
    stop(paste0(
      "Imcompatibility between antares solver version (", version_solver, ") and study version (", version_study, ")"
    ), call. = FALSE)
  }
  
  
  #Launch simulation
  if(version_solver >= 6 & parallel) {
    cmd <- '"%s" "%s" -n "%s" --%s --parallel'
    cmd <- sprintf(cmd, path_solver, opts$studyPath, name, mode)
  } else {
    cmd <- '"%s" "%s" -n "%s" --%s'
    cmd <- sprintf(cmd, path_solver, opts$studyPath, name, mode)
  }
  
  system(cmd, ignore.stdout = TRUE, wait = wait, show.output.on.console = show_output_on_console)
}
