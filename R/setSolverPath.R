#' Set path to Antares Solver
#'
#' @param path (optional) Path to the solver (e.g. 'antares-6.0-solver.exe' in \\bin directory where Antares is installed). 
#' If missing, a window opens and lets the user choose the directory of the simulation interactively.
#'
#' @export
#'
#' @examples
#' 
#' \dontrun{
#' 
#' setSolverPath(path = "C:/antares/bin/antares-6.0-solver.exe")
#' 
#' }
#' 
setSolverPath <- function(path) {
  if (missing(path)) {
    path <- file.choose()
  }
  options(antares.solver = path)
}

