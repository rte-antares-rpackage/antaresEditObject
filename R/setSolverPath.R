#' Set path to Antares Solver
#'
#' @param path (optional) Path to the solver (e.g. \code{antares-6.0-solver.exe} in \\bin directory where Antares is installed). 
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
  if (!grepl(pattern = "solver\\.exe$", x = path)) {
    path_ <- gsub(pattern = ".*/", replacement = "", x = path)
    cat(paste0("You have selected:\n-> ", path_, "\n"))
    ans <- readline("Are you sure that's the Antares solver? (y/n) ")
    if (ans != "y" & interactive()) {
      setSolverPath()
    } else {
      warning("Unrecognized Antares solver name.")
      options(antares.solver = path)
    }
  } else {
    options(antares.solver = path)
  }
  return(invisible(path))
}

