#' Remove a Binding Constraint
#'
#' @param name Name(s) of the binding constraint(s) to remove.
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath} 
#'
#' @return An updated list containing various information about the simulation.
#' @export
#'
#' @examples
#' \dontrun{
#' removeBindingConstraint("mybindingconstraint")
#' }
removeBindingConstraint <- function(name, opts = antaresRead::simOptions()) {
  
  assertthat::assert_that(class(opts) == "simOptions")
  
  ## Ini file
  pathIni <- file.path(opts$inputPath, "bindingconstraints/bindingconstraints.ini")
  bindingConstraints <- readIniFile(pathIni, stringsAsFactors = FALSE)
  
  namesbc <- unlist(lapply(bindingConstraints, `[[`, "name"), use.names = FALSE)
  
  for (i in name) {
    if (! i %in% namesbc) {
      warning(paste0("No binding constraint with name '", i, "'"))
    } else {
      index <- which(namesbc == i)
      id <- bindingConstraints[[index]]$id
      bindingConstraints[[index]] <- NULL
      pathValues <- file.path(opts$inputPath, "bindingconstraints", paste0(id, ".txt"))
      unlink(x = pathValues)
      namesbc <- unlist(lapply(bindingConstraints, `[[`, "name"), use.names = FALSE)
      names(bindingConstraints) <- as.character(seq_along(bindingConstraints) - 1)
    }
  }
  
  # Write Ini
  writeIni(listData = bindingConstraints, pathIni = pathIni, overwrite = TRUE)
  
  
  # Maj simulation
  res <- antaresRead::setSimulationPath(path = opts$studyPath, simulation = "input")
  
  invisible(res)
}