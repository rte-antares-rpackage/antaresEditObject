#' @title Remove a Binding Constraint
#' 
#' @description 
#' `r antaresEditObject:::badge_api_ok()`
#' 
#' Remove a binding constraint in an Antares study.
#' 
#' 
#' @param name Name(s) of the binding constraint(s) to remove.
#' 
#' @template opts
#' 
#' @family binding constraints functions
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' removeBindingConstraint("mybindingconstraint")
#' }
removeBindingConstraint <- function(name, opts = antaresRead::simOptions()) {
  
  assertthat::assert_that(inherits(opts, "simOptions"))
  
  # API block
  if (is_api_study(opts)) {
    
    for (i in name) {
      cmd <- api_command_generate(
        "remove_binding_constraint", 
        id = i
      )
      
      api_command_register(cmd, opts = opts)
      `if`(
        should_command_be_executed(opts), 
        api_command_execute(cmd, opts = opts, text_alert = "remove_binding_constraint: {msg_api}"),
        cli_command_registered("remove_binding_constraint")
      )
    }
    
    return(invisible(opts))
  }
  
  ## Ini file
  pathIni <- file.path(opts$inputPath, "bindingconstraints/bindingconstraints.ini")
  bindingConstraints <- readIniFile(pathIni, stringsAsFactors = FALSE)
  
  namesbc <- unlist(lapply(bindingConstraints, `[[`, "name"), use.names = FALSE)
  
  # suppression txt files + remove constraint from ini file
  for (i in name) {
    if (! i %in% namesbc) {
      warning(paste0("No binding constraint with name '", i, "'"))
    } else {
      index <- which(namesbc == i)
      id <- bindingConstraints[[index]]$id
      bindingConstraints[[index]] <- NULL
      # v870
      if(opts$antaresVersion>=870){
        path_lt <- file.path(opts$inputPath, 
                             sprintf("bindingconstraints/%s.txt", 
                                     paste0(id, "_lt")))
        path_gt <- file.path(opts$inputPath, 
                             sprintf("bindingconstraints/%s.txt", 
                                     paste0(id, "_gt")))
        path_eq <- file.path(opts$inputPath, 
                             sprintf("bindingconstraints/%s.txt", 
                                     paste0(id, "_eq")))
        lapply(c(path_lt, path_gt, path_eq), 
               unlink)
      }else{
        pathValues <- file.path(opts$inputPath, "bindingconstraints", paste0(id, ".txt"))
        unlink(x = pathValues)
      }
      
      namesbc <- unlist(lapply(bindingConstraints, `[[`, "name"), use.names = FALSE)
      names(bindingConstraints) <- as.character(seq_along(bindingConstraints) - 1)
    }
  }
  
  # Write Ini
  writeIni(listData = bindingConstraints, pathIni = pathIni, overwrite = TRUE)
  
  
  # Maj simulation
  suppressWarnings({
    res <- antaresRead::setSimulationPath(path = opts$studyPath, simulation = "input")
  })
  
  invisible(res)
}
