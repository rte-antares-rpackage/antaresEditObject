#' Update An Existing Binding Constraint
#'
#' @param name The name for the binding constraint
#' @param id An id
#' @param values Values used by the constraint.
#'  It contains one line per time step and three columns "less", "greater" and "equal".
#' @param enabled Logical, is the constraint enabled ?
#' @param timeStep Time step the constraint applies to : \code{hourly}, \code{daily} or \code{weekly}
#' @param operator Type of constraint: equality, inequality on one side or both sides.
#' @param coefficients A named vector containing the coefficients used by the constraint.
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath} 
#'
#' @return An updated list containing various information about the simulation.
#' @export
#' 
#' @importFrom antaresRead getLinks setSimulationPath
#' @importFrom utils write.table
#'
#' @examples
#' \dontrun{
#' editBindingConstraint(
#'   name = "toto", 
#'   values = matrix(data = rep(0, 8760 * 3), ncol = 3), 
#'   enabled = FALSE, 
#'   timeStep = "hourly",
#'   operator = "both",
#'   coefficients = c("fr%de" = 1)
#' )
#' }
editBindingConstraint <- function(name,
                                    id = tolower(name),
                                    values = NULL,
                                    enabled = NULL,
                                    timeStep = NULL,
                                    operator = NULL,
                                    coefficients = NULL,
                                    opts = antaresRead::simOptions()) {
  valuesIn <- values
  # Ini file
  pathIni <- file.path(opts$inputPath, "bindingconstraints/bindingconstraints.ini")
  bindingConstraints <- readIniFile(pathIni, stringsAsFactors = FALSE)
  previds <- lapply(bindingConstraints, `[[`, "id")
  previds <- unlist(previds, use.names = FALSE)
  if(!id %in% previds){
    stop("Binding constraint with id '", id, "' doesn't exist in current study.")
  }
  
  # Update general params
  bc_update_pos <- which(previds %in% id)
  bc_update <- bindingConstraints[[bc_update_pos]]
  
  iniParams <- list(
    name = bc_update$name,
    id = bc_update$id,
    enabled = bc_update$enabled,
    type = bc_update$type,
    operator = bc_update$operator
  )
  
  
  if(!is.null(name)) iniParams$name <- name
  if(!is.null(id)) iniParams$id <- id
  if(!is.null(enabled)) iniParams$enabled <- enabled
  if(!is.null(timeStep)) iniParams$type <- timeStep
  if(!is.null(operator)) iniParams$operator <- operator
  
  bindingConstraints[[bc_update_pos]]$name <- iniParams$name
  bindingConstraints[[bc_update_pos]]$id <- iniParams$id
  bindingConstraints[[bc_update_pos]]$enabled <- iniParams$enabled
  bindingConstraints[[bc_update_pos]]$type <- iniParams$type
  bindingConstraints[[bc_update_pos]]$operator <- iniParams$operator
  
  if(!is.null(coefficients)){
    
    links <- antaresRead::getLinks(opts = opts, namesOnly = TRUE)
    links <- as.character(links)
    links <- gsub(pattern = " - ", replacement = "%", x = links)
    resLinks <- strsplit(links, "%")
    
    for(i in seq_along(resLinks)){
      resLinks[[i]] <- paste(resLinks[[i]][2], resLinks[[i]][1], sep = "%")
    }
    links <- c(links, as.character(resLinks))
    
    coefficientsToControl <- coefficients[grep("%", names(coefficients))]
    if(length(coefficientsToControl) > 0) {
      if (!all(names(coefficientsToControl) %in% links)) {
        badcoef <- names(coefficientsToControl)[!names(coefficientsToControl) %in% links]
        badcoef <- paste(shQuote(badcoef), collapse = ", ")
        stop(paste0(badcoef, " : is/are not valid link(s)"))
      }
    }
    
    for(i in names(coefficients)){
      bindingConstraints[[bc_update_pos]][[i]] <- coefficients[i]
    }
  }
  
  values <- .valueCheck(values, bindingConstraints[[bc_update_pos]]$type)
  
  # Write Ini
  writeIni(listData = bindingConstraints, pathIni = pathIni, overwrite = TRUE)
  
  # Write values
  pathValues <- file.path(opts$inputPath, "bindingconstraints", paste0(id, ".txt"))
  
  if(!is.null(valuesIn))write.table(x = values, file = pathValues, col.names = FALSE, row.names = FALSE, sep = "\t")
  
  # Maj simulation
  suppressWarnings({
    res <- antaresRead::setSimulationPath(path = opts$studyPath, simulation = "input")
  })
  
}