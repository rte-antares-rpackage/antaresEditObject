#' Create a Binding Constraint
#'
#' @param name The name for the binding constraint
#' @param id An id
#' @param values Values used by the constraint.
#'  It contains one line per time step and three columns "less", "greater" and "equal".
#' @param enabled Logical, is the constraint enabled ?
#' @param timeStep Time step the constraint applies to : \code{hourly}, \code{daily} or \code{weekly}
#' @param operator Type of constraint: equality, inequality on one side or both sides.
#' @param coefficients A named vector containing the coefficients used by the constraint.
#' @param overwrite If the constraint already exist, overwrite the previous value.
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
#' createBindingConstraint(
#'   name = "myconstraint", 
#'   values = matrix(data = rep(0, 8760 * 3), ncol = 3), 
#'   enabled = FALSE, 
#'   timeStep = "hourly",
#'   operator = "both",
#'   coefficients = c("fr%myarea" = 1)
#' )
#' }
createBindingConstraint <- function(name, id = tolower(name),
                                    values = NULL,
                                    enabled = TRUE,
                                    timeStep = c("hourly", "daily", "weekly"),
                                    operator = c("both", "equal", "greater", "less"),
                                    coefficients = NULL,
                                    overwrite = FALSE,
                                    opts = antaresRead::simOptions()) {
  
  assertthat::assert_that(class(opts) == "simOptions")
  
  timeStep <- match.arg(arg = timeStep)
  operator <- match.arg(arg = operator)
  
  
  
  ## Ini file
  pathIni <- file.path(opts$inputPath, "bindingconstraints/bindingconstraints.ini")
  bindingConstraints <- readIniFile(pathIni, stringsAsFactors = FALSE)
  
  
  # Get ids and check if not already exist
  previds <- lapply(bindingConstraints, `[[`, "id")
  previds <- unlist(previds, use.names = FALSE)
  if (id %in% previds & !overwrite)
    stop(sprintf("A binding constraint with id '%s' already exist.", id))
  
  if (id %in% previds & overwrite) {
    bc_remove <- which(previds %in% id)
    bindingConstraints[bc_remove] <- NULL
  }
  
  # add the params for the binding constraint ti the ini file
  iniParams <- list(
    name = name,
    id = id,
    enabled = enabled,
    type = timeStep,
    operator = operator
  )
  
  
  # Check coefficients
  if (!is.null(coefficients)) {
    links <- antaresRead::getLinks(opts = opts, namesOnly = TRUE)
    links <- as.character(links)
    links <- gsub(pattern = " - ", replacement = "%", x = links)
    
    #for obscure reasons R CMD check inverse alphabetic order for coefficients
    #test for createPSP() are OK for devools::test() but not for devtools::check()
    #these lines are here to correct this behaviour
    #see https://github.com/r-lib/testthat/issues/144
    #and https://github.com/r-lib/testthat/issues/86
    #set Sys.setenv("R_TESTS" = "") do nothing 
    resLinks <- strsplit(links, "%")
    for(i in seq_along(resLinks)){
      resLinks[[i]] <- paste(resLinks[[i]][2], resLinks[[i]][1], sep = "%")
    }
    links <- c(links, as.character(resLinks))
    
    #Only coef which % are links
    coefficientsToControl <- coefficients[grep("%", names(coefficients))]
    if(length(coefficientsToControl) > 0) {
      if (!all(names(coefficientsToControl) %in% links)) {
        badcoef <- names(coefficientsToControl)[!names(coefficientsToControl) %in% links]
        badcoef <- paste(shQuote(badcoef), collapse = ", ")
        stop(paste0(badcoef, " : is or are not valid link(s)"))
      }
    }
  }
  
  
  indexBC <- as.character(length(bindingConstraints))
  if (indexBC %in% names(bindingConstraints)) {
    indexBC <- as.character(max(as.numeric(names(bindingConstraints))) + 1)
  }
  bindingConstraints[[indexBC]] <- c(iniParams, coefficients)
  
  
  
  
  ## Values
  
  if (!is.null(values)) {
    if (ncol(values) != 3 & is.null(colnames(values))) 
      stop("'values' must have 3 columns or must be named")
    
    if (!is.null(colnames(values))) {
      if (!any(c("less", "greater", "equal") %in% colnames(values))) {
        stop("'value' must have at least one colum named 'greater' or 'less' or 'equal")
      }
      
      var_to_add <- c("less", "greater", "equal")[!c("less", "greater", "equal") %in% colnames(values)]
      
      if (length(var_to_add) > 0) {
        names(var_to_add) <- var_to_add
        values <- do.call("cbind", c(list(values), lapply(var_to_add, function(x) 0)))
      }
      
      values <- values[, c("less", "greater", "equal")]
    }
    
    nrows <- switch(timeStep,
                    hourly = 24*366,
                    daily = 366,
                    weekly = 366,
                    monthly = 12,
                    annual = 1)
    
    if (NROW(values) == 24*365) {
      values <- rbind(values, matrix(rep(0, 24*3), ncol = 3, dimnames = list(list(), names(values))))
    }
    
    if (NROW(values) == 365) {
      values <- rbind(values, matrix(rep(0, 3), ncol = 3, dimnames = list(list(), names(values))))
    }
    
    if (! NROW(values) %in% c(0, nrows)) {
      stop("Incorrect number of rows according to the timeStep")
    }
    
  } else {
    values <- character(0)
  }
  
  
  
  
  
  
  # Write Ini
  writeIni(listData = bindingConstraints, pathIni = pathIni, overwrite = TRUE)
  
  # Write values
  pathValues <- file.path(opts$inputPath, "bindingconstraints", paste0(id, ".txt"))
  write.table(x = values, file = pathValues, col.names = FALSE, row.names = FALSE, sep = "\t")
  
  
  
  # Maj simulation
  suppressWarnings({
    res <- antaresRead::setSimulationPath(path = opts$studyPath, simulation = "input")
  })
  
  invisible(res)
}


