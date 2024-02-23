#' @title Create a binding constraint
#' 
#' @description 
#' `r antaresEditObject:::badge_api_ok()`
#' 
#' Create a new binding constraint in an Antares study.
#' `createBindingConstraintBulk()` allow to create multiple constraints at once.
#' 
#'
#' @param name The name for the binding constraint.
#' @param id An id, default is to use the name.
#' @param values Values used by the constraint.
#'  It contains one line per time step and three columns "less", "greater" and "equal".
#' @param enabled Logical, is the constraint enabled ?
#' @param timeStep Time step the constraint applies to : `hourly`, `daily` or `weekly`.
#' @param operator Type of constraint: equality, inequality on one side or both sides.
#' @param filter_year_by_year Marginal price granularity for year by year
#' @param filter_synthesis Marginal price granularity for synthesis
#' @param coefficients A named vector containing the coefficients used by the constraint, the coefficients have to be alphabetically ordered.
#' @param overwrite If the constraint already exist, overwrite the previous value.
#' 
#' @template opts
#' 
#' @seealso [editBindingConstraint()] to edit existing binding constraints, [removeBindingConstraint()] to remove binding constraints.
#' 
#' @export
#' 
#' @name create-binding-constraint
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
#' 
#' # Create multiple constraints
#' 
#' # Prepare data for constraints 
#' bindings_constraints <- lapply(
#'   X = seq_len(100),
#'   FUN = function(i) {
#'     # use arguments of createBindingConstraint()
#'     # all arguments must be provided !
#'     list(
#'       name = paste0("constraints", i), 
#'       id = paste0("constraints", i), 
#'       values = matrix(data = rep(0, 8760 * 3), ncol = 3), 
#'       enabled = FALSE, 
#'       timeStep = "hourly",
#'       operator = "both",
#'       coefficients = c("area1%area2" = 1),
#'       overwrite = TRUE
#'     )
#'   }
#' )
#' # create all constraints
#' createBindingConstraintBulk(bindings_constraints)
#' }
createBindingConstraint <- function(name, 
                                    id = tolower(name),
                                    values = NULL,
                                    enabled = TRUE,
                                    timeStep = c("hourly", "daily", "weekly"),
                                    operator = c("both", "equal", "greater", "less"),
                                    filter_year_by_year = "hourly, daily, weekly, monthly, annual",
                                    filter_synthesis = "hourly, daily, weekly, monthly, annual",
                                    coefficients = NULL,
                                    overwrite = FALSE,
                                    opts = antaresRead::simOptions()) {
  
  assertthat::assert_that(inherits(opts, "simOptions"))
  
  timeStep <- match.arg(arg = timeStep)
  operator <- match.arg(arg = operator)
  
  ## Values
  values <- .valueCheck(values, timeStep)
  
  if(!is.null(coefficients)){
    names_coef <- names(coefficients)
    splitted_names <- strsplit(names_coef, "%")
    are_areas_sorted <- sapply(splitted_names, function(areas) {
      identical(areas, sort(areas))
    })
    
    if (!all(are_areas_sorted)) {
      stop("The areas are not sorted alphabetically.", call. = FALSE)
    }
  }

  # API block
  if (is_api_study(opts)) {
    
    cmd <- api_command_generate(
      "create_binding_constraint", 
      name = name,
      enabled = enabled,
      time_step = timeStep,
      operator = operator,
      values = values,
      coeffs = lapply(as.list(coefficients), as.list)
    )
    
    api_command_register(cmd, opts = opts)
    `if`(
      should_command_be_executed(opts), 
      api_command_execute(cmd, opts = opts, text_alert = "create_binding_constraint: {msg_api}"),
      cli_command_registered("create_binding_constraint")
    )
    
    return(invisible(opts))
  }
  
  ## Ini file
  pathIni <- file.path(opts$inputPath, "bindingconstraints/bindingconstraints.ini")
  bindingConstraints <- readIniFile(pathIni, stringsAsFactors = FALSE)
  
  bindingConstraints <- createBindingConstraint_(
    bindingConstraints,
    name,
    id,
    values,
    enabled,
    timeStep,
    operator,
    filter_year_by_year,
    filter_synthesis,
    coefficients,
    overwrite,
    links = antaresRead::getLinks(opts = opts, namesOnly = TRUE),
    opts = opts
  )
  
  # Write Ini
  writeIni(listData = bindingConstraints, pathIni = pathIni, overwrite = TRUE)
  
  # Maj simulation
  suppressWarnings({
    res <- antaresRead::setSimulationPath(path = opts$studyPath, simulation = "input")
  })
  
  invisible(res)
}


createBindingConstraint_ <- function(bindingConstraints,
                                     name,
                                     id,
                                     values,
                                     enabled,
                                     timeStep,
                                     operator,
                                     filter_year_by_year = "hourly, daily, weekly, monthly, annual",
                                     filter_synthesis = "hourly, daily, weekly, monthly, annual",
                                     coefficients,
                                     overwrite,
                                     links,
                                     opts) {
  
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
  
  # Marginal price granularity (v8.3.2)
  if (opts$antaresVersion >= 832){
    iniParams$`filter-year-by-year` <- filter_year_by_year
    iniParams$`filter-synthesis` <- filter_synthesis
  }
  
  # Check coefficients
  if (!is.null(coefficients)) {
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
  
  
  # Write values
  pathValues <- file.path(opts$inputPath, "bindingconstraints", paste0(id, ".txt"))
  data.table::fwrite(x = data.table::as.data.table(values), file = pathValues, col.names = FALSE, row.names = FALSE, sep = "\t")
  
  return(bindingConstraints)
}


.valueCheck <- function(values, timeStep) {
  
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
  values
}



#' @param constraints A `list` of several named `list` containing data to create binding constraints.
#'  **Warning** all arguments for creating a binding constraints must be provided, see examples.
#' @export
#' 
#' @rdname create-binding-constraint
createBindingConstraintBulk <- function(constraints,
                                        opts = antaresRead::simOptions()) {
  assertthat::assert_that(inherits(opts, "simOptions"))
  ## Ini file
  pathIni <- file.path(opts$inputPath, "bindingconstraints/bindingconstraints.ini")
  bindingConstraints <- readIniFile(pathIni, stringsAsFactors = FALSE)
  
  for (i in seq_along(constraints)) {
    bindingConstraints <- do.call("createBindingConstraint_", c(
      constraints[[i]],
      list(
        opts = opts, 
        bindingConstraints = bindingConstraints,
        links = antaresRead::getLinks(opts = opts, namesOnly = TRUE)
      )
    ))
  }
  
  writeIni(listData = bindingConstraints, pathIni = pathIni, overwrite = TRUE)
  
  suppressWarnings({
    res <- antaresRead::setSimulationPath(path = opts$studyPath, simulation = "input")
  })
  invisible(res)
}













