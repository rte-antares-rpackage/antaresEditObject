#' @title Create a binding constraint
#' 
#' @description 
#' `r antaresEditObject:::badge_api_ok()`  
#' `r lifecycle::badge("experimental")` 
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
#' @param coefficients A named vector containing the coefficients used by the constraint.
#' @param group "character" group of the constraint, default value : "default group"
#' @param overwrite If the constraint already exist, overwrite the previous value.
#' 
#' @template opts
#' 
#' @seealso [editBindingConstraint()] to edit existing binding constraints, 
#' [removeBindingConstraint()] to remove binding constraints.
#' 
#' @details 
#' According to Antares version, usage may vary :
#' 
#' **< v8.7.0** : For each constraint name, a .txt file containing 3 time series `"less", "greater", "equal"`
#' 
#' **>= v8.7.0** : For each constraint name, one file .txt containing `<id>_lt.txt, <id>_gt.txt, <id>_eq.txt`  
#' Parameter `values` must be named `list` ("lt", "gt", "eq") containing `data.frame` scenarized.  
#' see example section below.
#' 
#' @export
#' 
#' @name createBindingConstraint
#' 
#' @importFrom antaresRead getLinks setSimulationPath
#' @importFrom utils write.table
#'
#' @examples
#' \dontrun{
#' # < v8.7.0 :
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
#' 
#' # >= v8.7.0 :
#' 
#' # values are now named list containing `data.frame` according to   
#'  # `operator` parameter (for "less", build a list with at least "lt" floor in list)
#'  
#' # data values (hourly)
#' df <- matrix(data = rep(0, 8760 * 3), ncol = 3)
#' values_data <- list(lt=df)
#'  
#' # create bc with minimum value
#' createBindingConstraint(name = "bc_example", 
#'                         operator = "less", 
#'                         values = values_data, 
#'                         overwrite = TRUE)
#'                         
#' # or you can provide list data with all value
#' values_data <- list(lt=df, 
#'                    gt= df, 
#'                    eq= df)   
#'                    
#' createBindingConstraint(name = "bc_example", 
#'                         operator = "less", 
#'                         values = values_data, 
#'                         overwrite = TRUE)      
#'                         
#' # create multiple constraints
#' bindings_constraints <- lapply(
#'   X = seq_len(10),
#'   FUN = function(i) {
#'     # use arguments of createBindingConstraint()
#'     # all arguments must be provided !
#'     list(
#'       name = paste0("constraints_bulk", i), 
#'       id = paste0("constraints_bulk", i), 
#'       values = values_data, 
#'       enabled = FALSE, 
#'       timeStep = "hourly",
#'       operator = "both",
#'       coefficients = c("at%fr" = 1),
#'       group= "group_bulk",
#'      overwrite = TRUE
#'    )
#'   }
#' )
#'  
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
                                    group = NULL,
                                    overwrite = FALSE,
                                    opts = antaresRead::simOptions()) {
  
  # check input parameters
  assertthat::assert_that(inherits(opts, "simOptions"))
  
  timeStep <- match.arg(arg = timeStep)
  operator <- match.arg(arg = operator)
  
  # API block
  if (is_api_study(opts)) {
    
    cmd <- api_command_generate(
      "create_binding_constraint", 
      name = name,
      enabled = enabled,
      time_step = timeStep,
      operator = operator,
      filter_year_by_year = filter_year_by_year,
      filter_synthesis = filter_synthesis,
      group = group,
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
  
  # v870
  if(opts$antaresVersion>=870){
    if(is.null(group))
      group <- "default"
    
    values_operator <- switch(operator,
                              less = "lt",
                              equal = "eq",
                              greater = "gt",
                              both = c("lt", "gt"))
    
    if(!is.null(values)){
      assertthat::assert_that(inherits(values, "list"))
      ##
      # check "values" according to "operator"
      ##
      if(!all(values_operator %in% names(values)))
        stop(paste0(
          "you must provide a list named according your parameter 'operator'  : ",
          "'", operator, "'",
          " with "), 
          paste0("'", values_operator, "'", collapse = " "),
          call. = FALSE)
      
      # v870 : check group and values
        # no check for add BC with NULL values
      group_values_check(group_value = group, 
                         values_data = values,
                         operator_check = operator,
                         output_operator = values_operator,
                         opts = opts)
    }
  }
  
  ##
  # build properties + write values
  ##
  
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
    group,
    overwrite,
    links = antaresRead::getLinks(opts = opts, namesOnly = TRUE),
    output_operator = values_operator,
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


#' @importFrom data.table fwrite
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
                                     group,
                                     overwrite,
                                     links,
                                     output_operator = NULL,
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
  
  # v870
  if(opts$antaresVersion>=870)
    iniParams$group <- group
  
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
 
  # check when overwrite element in list 
    # names of bindingConstraints are provided by R automatically
  indexBC <- as.character(length(bindingConstraints))
  if (indexBC %in% names(bindingConstraints)) {
    indexBC <- as.character(max(as.numeric(names(bindingConstraints))) + 1)
  }
  
  # add new bc to write then in .ini file
  bindingConstraints[[indexBC]] <- c(iniParams, coefficients)
  
  ## Values
  if(opts$antaresVersion>=870 & !is.null(values))
    values <- .valueCheck870(values, timeStep)
  else
    values <- .valueCheck(values, timeStep)
  
  # Write values
  # v870
  if(opts$antaresVersion>=870){
    # names_order_ts <- c("lt", "gt", "eq")
    name_file <- paste0(id, "_", output_operator, ".txt")
    
    up_path <- file.path(opts$inputPath, "bindingconstraints", name_file)
    
    lapply(up_path, function(x, df_ts= values, vect_path= up_path){
      if(identical(df_ts, character(0)))
        fwrite(x = data.table::as.data.table(df_ts), 
               file = x, 
               col.names = FALSE, 
               row.names = FALSE, 
               sep = "\t")
      else{
        index <- grep(x = up_path, pattern = x)
        fwrite(x = data.table::as.data.table(df_ts[[index]]), 
               file = x, 
               col.names = FALSE, 
               row.names = FALSE, 
               sep = "\t")
      }
    })
  }else{
    pathValues <- file.path(opts$inputPath, "bindingconstraints", paste0(id, ".txt"))
    data.table::fwrite(x = data.table::as.data.table(values), 
                       file = pathValues, 
                       col.names = FALSE, 
                       row.names = FALSE, 
                       sep = "\t")
  }
  return(bindingConstraints)
}



#' @title Check dimension of time series for binding constraints
#' @description Only needed for study version >= 870
#' @param group_value `character` name of group
#' @param values_data `list` values used by the constraint
#' @param operator_check `character` parameter "operator"
#' @param output_operator `character` for 
#' @return NULL if it's new group to add or error exceptions with dimension control
#' @template opts
#' @export 
#' @keywords internal
group_values_check <- function(group_value, 
                               values_data,
                               operator_check,
                               output_operator,
                               opts = antaresRead::simOptions()){
  
  # no check if col dim ==1
  if(operator_check%in%"both"){
    if(dim(values_data$lt)[2] <= 1)
      return()
  }else{
    if(dim(values_data[[output_operator]])[2] <= 1)
      return()
  }
    
  
  # read existing binding constraint
    # /!\/!\ function return "default values" (vector of 0)
  existing_bc <- readBindingConstraints(opts = opts)
  
  # study with no BC or virgin study
  if(is.null(existing_bc))
    return()
  
  ##
  # group creation
  ##
  
  # check existing group Versus new group 
  existing_groups <- unlist(
    lapply(existing_bc, 
           function(x){
             x[["properties"]][["group"]]})
    )
  search_group_index <- grep(pattern = group_value, 
                             x = existing_groups)
  
  # new group ? 
  new_group <- identical(search_group_index, 
                         integer(0))
  if(new_group)
    message("New group ", "'", group_value, "'", " will be created")
  
  # check dimension values existing group Versus new group 
  if(!new_group){
    # check dimension of existing group
    p_col <- sapply(existing_bc[search_group_index], 
                    function(x){
                      op <- x[["properties"]][["operator"]]
                      if(!op %in%"both")
                        dim(x[["values"]])[2]
                      else{
                        lt_dim <- dim(x[["values"]][["less"]])[2]
                        gt_dim <- dim(x[["values"]][["greater"]])[2]
                        if(lt_dim!=gt_dim)
                          stop("dimension of values are not similar for constraint : ", 
                               x$properties$id, call. = FALSE)
                        lt_dim
                      }
                      })
    
    # keep dimension >1 
    names(p_col) <- NULL
    if(identical(p_col[p_col>1], 
                 integer(0))){
      message("actual dimension of group : ", group_value, " is NULL or 1")
      return(NULL) # continue process to write data
    }else
      p_col <- unique(p_col[p_col>1])
    message("actual dimension of group : ", group_value, " is ", p_col)
 
    # check dimension of new group
    if(operator_check%in%"both"){
      lt_dim <- dim(values_data$lt)[2]
      gt_dim <- dim(values_data$gt)[2]
      if(lt_dim!=gt_dim)
        stop("dimension of values are not similar ",
             call. = FALSE)
      p_col_new <- lt_dim
    }else
      p_col_new <- dim(values_data[[output_operator]])[2]
    
    # # no values provided
    # if(is.null(p_col_new))
    #  p_col_new <- 0
   
   if(p_col!=p_col_new) # & p_col!=0
     stop(paste0("Put right columns dimension : ", 
                 p_col, " for existing 'group' : ", 
                 group_value), call. = FALSE)
  }
}

# v870
.valueCheck870 <- function(values, timeStep){
  # check nrow Vs timeStep
    nrows <- switch(timeStep,
                    hourly = 24*366,
                    daily = 366,
                    weekly = 366,
                    monthly = 12,
                    annual = 1)
    
    list_checked <- sapply(names(values), function(x, list_in= values, check_standard_rows= nrows){
      list_work <- list_in[[x]]
      
      # one column scenario
      if(ncol(list_work)==1){
        if (NROW(list_work) == 24*365)
            list_work <- rbind(list_work, matrix(rep(0, 24*1), ncol = 1))
        if (NROW(list_work) == 365) 
          list_work <- rbind(list_work, matrix(rep(0, 1), ncol = 1))
        if (! NROW(list_work) %in% c(0, check_standard_rows)) 
          stop("Incorrect number of rows according to the timeStep")
        }else{# scenarized columns
        if(dim(list_work)[1]==24*365)
          list_work <- rbind(list_work, 
                             matrix(rep(0, 24*dim(list_work)[2]), 
                                    ncol = dim(list_work)[2]))
        if(dim(list_work)[1]==365)
          list_work <- rbind(list_work, 
                             matrix(rep(0, dim(list_work)[2]), 
                                    ncol = dim(list_work)[2]))
        if (! dim(list_work)[1] %in% c(0, check_standard_rows)) 
          stop("Incorrect number of rows according to the timeStep")
        }
      list_work
      }, simplify = FALSE)
    list_checked
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


#' `r lifecycle::badge("experimental")` 
#' @param constraints A `list` of several named `list` containing data to create binding constraints.
#'  **Warning** all arguments for creating a binding constraints must be provided, see examples.
#' @export
#' 
#' @rdname createBindingConstraint
createBindingConstraintBulk <- function(constraints,
                                        opts = antaresRead::simOptions()) {
  assertthat::assert_that(inherits(opts, "simOptions"))
  ## Ini file
  pathIni <- file.path(opts$inputPath, "bindingconstraints/bindingconstraints.ini")
  bindingConstraints <- readIniFile(pathIni, stringsAsFactors = FALSE)
  
  
  
  for (i in seq_along(constraints)) {
    values_operator <- switch(constraints[[i]]$operator,
                              less = "lt",
                              equal = "eq",
                              greater = "gt",
                              both = c("lt", "gt"))
    
    bindingConstraints <- do.call("createBindingConstraint_", c(
      constraints[[i]],
      list(
        opts = opts, 
        bindingConstraints = bindingConstraints,
        links = antaresRead::getLinks(opts = opts, namesOnly = TRUE),
        output_operator = values_operator
      )
    ))
  }
  
  writeIni(listData = bindingConstraints, pathIni = pathIni, overwrite = TRUE)
  
  suppressWarnings({
    res <- antaresRead::setSimulationPath(path = opts$studyPath, simulation = "input")
  })
  invisible(res)
}



constructor_binding_values <- function(lt = NULL, gt = NULL, eq = NULL){
  # # check args
  # args <- sapply(match.call()[-1], 
  #                FUN = is.null)
  # 
  # if(!any(args)){
  #   args <- sapply(match.call()[-1], 
  #                  FUN = is.numeric)
  #   
  #   values <- sapply(match.call()[-1], 
  #                    FUN = as.data.frame)
  # }
    
  list(lt = lt,
       gt = gt, 
       eq = eq)
  
  
}










