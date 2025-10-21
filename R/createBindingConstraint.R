utils::globalVariables(c('V2', 'dim_study', 'dim_input', 'name_group'))

#' @title Create a binding constraint
#' 
#' @description 
#' `r antaresEditObject:::badge_api_ok()`  
#' `r lifecycle::badge("experimental")` 
#' 
#' Create a new binding constraint in an Antares study.
#' 
#'
#' @param name The name for the binding constraint.
#' @param id An id, default is to use the name.
#' @param values Values used by the constraint.
#'  It contains one line per time step and three columns "less", "greater" and "equal" 
#'  (see documentation below if you're using version study >= v8.7.0)
#' @param enabled Logical, is the constraint enabled ?
#' @param timeStep Time step the constraint applies to : `hourly`, `daily` or `weekly`.
#' @param operator Type of constraint: equality, inequality on one side or both sides.
#' @param filter_year_by_year Marginal price granularity for year by year
#' @param filter_synthesis Marginal price granularity for synthesis
#' @param coefficients A named list containing the coefficients used by the constraint, 
#' the coefficients have to be alphabetically ordered see examples below for entering 
#' weight or weight with offset.
#' @param group "character" group of the constraint, default value : "default group"
#' @param overwrite If the constraint already exist, overwrite the previous value.
#' 
#' @template opts
#' 
#' @family binding constraints functions
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
#' @importFrom antaresRead getLinks readClusterDesc setSimulationPath readIniFile simOptions
#' @importFrom utils write.table
#' @importFrom assertthat assert_that
#'
#' @examples
#' \dontrun{
#' # < v8.7.0 :
#' 
#' # Create constraints with multi coeffs (only weight)
#' 
#' createBindingConstraint(
#'   name = "myconstraint", 
#'   values = matrix(data = rep(0, 8760 * 3), ncol = 3), 
#'   enabled = FALSE, 
#'   timeStep = "hourly",
#'   operator = "both",
#'   coefficients = list("area1%area2" = 1,
#'     "area1%area3" = 2)
#' )
#' 
#' # Create constraints with multi coeffs + offset
#' 
#' createBindingConstraint(
#'   name = "myconstraint", 
#'   values = matrix(data = rep(0, 8760 * 3), ncol = 3), 
#'   enabled = FALSE, 
#'   timeStep = "hourly",
#'   operator = "both",
#'   coefficients = list("area1%area2" = "1%1",
#'     "area1%area3" = "2%3")
#' )
#' 
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
                                    opts = simOptions()) {
  
  # check input parameters
  assert_that(inherits(opts, "simOptions"))
  
  timeStep <- match.arg(arg = timeStep)
  operator <- match.arg(arg = operator)
  
  # Check parameter values + standardization of values
  if(opts$antaresVersion<870)
    values <- .valueCheck(values, timeStep)
  else
    if(!is.null(values))
      values <- .valueCheck870(values, timeStep)
  
  if(!is.null(coefficients)){
    # check if areas are sorted
    names_coef <- names(coefficients)
    splitted_names <- strsplit(names_coef, "%")
    are_areas_sorted <- sapply(splitted_names, function(areas) {
      identical(areas, sort(areas))
    })
    
    if (!all(are_areas_sorted)) 
      stop("The areas are not sorted alphabetically.", call. = FALSE)
  }
  
  # API block
  if (is_api_study(opts = opts)) {
    
    # reformat coefficients offset values
    coefficients <- .check_format_offset(coefficients = coefficients)
    
    # api treatments
    api_opts <- .createBC_api(name = name,
                              enabled = enabled,
                              time_step = timeStep,
                              operator = operator,
                              filter_year_by_year = filter_year_by_year,
                              filter_synthesis = filter_synthesis,
                              values = values,
                              group = group,
                              coeffs = coefficients, 
                              opts = opts)
    
    return(invisible(api_opts))
  }
  
  ## Ini file
  pathIni <- file.path(opts$inputPath, "bindingconstraints/bindingconstraints.ini")
  bindingConstraints <- readIniFile(pathIni, stringsAsFactors = FALSE)
  
  # v870
  if(opts$antaresVersion>=870){
    if(is.null(group))
      group <- "default"
    
    values_operator <- switch_to_list_name_operator_870(operator = operator)
    
    if(!is.null(values)){
      assert_that(inherits(values, "list"))
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
      group_values_meta_check(group_value = group, 
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
    values = values,
    enabled,
    timeStep,
    operator,
    filter_year_by_year,
    filter_synthesis,
    coefficients,
    group,
    overwrite,
    links = getLinks(opts = opts, namesOnly = TRUE),
    clusters = readClusterDesc(opts = opts),
    output_operator = values_operator,
    opts = opts
  )
  
  # Write Ini
  writeIni(listData = bindingConstraints, pathIni = pathIni, overwrite = TRUE)
  
  # Maj simulation
  suppressWarnings({
    res <- setSimulationPath(path = opts$studyPath, simulation = "input")
  })
  
  invisible(res)
}


.createBC_api <- function(..., opts){
  body <- list(...)
  # <v870
  if(opts$antaresVersion<870){
    # re structure parameter coeffs
    if(is.null(body$coeffs))
      body$coeffs <- list()
    else if(length(body$coeffs[[1]]) %in% 1)
      body$coeffs <- lapply(body$coeffs, 
                            as.list)
    # re structure parameter values
    if(identical(body$values, character(0)))
      body$values <- NULL
    
    cmd <- api_command_generate(
      "create_binding_constraint", 
      name = body$name,
      enabled = body$enabled,
      time_step = body$time_step,
      operator = body$operator,
      filter_year_by_year = body$filter_year_by_year,
      filter_synthesis = body$filter_synthesis,
      values = body$values,
      coeffs = body$coeffs)
    
    api_command_register(cmd, opts = opts)
    
    `if`(
      should_command_be_executed(opts), 
      api_command_execute(cmd, opts = opts, text_alert = "create_binding_constraint: {msg_api}"),
      cli_command_registered("create_binding_constraint")
    )
    return(invisible(opts))
  }
  
  # >=v870
  
  # reforge list structure
  if(!is.null(body$values)){
    list_values <- list(less_term_matrix = body$values$lt,
                        equal_term_matrix = body$values$eq,
                        greater_term_matrix = body$values$gt)
    
    list_values <- dropNulls(list_values)
    body$values <- NULL
    
    body <- append(body, list_values)
  }
  
  # delete NULL from parameters
  body <- dropNulls(body)
  body_terms <- NULL
  
  # filter coeffs if none null
  if(!is.null(body$coeffs)){
    body_terms <- body$coeffs
    body$coeffs <- NULL
    
    body_terms <- lapply(seq(length(body_terms)), function(x){
      # extract areas/cluster (links or thermal)
      name_coeff <- names(body_terms[x])
      term_coeff <- body_terms[x]
      terms_values <- strsplit(x = name_coeff, split = "%|\\.")
      
      is_dot <- grepl(x = name_coeff, 
                      pattern = "\\.")
      
      # build list 
      if(is_dot)
        data_list <- list(area=terms_values[[1]][1],
                          cluster=terms_values[[1]][2])
      else
        data_list <- list(area1=terms_values[[1]][1],
                          area2=terms_values[[1]][2])
      
      if(length(term_coeff[[1]])>1)
        body_terms <- list(weight=term_coeff[[1]][1],
                           offset=term_coeff[[1]][2],
                           data=data_list)
      else
        body_terms <- list(weight=term_coeff[[1]][1],
                           data=data_list)
    })
    
    # make json file
    body_terms <- jsonlite::toJSON(body_terms,
                                   auto_unbox = TRUE)
  }

  # make json file
  body <- jsonlite::toJSON(body,
                           auto_unbox = TRUE)
  
  # send request (without coeffs/term)
  result <- api_post(opts = opts, 
                     endpoint = file.path(opts$study_id, "bindingconstraints"), 
                     body = body, 
                     encode = "raw")
  # /validate 
  api_get(opts = opts, 
          endpoint = file.path(opts$study_id, 
                               "constraint-groups",
                               result$group, 
                               "validate"))
  
  # specific endpoint for coeffs/term
  if(!is.null(body_terms))
    api_post(opts = opts, 
             endpoint = file.path(opts$study_id, 
                                  "bindingconstraints", 
                                  result$id, 
                                  "terms"), 
             body = body_terms, 
             encode = "raw")
  
  # output message
  bc_name <- result$id
  cli::cli_alert_success("Endpoint {.emph {'Create bindingconstraints'}} {.emph 
                      {.strong {bc_name}}} success")
  
  return(invisible(opts))
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
                                     clusters,
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
    
    has_links_coefs <- length(grep("%", names(coefficients))) > 0
    has_clusters_coefs <- length(grep("\\.", names(coefficients))) > 0
    
    if (has_links_coefs) {
      if (length(links) > 0) {
        .check_bc_validity_coefficients(coefficients = coefficients, reference = links, type = "links")
      } else {
        stop("You are trying to create a binding constraint with a link coefficient but you have no link in your study.")
      }    
    }
    
    if (has_clusters_coefs) {
      if (nrow(clusters) > 0) {
        .check_bc_validity_coefficients(coefficients = coefficients, reference = clusters, type = "thermal")
      } else {
        stop("You are trying to create a binding constraint with a cluster coefficient but you have no cluster in your study.")
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
  
  # Write values
  # v870
  if(opts$antaresVersion>=870){
    # make name file + path file + code file 
      # to write values matching operator
    name_file <- paste0(id, "_", output_operator, ".txt")
    up_path <- file.path(opts$inputPath, "bindingconstraints", name_file)
    
    df <- data.frame(
      name_file = name_file,
      code_file = output_operator,
      path_file =  up_path)
    
    # write txt file(s)
    lapply(seq(nrow(df)), function(x, df_ts= values){
      if(identical(df_ts, character(0)))
        data_content <- data.table::as.data.table(df_ts)
      else{
        target_name <- df[x, "code_file"]
        data_content <- data.table::as.data.table(df_ts[[target_name]])
      }
      fwrite(x = data_content, 
             file = df[x, "path_file"], 
             col.names = FALSE, 
             row.names = FALSE, 
             sep = "\t")
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
#' 
#' Dimension of groups are compared with meta parameter `binding` returned by [antaresRead::simOptions()]
#' @param group_value `character` name of group
#' @param values_data `list` values used by the constraint
#' @param operator_check `character` parameter "operator"
#' @param output_operator `character` for 
#' @return NULL if it's new group to add or error exceptions with dimension control
#' @template opts
#' @export 
#' @keywords internal
group_values_meta_check <- function(group_value, 
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
  
  # check dimension of new group (INPUT)
  if(operator_check%in%"both"){
    lt_dim <- dim(values_data$lt)[2]
    gt_dim <- dim(values_data$gt)[2]
    if(lt_dim!=gt_dim)
      stop("dimension of values are not similar ",
           call. = FALSE)
    p_col_new <- lt_dim
  }else
    p_col_new <- dim(values_data[[output_operator]])[2]
  
  # check meta 
    # study with no BC or virgin study
  if(is.null(opts$binding)){
    cat("\nThere were no binding constraints in this study\n")
    return()
  }
  
  # read dimension
  dim_bc_group <- opts$binding 
  
  # group already exists ? 
    # no duplicate groups in the study
  is_exists <- grepl(pattern = group_value, 
                     x = dim_bc_group[, .SD, .SDcols = 1])
  
  if(!is_exists){
    cat("\nNew/existing group : ", 
        paste0("'", group_value, "'"), 
        " will be created/updated with dimension : ",
        paste0("[", p_col_new, "]"),
        "\n")
    return()
  }
  
  # dimension of existing group
  p_col <- dim_bc_group[name_group%in%group_value][["dim"]]
  
  if(p_col!=p_col_new) # & p_col!=0
    stop(paste0("Put right columns dimension : ", 
                p_col, " for existing 'group' : ", 
                group_value), call. = FALSE)
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
  
  list_checked <- sapply(
    names(values), 
    function(x, 
             list_in= values, 
             check_standard_rows= nrows){
      
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

# update structure of coefficients/offset for api mode (char to vector)
.check_format_offset <- function(coefficients){
  if(!is.null(coefficients)){
    # check if offset
    is_character_values <- sapply(coefficients, 
                                  function(x) is.character(x))
    
    if(any(is_character_values)){
      # format offset for solver
      index <- which(is_character_values)
      
      list_format <- lapply(index, function(x){
        var <- unlist(strsplit(x = coefficients[[x]], 
                               split = "%"))
        as.numeric(var)
      })
      
      # update list with format
      coefficients <- append(list_format, 
                             coefficients[-index])
    }else
      coefficients
  }
}

#' @title Create multiple binding constraint at once.
#' @description
#' `r lifecycle::badge("experimental")` 
#' `r antaresEditObject:::badge_api_no()` 
#' @param constraints A `list` of several named `list` containing data to create binding constraints.
#'  **Warning** all arguments for creating a binding constraints must be provided, see examples.
#' @template opts
#' @family binding constraints functions
#'
#' @importFrom antaresRead getLinks setSimulationPath readIniFile
#' 
#' @details 
#' According to Antares version, usage may vary :
#' 
#' **>= v8.7.0** :  
#'  - For each constraint name, one file .txt containing `<id>_lt.txt, <id>_gt.txt, <id>_eq.txt`.  
#' 
#'  - Parameter `values` must be named `list` ("lt", "gt", "eq") containing `data.frame` scenarized.  
#' 
#'  - Add parameter `group` in input list `constraints`
#' 
#' see example section below.
#' @export
#' 
#' @examples
#' \dontrun{
#' # For Study version < v8.7.0
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
#'       coefficients = list("area1%area2" = 1),
#'       overwrite = TRUE
#'     )
#'   }
#' )
#' # create all constraints
#' createBindingConstraintBulk(bindings_constraints)
#' 
#' # For Study version >= v8.7.0 (add parameter `group`)
#' 
#' # data values (hourly)
#' df <- matrix(data = rep(0, 8760 * 3), ncol = 3)
#' values_data <- list(lt=df, 
#'                     gt= df)   
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
#'       coefficients = list("at%fr" = 1),
#'       group= "group_bulk",
#'      overwrite = TRUE
#'    )
#'   }
#' )
#'  
#' createBindingConstraintBulk(bindings_constraints)  
#' }
#' 
createBindingConstraintBulk <- function(constraints,
                                        opts = antaresRead::simOptions()) {
  
  assertthat::assert_that(inherits(opts, "simOptions"))
  assertthat::assert_that(isFALSE(is_api_study(opts = opts)), msg = "createBindingConstraintBulk() is not available in API mode.")
  
  if(opts[["antaresVersion"]] >= 870) {
    # check matrix dimension
    .check_bulk_object_dim(constraints = constraints, opts = opts)
  }
  
  pathIni <- file.path(opts$inputPath, "bindingconstraints", "bindingconstraints.ini")
  bindingConstraints <- readIniFile(pathIni, stringsAsFactors = FALSE)
  
  for (i in seq_along(constraints)) {
    values_operator <- switch_to_list_name_operator_870(operator = constraints[[i]][["operator"]])
    
    bindingConstraints <- do.call("createBindingConstraint_", c(
      constraints[[i]],
      list(
        opts = opts, 
        bindingConstraints = bindingConstraints,
        links = antaresRead::getLinks(opts = opts, namesOnly = TRUE),
        clusters = readClusterDesc(opts = opts),
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


# control group dimensions in bulk object
  # control object with study
.check_bulk_object_dim <- function(constraints,
                                   opts = antaresRead::simOptions()){
  assertthat::assert_that(inherits(constraints, "list"))
  
  # check matrix number of columns by group
  # In all_dim_group, group is column V1, number of columns is column V2
  matrix_dimension_by_constraint <- lapply(constraints, FUN = .compute_matrix_dimension_constraint)
  all_dim_group <- do.call("rbind", c(matrix_dimension_by_constraint, fill = TRUE))

  # If each matrix is NULL, there is no second dimension in the table
  if (dim(all_dim_group)[2] < 2) {
    return()
  }
  
  # Deduplicate rows and filter V2 > 1
  select_dim <- unique(all_dim_group)[V2 > 1]
  
  # Detect duplicated groups
  duplicated_groups <- select_dim[duplicated(select_dim$V1),]$V1
  
  if (!identical(duplicated_groups, character(0))) {
    stop("Problem dimension with group : ", 
         paste0(duplicated_groups, sep = " "), 
         call. = FALSE)
  }
  
  # check input object with study
  if (is.null(opts[["binding"]])) {
    return()
  }
  else{
    merge_groups <- merge.data.table(x = opts[["binding"]], 
                     y = select_dim, 
                     by.x ="name_group", 
                     by.y = "V1")
    
    names(merge_groups) <- c("name_group", "dim_study", "dim_input")
    
    # check diff 
    diff_dim <- merge_groups[dim_study!=dim_input]
    
    if (nrow(diff_dim) > 0) {
      stop("Problem dimension with group in Study: ", 
           paste0(diff_dim$name_group, sep = " "), 
           call. = FALSE)
    }
  }
}


switch_to_list_name_operator_870 <- function(operator) {
  
  assertthat::assert_that(operator %in% c("less", "greater", "equal", "both"))
  
  operator_symbol <- switch(operator,
                            "less" = "lt",
                            "equal" = "eq",
                            "greater" = "gt",
                            "both" = c("lt", "gt")
                            )
  
  return(operator_symbol)
}

# Compute the dimension of a matrix (if operatior is not "both") or 2 (if operatior is "both") in a constraint
.compute_matrix_dimension_constraint <- function(constraint){
  
  assertthat::assert_that(inherits(constraint, "list"))
  assertthat::assert_that(all(c("group", "operator", "values") %in% names(constraint)))
  
  res <- data.table()
  
  operator_symbol <- switch_to_list_name_operator_870(operator = constraint[["operator"]])
  dim_matrix <- lapply(constraint[["values"]][which(names(constraint[["values"]]) %in% operator_symbol)], dim)
  dim_matrix <- dim_matrix[!sapply(dim_matrix, is.null)]
  nb_matrix <- length(dim_matrix)
  if (nb_matrix > 0) {
    res <- data.table(rep(constraint[["group"]], nb_matrix), sapply(dim_matrix, "[[", 2))
  }
  
  return(res)
}


.check_bc_validity_coefficients <- function(coefficients, reference, type) {
  
  if (type == "links") {
    pattern_coefficientsToControl <- "%"
    reference <- as.character(reference)
    reference <- gsub(pattern = " - ", replacement = "%", x = reference)
    
    #for obscure reasons R CMD check inverse alphabetic order for coefficients
    #test for createPSP() are OK for devools::test() but not for devtools::check()
    #these lines are here to correct this behaviour
    #see https://github.com/r-lib/testthat/issues/144
    #and https://github.com/r-lib/testthat/issues/86
    #set Sys.setenv("R_TESTS" = "") do nothing
    resLinks <- strsplit(reference, "%")
    for(i in seq_along(resLinks)){
      resLinks[[i]] <- paste(resLinks[[i]][2], resLinks[[i]][1], sep = "%")
    }
    reference <- c(reference, as.character(resLinks))
    pattern_stop_msg <- "link(s)"
  } else if (type == "thermal") {
    pattern_coefficientsToControl <- "\\."
    reference <- reference[,c("area", "cluster")]
    reference <- paste0(reference$area, ".", reference$cluster)
    pattern_stop_msg <- "cluster(s)"
  }
  
  coefficientsToControl <- coefficients[grep(pattern_coefficientsToControl, names(coefficients))]
  
  if(length(coefficientsToControl) > 0) {
    if (!all(names(coefficientsToControl) %in% reference)) {
      badcoef <- names(coefficientsToControl)[!names(coefficientsToControl) %in% reference]
      badcoef <- paste(shQuote(badcoef), collapse = ", ")
      stop(paste0(badcoef, " : is or are not valid ", pattern_stop_msg))
    }
  }
}
