#' @title Update an existing binding constraint
#' 
#' @description 
#' `r antaresEditObject:::badge_api_ok()`
#' `r lifecycle::badge("experimental")`
#' 
#' Update an existing binding constraint in an Antares study.  
#' The key search value of the constraint is the `id` field
#' 
#' @inheritParams createBindingConstraint
#' @param group "character" group of the constraint, default value : "default"
#' @param values Values used by the constraint.
#'  It contains one line per time step and three columns "less", "greater" and "equal"
#'  (see documentation below if you're using version study >= v8.7.0)
#' @template opts
#' 
#' @family binding constraints functions
#' 
#' @section Warning: 
#' Put values with rights dimensions :  
#'  - hourly : 8784  
#'  - daily = 366  
#'  
#' 
#' **>= v8.7.0** : For each constraint name, one file .txt containing `<id>_lt.txt, <id>_gt.txt, <id>_eq.txt`  
#' Parameter `values` must be named `list` ("lt", "gt", "eq") containing `data.frame` scenarized.  
#' see example section below.
#' 
#' @export
#' 
#' @importFrom antaresRead getLinks setSimulationPath readClusterDesc simOptions readIniFile
#' @importFrom utils write.table
#' @importFrom assertthat assert_that
#' @importFrom data.table as.data.table fwrite
#'
#' @examples
#' \dontrun{
#'  # < v8.7.0 :
#' editBindingConstraint(
#'   name = "myconstraint", 
#'   values = matrix(data = rep(0, 8784 * 3), ncol = 3), 
#'   enabled = FALSE, 
#'   timeStep = "hourly",
#'   operator = "both",
#'   coefficients = list("fr%de" = 1)
#' )
#' 
#' # update binding constraint with weight + offset 
#' editBindingConstraint(
#'   name = "myconstraint", 
#'   values = matrix(data = rep(0, 8784 * 3), ncol = 3), 
#'   enabled = FALSE, 
#'   timeStep = "hourly",
#'   operator = "both",
#'   coefficients = list("fr%de" = "1%-5")
#' )
#' 
#'  # >= v8.7.0 :
#'  
#' # data values scenarized (hourly)
#' df <- matrix(data = rep(0, 8784 * 3), ncol = 3)
#'  
#' # you can provide list data with all value 
#' # or just according with 'operator' (ex : 'lt' for 'less)
#' values_data <- list(lt=df, 
#'                    gt= df, 
#'                    eq= df)  
#'                      
#' editBindingConstraint(name = "myconstraint", 
#'                       values = values_data, 
#'                       enabled = TRUE, 
#'                       timeStep = "hourly", 
#'                       operator = "both", 
#'                       filter_year_by_year = "hourly", 
#'                       filter_synthesis = "hourly", 
#'                       coefficients = list("fr%de" = 1), 
#'                       group = "myconstraint_group")                   
#' }
editBindingConstraint <- function(name,
                                  id = tolower(name),
                                  values = NULL,
                                  enabled = NULL,
                                  timeStep = NULL,
                                  operator = NULL,
                                  filter_year_by_year = NULL,
                                  filter_synthesis = NULL,
                                  coefficients = NULL,
                                  group = NULL,
                                  opts = simOptions()) {
  assert_that(inherits(opts, "simOptions"))
  
  ## API block ----
  if (is_api_study(opts = opts)) {
    # reformat coefficients offset values
    coefficients <- .check_format_offset(coefficients = coefficients)
    
    # api treatments
    opts_api <- .editBC_api(id = name,
                enabled = enabled,
                time_step = timeStep,
                operator = operator,
                filter_year_by_year = filter_year_by_year,
                filter_synthesis = filter_synthesis,
                values = values,
                coeffs = coefficients,
                group = group,
                opts = opts)
    
    return(invisible(opts_api))
  }
  
  # valuesIn <- values
  # check Ini file names constraints
  pathIni <- file.path(opts$inputPath, 
                       "bindingconstraints/bindingconstraints.ini")
  
  # initial parameter list
  bindingConstraints <- readIniFile(pathIni, stringsAsFactors = FALSE)
  
  previds <- lapply(bindingConstraints, 
                    `[[`, 
                    "id")
  previds <- unlist(previds, use.names = FALSE)
  if(!id %in% previds)
    stop("Binding constraint with id '", 
         id, 
         "' doesn't exist in current study.")
  
  
  # Update general params
  bc_update_pos <- which(previds %in% id)
  bc_update <- bindingConstraints[[bc_update_pos]]
  
  # Initial parameters of constraint to edit
  iniParams <- list(
    name = bc_update$name,
    id = bc_update$id,
    enabled = bc_update$enabled,
    type = bc_update$type,
    operator = bc_update$operator
  )
  
  # update parameters
    # name can be different of id
  if(!is.null(name)) 
    iniParams$name <- name
  if(!is.null(enabled)) 
    iniParams$enabled <- enabled
  if(!is.null(timeStep)) 
    iniParams$type <- timeStep
  if(!is.null(operator)) 
    iniParams$operator <- operator
  
  # Marginal price granularity (v8.3.2)
  if (opts$antaresVersion >= 832){
    iniParams <- append(iniParams, 
                        list(`filter-year-by-year` = bc_update$`filter-year-by-year`,
                             `filter-synthesis` = bc_update$`filter-synthesis`))
    if(!is.null(filter_year_by_year))
      iniParams$`filter-year-by-year` <- filter_year_by_year
    if(!is.null(filter_synthesis))
      iniParams$`filter-synthesis` <- filter_synthesis
  }
  
  # v870
  if(opts$antaresVersion>=870){
    if(!is.null(group))
      iniParams$group <- group
    else
      group <- "default"
    
    # check group values (depend of "operator")
    if(!is.null(values)){
      if(!is.null(operator)) {
        values_operator <- switch_to_list_name_operator_870(operator = operator)
      } else {
        stop("To modify the 'values' you must enter the 'operator' parameter (e.g operator = \"both\")")
      }
      group_values_meta_check(group_value = group, 
                              values_data = values,
                              operator_check = operator,
                              output_operator = values_operator,
                              opts = opts)
    }
    
  }
    
  # update constraint parameters with new parameters
  bindingConstraints[[bc_update_pos]]$name <- iniParams$name
  bindingConstraints[[bc_update_pos]]$id <- iniParams$id
  bindingConstraints[[bc_update_pos]]$enabled <- iniParams$enabled
  bindingConstraints[[bc_update_pos]]$type <- iniParams$type
  bindingConstraints[[bc_update_pos]]$operator <- iniParams$operator
  bindingConstraints[[bc_update_pos]]$`filter-year-by-year` <- iniParams$`filter-year-by-year`
  bindingConstraints[[bc_update_pos]]$`filter-synthesis` <- iniParams$`filter-synthesis`
  
  if(!is.null(coefficients)){
    
    has_links_coefs <- length(grep("%", names(coefficients))) > 0
    has_clusters_coefs <- length(grep("\\.", names(coefficients))) > 0
    
    if (has_links_coefs) {
      links <- getLinks(opts = opts, namesOnly = TRUE)
      .check_bc_validity_coefficients(coefficients = coefficients, reference = links, type = "links")
    }
    
    if (has_clusters_coefs) {
      clusters <- readClusterDesc(opts = opts)
      .check_bc_validity_coefficients(coefficients = coefficients, reference = clusters, type = "thermal")
    }
    
    bc_properties <- c("name", "id", "enabled", "type", "operator", "filter-year-by-year", "filter-synthesis", "group", "comments")
    names_coefs <- setdiff(names(bindingConstraints[[bc_update_pos]]), bc_properties)
    removed_coefs <- setdiff(names_coefs, names(coefficients))
    
    if (length(removed_coefs) > 0) {
      bindingConstraints[[bc_update_pos]][removed_coefs] <- NULL
			warning("These coefficients will be removed from the binding constraint : \n", paste0(removed_coefs, collapse = "\n"))
    }
    if (!is.list(coefficients)) {
      coefficients <- as.list(coefficients)
    }
    bindingConstraints[[bc_update_pos]] <- modifyList(x = bindingConstraints[[bc_update_pos]], val = coefficients, keep.null = FALSE)
  }
  
  # write txt files
    # v870
  if(opts$antaresVersion>=870 & !is.null(values))
    values <- .valueCheck870(values, 
                             bindingConstraints[[bc_update_pos]]$type)
  else
    values <- .valueCheck(values, 
                          bindingConstraints[[bc_update_pos]]$type)
  
  # Write Ini
  writeIni(listData = bindingConstraints, 
           pathIni = pathIni, 
           overwrite = TRUE)
  
  # Write values
  # v870
  if(opts$antaresVersion>=870){
    if(!identical(values, character(0))){
      name_file <- paste0(id, "_", 
                          values_operator, 
                          ".txt")
      
      up_path <- file.path(opts$inputPath, 
                           "bindingconstraints", 
                           name_file)
      
      df <- data.frame(
        name_file = name_file,
        code_file = values_operator,
        path_file =  up_path)
      
      lapply(seq(nrow(df)), 
             function(x, 
                      df_ts= values){
               target_name <- df[x, "code_file"]
               fwrite(x = as.data.table(df_ts[[target_name]]), 
                      file = df[x, "path_file"], 
                      col.names = FALSE, 
                      row.names = FALSE, 
                      sep = "\t")
      })
    }
    
  }else{
    pathValues <- file.path(opts$inputPath, 
                            "bindingconstraints", 
                            paste0(id, ".txt"))
    
    # read to check timestep
    suppressWarnings(
      file_r <- fread(pathValues)
    )

    if(!identical(values, character(0)))
      write.table(x = values, 
                  file = pathValues, 
                  col.names = FALSE, 
                  row.names = FALSE, sep = "\t")
  }
  
  # Maj simulation
  suppressWarnings({
    res <- antaresRead::setSimulationPath(path = opts$studyPath, 
                                          simulation = "input")
  })
  
}

# api part code
.editBC_api <- function(..., opts){
  
  body <- list(...)
  # checks for any study version (legacy)
  if (is.null(body$time_step))
    stop("You must provide `timeStep` argument with API.", 
         call. = FALSE)
  if (is.null(body$operator))
    stop("You must provide `operator` argument with API.", 
         call. = FALSE)
  
  # <v870
  if(opts$antaresVersion<870){
    # re structure parameter coeffs
    if(is.null(body$coeffs))
      body$coeffs <- list()
    else if(length(body$coeffs[[1]]) %in% 1)
      body$coeffs <- lapply(body$coeffs, 
                            as.list)
    
    cmd <- api_command_generate(
      "update_binding_constraint", 
      id = body$id,
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
      api_command_execute(cmd, opts = opts, 
                          text_alert = "update_binding_constraint: {msg_api}"),
      cli_command_registered("update_binding_constraint")
    )
    
    return(invisible(opts))
  }
  
  # >=v870
  with_time_series <- !is.null(body$values)  
  # reforge list structure
  if (with_time_series) {
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
  
  # keep id/name of constraint
  names_to_keep <- setdiff(names(body), "id")
  id_bc <- body$id
  
  # drop id
  body$id <- NULL
  
  # make json file
  body <- jsonlite::toJSON(body, 
                           auto_unbox = TRUE)
  
  # send request
  result <- api_put(opts = opts, 
          endpoint =  file.path(opts$study_id, 
                                "bindingconstraints", 
                                id_bc), 
          body = body, 
          encode = "raw")
  
  # /validate only if user provides a time series for optimization reason
  if (with_time_series) {
    api_get(opts = opts, 
            endpoint = file.path(opts$study_id, 
                                 "constraint-groups",
                                 result$group, 
                                 "validate")
            )
  }
  
  # specific endpoint for coeffs/terms
  if(!is.null(body_terms))
    api_put(opts = opts, 
             endpoint = file.path(opts$study_id, 
                                  "bindingconstraints", 
                                  result$id, 
                                  "terms"), 
             body = body_terms, 
             encode = "raw")
  
  cli::cli_alert_success("Endpoint {.emph {'Update bindingconstraints'}} {.emph 
                      {.strong {id_bc}}} success")
  
  return(invisible(opts))
}