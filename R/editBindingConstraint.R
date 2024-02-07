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
#' @template opts
#' 
#' @seealso [createBindingConstraint()] to create new binding constraints, [removeBindingConstraint()] to remove binding constraints.
#' 
#' @export
#' 
#' @importFrom antaresRead getLinks setSimulationPath
#' @importFrom utils write.table
#'
#' @examples
#' \dontrun{
#' editBindingConstraint(
#'   name = "myconstraint", 
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
                                  filter_year_by_year = NULL,
                                  filter_synthesis = NULL,
                                  coefficients = NULL,
                                  group = NULL,
                                  opts = antaresRead::simOptions()) {
  assertthat::assert_that(inherits(opts, "simOptions"))
  
  # API block
  if (is_api_study(opts)) {
    
    if (is.null(timeStep))
      stop("You must provide `timeStep` argument with API.", call. = FALSE)
    if (is.null(operator))
      stop("You must provide `operator` argument with API.", call. = FALSE)
    
    cmd <- api_command_generate(
      "update_binding_constraint", 
      id = name,
      enabled = enabled,
      time_step = timeStep,
      operator = operator,
      values = values,
      coeffs = lapply(as.list(coefficients), as.list)
    )
    
    api_command_register(cmd, opts = opts)
    `if`(
      should_command_be_executed(opts), 
      api_command_execute(cmd, opts = opts, text_alert = "update_binding_constraint: {msg_api}"),
      cli_command_registered("update_binding_constraint")
    )
    
    return(invisible(opts))
  }
  
  # valuesIn <- values
  # check Ini file names constraints
  pathIni <- file.path(opts$inputPath, "bindingconstraints/bindingconstraints.ini")
  
  # initial parameter list
  bindingConstraints <- readIniFile(pathIni, stringsAsFactors = FALSE)
  
  previds <- lapply(bindingConstraints, `[[`, "id")
  previds <- unlist(previds, use.names = FALSE)
  if(!id %in% previds){
    stop("Binding constraint with id '", id, "' doesn't exist in current study.")
  }
  
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
  
  # if(!is.null(name)) iniParams$name <- name
  # if(!is.null(id)) iniParams$id <- id
  
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
    if(!is.null(filter_year_by_year))
      iniParams$`filter-year-by-year` <- filter_year_by_year
    if(!is.null(filter_synthesis))
      iniParams$`filter-synthesis` <- filter_synthesis
  }
  
  # v870
  if(opts$antaresVersion>=870){
    if(!is.null(group)){
      iniParams$group <- group
      
      # edit group with check group values
      if(is.null(values))
        group_values_check(group_value = group, 
                           values_data = values,
                           opts = opts)
    }else
      group <- "default group"
    
    # check group values
    if(!is.null(values))
      group_values_check(group_value = group, 
                       values_data = values,
                       opts = opts)
    
  }
    
  # update constraint parameters with new parameters
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
  
  # write txt files
    # v870
  if(opts$antaresVersion>=870 & !is.null(values))
    values <- .valueCheck870(values, bindingConstraints[[bc_update_pos]]$type)
  else
    values <- .valueCheck(values, bindingConstraints[[bc_update_pos]]$type)
  
  # Write Ini
  writeIni(listData = bindingConstraints, pathIni = pathIni, overwrite = TRUE)
  
  # Write values
  # v870
  if(opts$antaresVersion>=870){
    if(!identical(values, character(0))){
      names_order_ts <- c("lt", "gt", "eq")
      name_file <- paste0(id, "_", names_order_ts, ".txt")
      
      up_path <- file.path(opts$inputPath, "bindingconstraints", name_file)
      
      lapply(up_path, function(x, df_ts= values, vect_path= up_path){
        index <- grep(x = up_path, pattern = x)
        fwrite(x = data.table::as.data.table(df_ts[[index]]), 
               file = x, 
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
    
    # # check nrow Vs timeStep
    # nrows <- switch(timeStep,
    #                 hourly = 24*366,
    #                 daily = 366,
    #                 weekly = 366,
    #                 monthly = 12,
    #                 annual = 1)
    # 
    # # check existing values 
    # if(!is.null(timeStep) & nrow(file_r)>0)
    #   if(!nrow(file_r) %in% nrows)
    #     stop("Incorrect number of rows according to the timeStep")

    if(!identical(values, character(0)))
      write.table(x = values, 
                  file = pathValues, 
                  col.names = FALSE, 
                  row.names = FALSE, sep = "\t")
  }
  
  # Maj simulation
  suppressWarnings({
    res <- antaresRead::setSimulationPath(path = opts$studyPath, simulation = "input")
  })
  
}
