#' @title Remove a Binding Constraint
#' 
#' @description 
#' `r antaresEditObject:::badge_api_ok()`
#' `r lifecycle::badge("experimental")`
#' 
#' Remove a binding constraint in an Antares study.
#' 
#' 
#' @param name Name(s) of the binding constraint(s) to remove.
#' @param group `character` Name(s) of group to delete 
#' 
#' @template opts
#' 
#' @note 
#' Starting with version **v8.7.0**, you can delete binding constraints by 
#' name or by group.
#' 
#' @family binding constraints functions
#' 
#' @export
#'
#' @examples
#' \dontrun{
#'# < v8.7.0 :
#' removeBindingConstraint(name = "mybindingconstraint")
#' 
#' # >= v8.7.0 (delete by names group) :
#' # read
#' bc <- readBindingConstraints()
#'
#' # select all groups
#' group_to_delete <- sapply(bc, function(x){
#'   x$properties$group
#' })
#'
#' # delete all groups
#' removeBindingConstraint(group = group_to_delete)
#' }
removeBindingConstraint <- function(name = NULL, 
                                    group = NULL, 
                                    opts = antaresRead::simOptions()) {
  
  assertthat::assert_that(inherits(opts, "simOptions"))
  
  # some checks for "group" parameter according to study version
  if(!opts$antaresVersion >= 870 & !is.null(group))
    stop("Parameter 'group' is only for Antares study version >= v8.7.0", 
         call. = FALSE)
  if(opts$antaresVersion >= 870){
    if(!is.null(name) & !is.null(group))
      stop("You can only delete binding constraint by id/name or by group", 
           call. = FALSE)
  }
  
  # API block
  if (is_api_study(opts)) {
    opts_api <- .remove_bc_api(name = name, 
                               group = group,
                               opts = opts)
    
    return(invisible(opts_api))
  }
  
  ## read Ini file
  pathIni <- file.path(opts$inputPath, "bindingconstraints/bindingconstraints.ini")
  bindingConstraints <- readIniFile(pathIni, stringsAsFactors = FALSE)
  
  namesbc <- unlist(lapply(bindingConstraints, `[[`, "id"), use.names = FALSE)
  
  # suppression txt files + remove constraint from ini file
  if(!is.null(name))
    updated_bc <- .delete_by_name(bc_properties = bindingConstraints, 
                                names_to_delete = name, 
                                all_bc_names = namesbc, 
                                opts = opts)
  
  # suppression txt files + remove constraint from ini file [by group]
  if(!is.null(group))
    updated_bc <- .delete_by_group(group = group, 
                                   bc_properties = bindingConstraints, 
                                   all_bc_names = namesbc, 
                                   opts = opts)
  
  # Write Ini
  writeIni(listData = updated_bc, pathIni = pathIni, overwrite = TRUE)
  
  # Maj simulation
  suppressWarnings({
    res <- antaresRead::setSimulationPath(path = opts$studyPath, simulation = "input")
  })
  
  invisible(res)
}

.delete_by_name <- function(bc_properties, 
                            names_to_delete, 
                            all_bc_names, 
                            opts){
  # delete all bc with names/id matching in study
    # delete values
    # delete .ini section
  for (i in names_to_delete) {
    if (! i %in% all_bc_names) {
      warning(paste0("No binding constraint with name '", i, "'"))
    } else {
      index <- which(all_bc_names == i)
      id <- bc_properties[[index]]$id
      bc_properties[[index]] <- NULL
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
      
      all_bc_names <- unlist(lapply(bc_properties, `[[`, "id"), use.names = FALSE)
      names(bc_properties) <- as.character(seq_along(bc_properties) - 1)
      }
  }
  return(bc_properties)
}

# feature v870 delete bc by group
.delete_by_group <- function(group, 
                             bc_properties, 
                             all_bc_names, 
                             opts){
  # extract groups
  bc_groups <- unlist(
      lapply(bc_properties, 
             `[[`, 
             "group"), 
      use.names = FALSE)
    
    if(!all(group%in%bc_groups))
      warning(paste0("No binding constraint with group '", 
                     group[!group%in%bc_groups], "'"), 
              call. = FALSE)
    else{
      index <- which(bc_groups%in%group)
      names_to_delete <- sapply(index, 
                                function(x, 
                                         bc = bc_properties){
                                  bc[[x]]$id
                                  })
      
      updated_bc <- .delete_by_name(bc_properties = bc_properties, 
                                    names_to_delete = names_to_delete, 
                                    all_bc_names = all_bc_names, 
                                    opts = opts)
      updated_bc
    }
}

.remove_bc_api <- function(..., opts){
  args <- list(...)
  # <v870
  if(opts$antaresVersion<870){
    # remove serial bc
    for (i in args$name) {
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
  
  # >=v870
  # delete by group(s) name(s)
  if(!is.null(args$group)){
    group <- args$group
    all_bc <- readBindingConstraints(opts = opts)
    
    # extract groups
    bc_groups <- sapply(all_bc, function(x){
      x$properties$group
    })
    
    # check
    if(!all(group%in%bc_groups))
      stop(paste0("No binding constraint with group '", 
                     group[!group%in%bc_groups], "'"), 
              call. = FALSE)
    
    # select name to delete
    index <- which(bc_groups%in%group)
    names_to_delete <- sapply(index, 
                              function(x, 
                                       bc = all_bc){
                                bc[[x]]$properties$id
                              })
    
    # delete names
    lapply(names_to_delete, function(x){
      # send request
      api_delete(opts = opts, 
                 endpoint =  file.path(opts$study_id, "bindingconstraints", x), 
                 encode = "raw")
      cli::cli_alert_info("Endpoint {.emph {'Delete bindingconstraints'}} {.emph 
                          {.strong {x}}} success")
    })
    
  }else
    lapply(args$name, function(x){
      # send request
      api_delete(opts = opts, 
                 endpoint =  file.path(opts$study_id, "bindingconstraints", x), 
                 encode = "raw")
      cli::cli_alert_info("Endpoint {.emph {'Delete bindingconstraints'}} {.emph 
                          {.strong {x}}} success")
  })
  return(invisible(opts))
}
