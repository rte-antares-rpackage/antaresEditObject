
#' @title Set the thematic trimming of an Antares Study
#'
#' @description 
#' `r antaresEditObject:::badge_api_ok()`
#' 
#' 
#' Put only variables names you want to keep in study output.  
#' You can add or remove variables (use study version >=v8.8).
#'
#' @param selection_variables `character` of variables to add or remove.
#' @param type_select `character` select mode to add or remove (default add mode).
#' @template opts
#' 
#' @note You can put only variables according to version of study
#' 
#' @export
#' 
#' @examples 
#' \dontrun{
#' 
#' # list of variables (version >= v8.8)
#' vect_select_vars <- list_thematic_variables()
#' 
#' ##
#' # add all variables
#' ##
#' setThematicTrimming(selection_variables = vect_select_vars$variable)
#' 
#' ##
#' # remove all variables
#' ##
#' setThematicTrimming(selection_variables = vect_select_vars$variable, 
#'                     type_select = "suppr")
#' 
#' }
setThematicTrimming <- function(selection_variables, 
                                type_select = c("add", "suppr"),
                                opts = simOptions()){

  # available only for study version >= 880
  if(opts$antaresVersion<880)
    stop("'setThematicTrimming()' calls is only available for study version >= v8.8", 
         call. = FALSE)
  
  # basics parameters checks
  assertthat::assert_that(inherits(opts, "simOptions"))
  assertthat::assert_that(inherits(selection_variables, "character"))
  type_select <- match.arg(type_select)
  
  # checks whether the variables agree with the version
    # at least one variable must be present in the list 
  target_version <- as.character(opts$antaresVersion)
  variables_package_list <- antaresRead:::pkgEnv$thematic[[target_version]]$col_name
  
  is_selection_ok <- any(
    selection_variables %in%
    variables_package_list
  )
  
  if(!is_selection_ok)
    stop(paste0("Put only variables according too version of study : "), 
         opts$antaresVersion, 
         call. = FALSE)
  
  # update [general] (used to custom variable selection in Antares UI)
  opts$parameters$general$`thematic-trimming` <- TRUE
  
  # manage writing general data
  opts_generaldata_updated <- .set_thematic(type_select = type_select, 
                pkg_var_version = variables_package_list, 
                list_var_selection = selection_variables, 
                opts_param =  opts, 
                api_mode = is_api_study(opts = opts))

  # write updated file
  if (is_api_study(opts)) {
    bloc_selection <- opts_generaldata_updated$parameters[
      names(opts_generaldata_updated$parameters)%in%
        c("general", "variables selection")]
  }else
    bloc_selection <- opts_generaldata_updated$parameters
  
  writeIni(listData = bloc_selection, 
           pathIni = "settings/generaldata", 
           overwrite = TRUE, 
           opts = opts)
  
  # Update simulation options object
  invisible(opts_generaldata_updated)
}


# according to UI or API 
  # write in generaldata file only the fewest variables 
  # Ex : you want ADD 90% of variables => write to SUPPR 10% 
.set_thematic <- function(type_select, 
                          pkg_var_version, 
                          list_var_selection,
                          opts_param,
                          api_mode = FALSE){

  # reset [variables selection]
  opts_param$parameters$`variables selection` <- NULL
  
  # count variables to write
  nb_var_version <- length(pkg_var_version)
  index_var_selection <- pkg_var_version %in% 
    list_var_selection
  nb_right_var_selection <- sum(index_var_selection)
  
  # case with all variables selected
  if(nb_var_version==nb_right_var_selection){
    if(type_select %in% "add"){
      message("All variables are selected, by default it's not required to edit 'generaldata.ini'")
      # general_data$`variables selection` <- NULL
      return(opts_param)
      }
    else{
      message("All variables will be skiped")
      opts_param$parameters$`variables selection`$selected_vars_reset <- FALSE
      return(opts_param)
      }
    }
  
  # write your selection
  if(nb_right_var_selection<as.integer(nb_var_version/2)){
    if(type_select %in% "add"){
      var_select_bloc <- .make_thematic_list(var_selection = list_var_selection, 
                                             api_mode = api_mode)
      opts_param$parameters$`variables selection` <- var_select_bloc
      return(opts_param)
    }else{
      var_select_bloc <- .make_thematic_list(var_selection = list_var_selection, 
                                             pattern_list = "select_var -", 
                                             type_select = "suppr",
                                             api_mode = api_mode)
      opts_param$parameters$`variables selection` <- var_select_bloc
      return(opts_param)
    }
    # write the opposite 
  }else{ 
    # diff 
    list_var_selection <- setdiff(pkg_var_version, list_var_selection)
    if(type_select %in% "add"){
      var_select_bloc <- .make_thematic_list(var_selection = list_var_selection, 
                                             pattern_list = "select_var -", 
                                             type_select = "suppr",
                                             api_mode = api_mode)
      opts_param$parameters$`variables selection` <- var_select_bloc
      return(opts_param)
    }else{
      var_select_bloc <- .make_thematic_list(var_selection = list_var_selection, 
                                             pattern_list = "select_var +", 
                                             type_select = "add",
                                             api_mode = api_mode)
      opts_param$parameters$`variables selection` <- var_select_bloc
      return(opts_param)
    }
  }
}



# list construction (section [variables selection]
  # build list with pattern and type 
.make_thematic_list <- function(pattern_list = "select_var +", 
                                type_select = "add",
                                var_selection, 
                                api_mode = FALSE){
  # "add"
  if(type_select %in% "add")
    # init list
    ll_init <- list(
      selected_vars_reset = FALSE
    )
  else
    # init list
    ll_init <- list(
      selected_vars_reset = TRUE
    )
  
  # update list
  if(api_mode){
    element_list <- jsonlite::toJSON(var_selection)
    bloc_list <- list(element_list)
    names(bloc_list) <- pattern_list
    }else{
    bloc_list <- lapply(var_selection, function(x){
      x
    })
    names(bloc_list) <- rep(pattern_list, 
                            length(var_selection))
  }
    
  # add to init list
  bloc_list <- append(ll_init, 
                      bloc_list)
  
  return(bloc_list)
}
