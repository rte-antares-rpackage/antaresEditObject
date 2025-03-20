#' @title Edit a short-term storage cluster 
#' 
#' @description 
#' `r antaresEditObject:::badge_api_ok()`
#' 
#' Edit parameters and time series of an existing `st-storage` cluster (Antares studies >= v8.6.0).
#' 
#' @inheritParams createClusterST
#' @template opts
#' 
#' @note
#' Put only properties or TS value you want to edit (see `examples` section).
#' 
#' @seealso [createClusterST()], [removeClusterST()]
#' 
#' @examples
#' \dontrun{
#' # TODO
#' print(2+2)
#' }
#' @export
editClusterST <- function(area,
                          cluster_name, 
                          group = NULL,
                          storage_parameters = NULL,
                          PMAX_injection = NULL,
                          PMAX_withdrawal = NULL,
                          inflows = NULL,
                          lower_rule_curve = NULL,
                          upper_rule_curve = NULL,
                          cost_injection = NULL,
                          cost_withdrawal = NULL,
                          cost_level = NULL,
                          cost_variation_injection = NULL,
                          cost_variation_withdrawal = NULL,
                          add_prefix = TRUE, 
                          opts = antaresRead::simOptions()) {

  ## check study opts parameters ----
  assertthat::assert_that(inherits(opts, "simOptions"))
  
  ## check study version ----
  check_active_ST(opts, check_dir = TRUE)
  
  ##  check area ----
  check_area_name(area, opts)
  
  ## tolower area----
  area <- tolower(area)
  
  ## check 'group'----
  .check_group_st(group = group, opts = opts)
  
  ##
  ## check parameters ----
  ##
  params_cluster <- NULL
  
  if(!is.null(storage_parameters)){
    assertthat::assert_that(inherits(storage_parameters, "list"))
    
    # static name of list parameters 
    names_parameters <- names(storage_values_default(opts = opts))
    
    if(!all(names(storage_parameters) %in% names_parameters))
      stop(append("Parameter 'st-storage' must be named with the following elements: ", 
                  paste0(names_parameters, collapse= ", ")))
    
    # check values parameters
    .st_mandatory_params(list_values = storage_parameters, opts = opts)
    
    ### Standardize cluster name  ----
    params_cluster <- hyphenize_names(storage_parameters)
  }
  
  ## cluster name prefix ----
  if (add_prefix)
    cluster_name <- generate_cluster_name(area, 
                                          cluster_name, 
                                          add_prefix)
  
  # all properties of cluster standardized
  params_cluster <- c(list(name = cluster_name, 
                           group = group),
                      params_cluster)
  
  
  
  # delete it (for writeIni())
  if(is.null(group))
    params_cluster$group <- NULL
  
  ## check dim data ----
  
  # According to Antares Version 
    # default values associated with TS + .txt names files
  list_local_values_params <- .default_values_st_TS(opts = opts)
  
  # check every ts parameter
  for (name in names(list_local_values_params)){
    if (!is.null(dim(get(name))))
      if (!identical(dim(get(name)), c(8760L, 1L)))
        stop(paste0("Input data for ", name, " must be 8760*1"))
  }
  
  ## API block ----
  if (is_api_study(opts)) {
    # format name for API 
    cluster_name <- transform_name_to_id(cluster_name)
    
    ##
    # PATCH for properties 
    ##
    # adapt parameter names
    list_properties <- list(
      "group" = params_cluster[["group"]],
      "name" = cluster_name,
      "injectionNominalCapacity" = params_cluster[["injectionnominalcapacity"]],
      "withdrawalNominalCapacity" = params_cluster[["withdrawalnominalcapacity"]],
      "reservoirCapacity" = params_cluster[["reservoircapacity"]],
      "efficiency" = params_cluster[["efficiency"]],
      "initialLevel" = params_cluster[["initiallevel"]],
      "initialLevelOptim" = params_cluster[["initialleveloptim"]],
      "enabled" = params_cluster[["enabled"]])
    
    list_properties <- dropNulls(list_properties)
    
    if(length(list_properties)>1){
      # make json file
      body <- jsonlite::toJSON(list_properties,
                               auto_unbox = TRUE)
      
      # send request (without coeffs/term)
      result <- api_patch(opts = opts, 
                          endpoint = file.path(opts$study_id, 
                                               "areas", 
                                               area,
                                               "storages",
                                               cluster_name), 
                          body = body, 
                          encode = "raw")
      
      cli::cli_alert_success("Endpoint {.emph {'Edit ST-storage (properties)'}} {.emph 
                      {.strong {cluster_name}}} success")
    }
    
    ##
    # PUT for TS values
    ##
    # adapt list name TS 
    list_value_ts <- list(pmax_injection = PMAX_injection,
                          pmax_withdrawal = PMAX_withdrawal,
                          inflows = inflows,
                          lower_rule_curve = lower_rule_curve,
                          upper_rule_curve = upper_rule_curve)
    
    list_value_ts <- dropNulls(list_value_ts)
    
    if(length(list_value_ts)!=0){
      lapply(names(list_value_ts), function(x){
        body = jsonlite::toJSON(list(data=list_value_ts[[x]],
                                     index=0, 
                                     columns=0),
                                auto_unbox = FALSE)
        
        endpoint <- file.path(opts$study_id, 
                              "areas", 
                              area, 
                              "storages",
                              cluster_name,
                              "series", 
                              x)
        
        # update
        api_put(opts = opts, 
                endpoint =  endpoint, 
                body = body, 
                encode = "raw")
        
        cli::cli_alert_success("Endpoint {.emph {'Edit ST-storage (TS value)'}} {.emph 
                      {.strong {x}}} success")
      })
    }
    return(invisible(opts))
  }
  
  ## write properties ----
  # path to ini file
  path_clusters_ini <- file.path(opts$inputPath, 
                                 "st-storage", 
                                 "clusters", 
                                 area, 
                                 "list.ini")
  if (!file.exists(path_clusters_ini))
    stop("'", cluster_name, "' in area '", area, "' doesn't seems to exist.")
  
  # only edition if parameters are no NULL
  is_null_parameter <- all(names(params_cluster)%in%"name")
  if(is_null_parameter)
    warning("No edition for 'list.ini' file", call. = FALSE)
  else{
    # read previous content of ini
    previous_params <- readIniFile(file = path_clusters_ini)
    
    if (!tolower(cluster_name) %in% tolower(names(previous_params)))
      stop(
        "'", 
        cluster_name, 
        "' doesn't exist, it can't be edited. You can create cluster with createCluster().",
        call. = FALSE
      )
    
    
    # select existing cluster
    ind_cluster <- which(tolower(names(previous_params)) %in% 
                           tolower(cluster_name))[1]
    previous_params[[ind_cluster]] <- utils::modifyList(x = previous_params[[ind_cluster]], 
                                                        val = params_cluster)
    names(previous_params)[[ind_cluster]] <- cluster_name
    
    # write modified ini file
    writeIni(
      listData = previous_params,
      pathIni = path_clusters_ini,
      overwrite = TRUE
    )
  }
  
  
  ## write TS ----
  
  ##
  # Write TS PART ("series/")
  ##

  # Path folder for TS
  path_txt_file <- file.path(opts$inputPath, 
                             "st-storage", 
                             "series", 
                             tolower(area), 
                             tolower(cluster_name))
  
  # list of params of TS 
  list_local_params <- list(PMAX_injection = PMAX_injection,
                         PMAX_withdrawal =  PMAX_withdrawal,
                         inflows =  inflows ,
                         lower_rule_curve =  lower_rule_curve,
                         upper_rule_curve =  upper_rule_curve,
                         cost_injection =  cost_injection,
                         cost_withdrawal =  cost_withdrawal,
                         cost_level =  cost_level,
                         cost_variation_injection =  cost_variation_injection,
                         cost_variation_withdrawal =  cost_variation_withdrawal)
  
  # write TS
  lapply(names(list_local_params), function(x_val){
    if(!is.null(list_local_params[[x_val]])){
      # name file
      name_file <- list_local_values_params[[x_val]][["string"]]
      # write disk
      fwrite(
        x = list_local_params[[x_val]], 
        row.names = FALSE, 
        col.names = FALSE, 
        sep = "\t",
        file = file.path(path_txt_file, 
                         paste0(name_file, 
                                ".txt"))
      )
    }
  })
  
  # Maj simulation
  suppressWarnings({
    res <- antaresRead::setSimulationPath(path = opts$studyPath, simulation = "input")
  })
  
  invisible(res)
}
