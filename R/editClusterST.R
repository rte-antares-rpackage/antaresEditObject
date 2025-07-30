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
#' # study version >= "8.6.0"
#'
#' # edit an existing cluster (see doc approved groups)
#' name_group <- "Pondage"
#'
#' editClusterST(area = "areaname", 
#'               cluster_name = "clustername", 
#'               group = name_group)
#' 
#' # edit properties
#' all_params <- storage_values_default()
#' all_params[["efficiency"]] <- 0.9
#' all_params[["reservoircapacity"]] <- 1000
#' all_params[["initiallevel"]] <- 0.5
#' all_params[["withdrawalnominalcapacity"]] <- 250
#' all_params[["injectionnominalcapacity"]] <- 200
#' all_params[["initialleveloptim"]] <- TRUE
#'
#' editClusterST(area = "areaname", 
#'               cluster_name = "clustername", 
#'               storage_parameters = all_params)
#'
#' # edit time series
#' inflow_data <- matrix(3, 8760)
#' ratio_data <- matrix(0.7, 8760)
#'
#' editClusterST(area = "areaname", 
#'               cluster_name = "clustername",
#'               PMAX_withdrawal = ratio_data, 
#'               inflows = inflow_data, 
#'               PMAX_injection = ratio_data, 
#'               lower_rule_curve = ratio_data, 
#'               upper_rule_curve = ratio_data)
#'
#' # study version >= "9.2" (new parameters and TS)
#' 
#' # edit group (dynamic)
#' name_group <- "MyOwnGroup"
#'
#' editClusterST(area = "areaname", 
#'               cluster_name = "clustername", 
#'               group = name_group)
#'
#' # edit properties
#' my_parameters <- storage_values_default()
#' my_parameters$efficiencywithdrawal <- 0.5
#' my_parameters$`penalize-variation-injection` <- TRUE
#' my_parameters$`penalize-variation-withdrawal` <- TRUE
#'
#' editClusterST(area = "areaname", 
#'               cluster_name = "clustername", 
#'               storage_parameters = my_parameters)
#'
#' # edit time series
#' ratio_data <- matrix(0.7, 8760)
#'
#' editClusterST(area = "areaname", 
#'               cluster_name = "clustername",
#'               cost_injection = ratio_data, 
#'               cost_withdrawal = ratio_data, 
#'               cost_level = ratio_data, 
#'               cost_variation_injection = ratio_data, 
#'               cost_variation_withdrawal = ratio_data)
#'               
#' # Edit optional constraints properties 
#' # make a list with names for the section and mandatory parameters
#' 
#' constraints_properties <- list(
#'   "withdrawal-1"= list(
#'     variable = "withdrawal",
#'     operator = "equal",
#'     hours = c("[1,3,5]", 
#'               "[120,121,122,123,124,125,126,127,128]")
#'   ),
#'   "netting-1"= list(
#'     variable = "netting",
#'     operator = "less",
#'     hours = c("[1, 168]")
#'   ))     
#'   
#' # make a list for TS with same names like previous properties
#' TS_values <- matrix(0.2, 8760)
#' 
#' constraints_ts <- list(
#'   "withdrawal-1"=TS_values,
#'   "netting-1"=TS_values)0
#' 
#' editClusterST(area = "areaname", 
#'               cluster_name = "clustername", 
#'               constraints_properties = constraints_properties,
#'               constraints_ts = constraints_ts)             
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
                          constraints_properties = NULL, 
                          constraints_ts = NULL,
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
    
    ### Standardize storage_parameters  ----
    params_cluster <- hyphenize_names(storage_parameters)
  }
  
  ## Standardize cluster name + prefix ----
  cluster_name_ori <- cluster_name
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
  
  if(!is_null_parameter){
    # read previous content of ini
    previous_params <- readIniFile(file = path_clusters_ini)
    
    if (!cluster_name %in% tolower(names(previous_params)))
      stop(
        "'", 
        cluster_name, 
        "' doesn't exist, it can't be edited. You can create cluster with createCluster().",
        call. = FALSE
      )
    
    
    # select existing cluster
    ind_cluster <- which(tolower(names(previous_params)) %in% 
                           cluster_name)[1]
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
                             area, 
                             cluster_name)
  
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
      dt_to_write <- as.data.table(list_local_params[[x_val]])
      fwrite(
        x = dt_to_write, 
        row.names = FALSE, 
        col.names = FALSE, 
        sep = "\t",
        file = file.path(path_txt_file, 
                         paste0(name_file, 
                                ".txt"))
      )
    }
  })
  
  ## Optional constraints ----
  .edit_storage_constraints(area = area, 
                            cluster_name = cluster_name_ori,
                            constraints_properties = constraints_properties, 
                            constraints_ts = constraints_ts, 
                            opts = opts)
  
  # Maj simulation
  suppressWarnings({
    res <- antaresRead::setSimulationPath(path = opts$studyPath, simulation = "input")
  })
  
  invisible(res)
}


#' Edit constraints to a st-storage
#' 
#' @inheritParams createClusterST
#' @noRd
.edit_storage_constraints <- function(area, 
                                    cluster_name, 
                                    constraints_properties, 
                                    constraints_ts, 
                                    opts){
  # constraints/<area id>/<cluster id>/additional-constraints.ini
  
  # target dir
  dir_path <- file.path(opts$inputPath, 
                        "st-storage", 
                        "constraints", 
                        area,
                        cluster_name)
  
  # ini file path
  path_contraint_ini <- file.path(dir_path,
                                  "additional-constraints.ini")
  
  # properties part
  if(!is.null(constraints_properties)){
    # read previous content of ini (if exists)
    previous_params <- .check_constaints_ini(path_file = path_contraint_ini, 
                                             list_data_constraints = constraints_properties)
    
    
    # insert/update
    previous_params_updated <- utils::modifyList(x = previous_params, 
                                                 val = constraints_properties)
    
    # write modified ini file
    writeIni(
      listData = previous_params_updated,
      pathIni = path_contraint_ini,
      overwrite = TRUE
    )
  }
  
  # TS part 
  if(!is.null(constraints_ts)){
    # check ini file => constraint name must be present
    
    # check constraint name
    .check_constaints_ini(path_file = path_contraint_ini, 
                          list_data_constraints = constraints_ts)
    
    # update/overwrite/create
    lapply(names(constraints_ts), 
           function(x){
             fwrite(
               x = constraints_ts[[x]], 
               row.names = FALSE, 
               col.names = FALSE, 
               sep = "\t",
               file = file.path(dir_path, 
                                paste0("rhs_", x, ".txt")))
           })
  }
  
}


.check_constaints_ini <- function(path_file, list_data_constraints){
  # previous properties
  previous_params <- readIniFile(file = path_file)
  
  ## check constraints 
  names_previous_params <- tolower(names(previous_params))
  constraints_names <- names(list_data_constraints)
  
  if (!all(
    constraints_names %in% names_previous_params
  ))
    stop("'",
         paste0(setdiff(constraints_names, 
                        names_previous_params), 
                collapse = ", "), 
         "' doesn't exist, it can't be edited. You can create constaints with createCluster().", 
         call. = FALSE)
  
  return(previous_params)
}

