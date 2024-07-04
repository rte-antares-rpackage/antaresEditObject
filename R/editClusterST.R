#' @title Edit a short-term storage cluster 
#' 
#' @description 
#' `r antaresEditObject:::badge_api_ok()`
#' 
#' Edit parameters and time series of an existing `st-storage` cluster (Antares studies >= v8.6.0).
#' 
#' @param area The area where to create the cluster.
#' @param cluster_name Name for the cluster, it will prefixed by area name, unless you set `add_prefix = FALSE`.
#' @param group Group of the cluster, one of : "PSP_open", "PSP_closed", "Pondage", "Battery", "Other". It corresponds to the type of stockage.
#' @param storage_parameters Parameters to write in the Ini file. 
#' @param PMAX_injection modulation of charging capacity on an 8760-hour basis. The values are float between 0 and 1.
#' @param PMAX_withdrawal modulation of discharging capacity on an 8760-hour basis. The values are float between 0 and 1.
#' @param inflows imposed withdrawals from the stock for other uses, The values are integer.
#' @param lower_rule_curve This is the lower limit for filling the stock imposed each hour. The values are float between 0 and 1.
#' @param upper_rule_curve This is the upper limit for filling the stock imposed each hour. The values are float between 0 and 1.
#' @param add_prefix If `TRUE` (the default), `cluster_name` will be prefixed by area name.
#' 
#' @template opts
#' 
#' @seealso [createClusterST()] to edit existing clusters, [removeClusterST()] to remove clusters.
#' 
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
                          add_prefix = TRUE, 
                          opts = antaresRead::simOptions()) {

  # basic checks
  assertthat::assert_that(inherits(opts, "simOptions"))
  check_active_ST(opts, check_dir = TRUE)
  check_area_name(area, opts)
  
  # statics groups
  st_storage_group <- c("PSP_open", 
                        "PSP_closed", 
                        "Pondage", 
                        "Battery",
                        paste0("Other", 
                               seq(1,5)))
  
  # check valid group
  if (!is.null(group) && !tolower(group) %in% tolower(st_storage_group))
    stop(
      "Group: '", group, "' is not a valid group recognized by Antares,",
      " you should be using one of: ", 
      paste(st_storage_group, collapse = ", "), call. = FALSE
    )
  
  ##
  # check parameters (ini file)
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
    
    # check list of parameters
    params_cluster <- hyphenize_names(storage_parameters)
  }
  
  # make list of parameters
  area <- tolower(area)
  if(!(is.null(params_cluster)&&is.null(group))){
    cluster_name <- generate_cluster_name(area, cluster_name, add_prefix)
    params_cluster <- c(list(name = cluster_name, group = group), 
                        params_cluster)
  }
  if(is.null(group))
    params_cluster$group <- NULL
  
  ##### API block ----
  if (is_api_study(opts)) {
    # format name for API 
    cluster_name <- transform_name_to_id(cluster_name)
    
    # /!\ temporary solution /!\ 
      # as the endpoint does not return an error if the cluster does not exist 
    if(!is_api_mocked(opts)){
      cluster_exists <- check_cluster_name(area, cluster_name, add_prefix, opts)
      assertthat::assert_that(cluster_exists, 
                              msg = paste0("Cluster '", 
                                           cluster_name, 
                                           "' does not exist. It can not be edited."))
    }
    
    # update parameters if something else than name
    if (length(params_cluster) > 1) {
      currPath <- "input/st-storage/clusters/%s/list/%s" 
      writeIni(
        listData = params_cluster,
        pathIni = sprintf(currPath, area, cluster_name),
        opts = opts
      )
    }
    
    # update data
    names_data_params <- c("PMAX_injection",
                           "PMAX_withdrawal",
                           "inflows",
                           "lower_rule_curve",
                           "upper_rule_curve")
    
    for (i in names_data_params){
      if (!is.null(get(i))) {
        # format name for API 
        data_param_name <- transform_name_to_id(i, id_dash = TRUE)
        
        currPath <- paste0("input/st-storage/series/%s/%s/",data_param_name)
        cmd <- api_command_generate(
          action = "replace_matrix",
          target = sprintf(currPath, area, cluster_name),
          matrix = get(i)
        )
        api_command_register(cmd, opts = opts)
        `if`(
          should_command_be_executed(opts),
          api_command_execute(cmd, 
                              opts = opts, 
                              text_alert = paste0("Update ", 
                                                  i, 
                                                  " cluster's series: {msg_api}")),
          cli_command_registered("replace_matrix")
        )
      }
    }
    
    return(invisible(opts))
  }
  #####-
  
  # path to ini file
  path_clusters_ini <- file.path(opts$inputPath, 
                                 "st-storage", 
                                 "clusters", 
                                 area, 
                                 "list.ini")
  if (!file.exists(path_clusters_ini))
    stop("'", cluster_name, "' in area '", area, "' doesn't seems to exist.")
  
  # only edition if parameters are no NULL
  if(is.null(params_cluster))
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
  
  ##
  # check DATA (series/)
  ##
  
  # datas associated with cluster
  path_txt_file <- file.path(opts$inputPath, 
                             "st-storage", 
                             "series", 
                             tolower(area), 
                             tolower(cluster_name))
  
  # PMAX_injection
  if(!is.null(PMAX_injection)){
    fwrite(
      x = PMAX_injection, row.names = FALSE, col.names = FALSE, sep = "\t",
      file = file.path(path_txt_file, 
                       paste0("PMAX-injection", ".txt"))
    )
  }
  
  # PMAX_withdrawal
  if(!is.null(PMAX_withdrawal)){
    fwrite(
      x = PMAX_withdrawal, row.names = FALSE, col.names = FALSE, sep = "\t",
      file = file.path(path_txt_file, 
                       paste0("PMAX-withdrawal", ".txt"))
    )
  }
  
  # inflows
  if(!is.null(inflows)){
    fwrite(
      x = inflows, row.names = FALSE, col.names = FALSE, sep = "\t",
      file = file.path(path_txt_file, 
                       paste0("inflows", ".txt"))
    )
  }
  
  # lower_rule_curve
  if(!is.null(lower_rule_curve)){
    fwrite(
      x = lower_rule_curve, row.names = FALSE, col.names = FALSE, sep = "\t",
      file = file.path(path_txt_file, 
                       paste0("lower-rule-curve", ".txt"))
    )
  }
  
  # upper_rule_curve 
  if(!is.null(upper_rule_curve)){
    fwrite(
      x = upper_rule_curve, row.names = FALSE, col.names = FALSE, sep = "\t",
      file = file.path(path_txt_file, 
                       paste0("upper-rule-curve", ".txt"))
    )
  }
  
  # Maj simulation
  suppressWarnings({
    res <- antaresRead::setSimulationPath(path = opts$studyPath, simulation = "input")
  })
  
  invisible(res)
  
}
