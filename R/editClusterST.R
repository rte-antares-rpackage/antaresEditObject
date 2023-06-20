#' @title Edit a short-term storage cluster 
#' 
#' @description 
#' `r antaresEditObject:::badge_api_no()`
#' 
#' Edit parameters and time series of an existing `st-storage` cluster (Antares studies >= v8.6.0).
#' 
#' @param area The area where to create the cluster.
#' @param cluster_name Name for the cluster, it will prefixed by area name, unless you set `add_prefix = FALSE`.
#' @param group Group of the cluster, one of : "PSP_open", "PSP_closed", "Pondage", "Battery", "Other". It corresponds to the type of stockage.
#' @param ... Parameters to write in the Ini file. Careful!
#'  Some parameters must be set as `integers` to avoid warnings in Antares, for example, 
#'  to set `unitcount`, you'll have to use `unitcount = 1L`.
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
                          group = "Other",
                          ...,
                          PMAX_injection = NULL,
                          PMAX_withdrawal = NULL,
                          inflows = NULL,
                          lower_rule_curve = NULL,
                          upper_rule_curve = NULL,
                          add_prefix = TRUE, 
                          opts = antaresRead::simOptions()) {

  # basics checks
  assertthat::assert_that(inherits(opts, "simOptions"))
  check_active_ST(opts, check_dir = TRUE)
  check_area_name(area, opts)
  
  # statics groups
  st_storage_group <- c("PSP_open", 
                        "PSP_closed", 
                        "Pondage", 
                        "Battery",
                        "Other")
  
  # Check valid group
  if (!is.null(group) && !tolower(group) %in% tolower(st_storage_group))
    warning(
      "Group: '", group, "' is not a valid name recognized by Antares,",
      " you should be using one of: ", 
      paste(st_storage_group, collapse = ", ")
    )
  
  # Cluster's parameters
  area <- tolower(area)
  params_cluster <- hyphenize_names(list(...))
  
  if (add_prefix)
    cluster_name <- paste(area, cluster_name, sep = "_")
  
  ##### API block ----
  if (is_api_study(opts)) {
    
    # update parameters if something else than name
    if (length(params_cluster) > 1) {
      currPath <- ifelse(identical(cluster_type, "renewables"), "input/renewables/clusters/%s/list/%s", "input/thermal/clusters/%s/list/%s")
      writeIni(
        listData = params_cluster,
        pathIni = sprintf(currPath, area, cluster_name),
        opts = opts
      )
    }
    
    return(invisible(opts))
  }
  #####-
  
  # path to ini file
  path_clusters_ini <- file.path(opts$inputPath, "st-storage", "clusters", tolower(area), "list.ini")
  if (!file.exists(path_clusters_ini))
    stop("'", cluster_name, "' in area '", area, "' doesn't seems to exist.")
  
  # read previous content of ini
  previous_params <- readIniFile(file = path_clusters_ini)
  
  if (!tolower(cluster_name) %in% tolower(names(previous_params))){
    stop(
      "'", cluster_name, "' doesn't exist, it can't be edited. You can create cluster with createCluster().",
      call. = FALSE
    )
  }
  
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
  
  # PMAX_injection = NULL,
  # PMAX_withdrawal = NULL,
  # inflows = NULL,
  # lower_rule_curve = NULL,
  # upper_rule_curve 
 
  
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
