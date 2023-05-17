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
#' @param PMAX_charging modulation of charging capacity on an 8760-hour basis. The values are float between 0 and 1.
#' @param PMAX_discharging modulation of discharging capacity on an 8760-hour basis. The values are float between 0 and 1.
#' @param inflow imposed withdrawals from the stock for other uses, The values are integer.
#' @param lower_rule_curve This is the lower limit for filling the stock imposed each hour. The values are float between 0 and 1.
#' @param upper_rule_curve This is the upper limit for filling the stock imposed each hour. The values are float between 0 and 1.
#' @param add_prefix If `TRUE` (the default), `cluster_name` will be prefixed by area name.
#' 
#' @template opts
#' @export
editClusterST <- function(area,
                          cluster_name, 
                          group = "Other",
                          ...,
                          PMAX_charging = NULL,
                          PMAX_discharging = NULL,
                          inflow = NULL,
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
  
  # API block
  if (is_api_study(opts)) {
    
    return(invisible(opts))
  }
  
  
  # Cluster's parameters
  area <- tolower(area)
  params_cluster <- hyphenize_names(list(...))
  
  if (add_prefix)
    cluster_name <- paste(area, cluster_name, sep = "_")
  
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
  
  
  # Maj simulation
  suppressWarnings({
    res <- antaresRead::setSimulationPath(path = opts$studyPath, simulation = "input")
  })
  
  invisible(res)
  
}
