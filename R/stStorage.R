#' @title Create a cluster
#' 
#' @description 
#' `r antaresEditObject:::badge_api_ok()` (ST-storage clusters only)
#' 
#' Create a new ST-storage cluster.
#'
#' @param area The area where to create the cluster.
#' @param cluster_name Name for the cluster, it will prefixed by area name, unless you set `add_prefix = FALSE`.
#' @param group Group of the cluster, one of : "PSP_open", "PSP_closed", "Pondage", "Battery", "Other".
#' @param ... Parameters to write in the Ini file. Careful!
#'  Some parameters must be set as `integers` to avoid warnings in Antares, for example, 
#'  to set `unitcount`, you'll have to use `unitcount = 1L`.
#' @param PMAX_charging modulation of charging capacity on an 8760-hour basis. The values are float between 0 and 1.
#' @param PMAX_discharging modulation of discharging capacity on an 8760-hour basis. The values are float between 0 and 1.
#' @param inflow imposed withdrawals from the stock for other uses, The values are integer.
#' @param lower_rule_curve This is the lower limit for filling the stock imposed each hour. The values are float between 0 and 1.
#' @param upper_rule_curve This is the upper limit for filling the stock imposed each hour. The values are float between 0 and 1.
#' @param add_prefix If `TRUE` (the default), `cluster_name` will be prefixed by area name.
#' @param overwrite Logical, overwrite the cluster or not.
#' 
#' @template opts
#' 
#' @seealso [editCluster()] or [editClusterRES()] to edit existing clusters, [removeCluster()] or [removeClusterRES()] to remove clusters.
#' 
#' @export
#' 
#' 
#' @importFrom antaresRead simOptions
#' @importFrom stats setNames
#' @importFrom utils read.table write.table
#' @importFrom data.table setcolorder year yday month setnames fwrite
#'
#' @examples
#' \dontrun{
#' 
#' library(antaresRead)
#' library(antaresEditObject)
#' 
#' # Create a cluster :
#' createCluster(
#'   area = "fr", 
#'   cluster_name = "my_cluster",
#'   group = "other", 
#'   unitcount = 1L, # or as.integer(1)
#'   marginal_cost = 50
#' )
#' # by default, cluster name is prefixed 
#' # by the area name
#' levels(readClusterDesc()$cluster)
#' # > "fr_my_cluster"
#' 
#' }
createClusterST <- function(area,
                          cluster_name, 
                          group = "Other",
                          ...,
                          PMAX_charging = NULL,
                          PMAX_discharging = NULL,
                          inflow = NULL,
                          lower_rule_curve = NULL,
                          upper_rule_curve = NULL,
                          add_prefix = TRUE, 
                          overwrite = FALSE,
                          opts = antaresRead::simOptions()) {
  st_storage_group <- c("PSP_open", 
                        "PSP_closed", 
                        "Pondage", 
                        "Battery", 
                        "Other")
  if (!is.null(group) && !tolower(group) %in% tolower(st_storage_group))
    warning(
      "Group: '", group, "' is not a valid name recognized by Antares,",
      " you should be using one of: ", paste(st_storage_group, collapse = ", ")
    )
  # Input path
  inputPath <- opts$inputPath
  
  area <- tolower(area)
  
  storage_value <- list(PMAX_charging = list(N=1, string= "PMAX-charging"),
                        PMAX_discharging = list(N=1, string= "PMAX-discharging"),
                        inflow = list(N=0, string= "inflow"),
                        lower_rule_curve = list(N=0, string= "lower-rule-curve"),
                        upper_rule_curve = list(N=1, string= "upper-rule-curve")
  )
  
  for (name in names(storage_value)){
    if (!NROW(get(name)) %in% c(0, 8760)) {
      stop(paste0("Number of rows for ", name, " must be 0 or 8760"))
    } 
  }
  
  # Cluster's parameters
  params_cluster <- antaresEditObject:::hyphenize_names(list(...))
  if (add_prefix)
    cluster_name <- paste(area, cluster_name, sep = "_")
  params_cluster$name <- cluster_name
  
  
  
  
  
  #################
  # API block
  if (antaresEditObject:::is_api_study(opts)) {
    
    cmd <- api_command_generate(
      action = "create_cluster",
      area_id = area,
      cluster_name = cluster_name,
      parameters = params_cluster
    )
    
    api_command_register(cmd, opts = opts)
    `if`(
      should_command_be_executed(opts), 
      api_command_execute(cmd, opts = opts, text_alert = "{.emph create_cluster}: {msg_api}"),
      cli_command_registered("create_cluster")
    )
    
    for (i in names(storage_value)){
      if (is.null(get(i))) {
        currPath <- paste0("input/ST-storages/series/%s/%s/",storage_value[[i]]$string)
        cmd <- api_command_generate(
          action = "replace_matrix",
          target = sprintf(currPath, area, tolower(cluster_name)),
          matrix = time_series
        )
        api_command_register(cmd, opts = opts)
        `if`(
          should_command_be_executed(opts), 
          api_command_execute(cmd, opts = opts, text_alert = "Writing cluster's series: {msg_api}"),
          cli_command_registered("replace_matrix")
        )
      }
    }
    
    return(invisible(opts))
  }
  ##########################
  
  
  
  assertthat::assert_that(!is.null(inputPath) && file.exists(inputPath))
  antaresEditObject:::check_area_name(area, opts)
  
  # named list for writing ini file
  # params_cluster <- stats::setNames(object = list(params_cluster), nm = cluster_name)
  
  # path to ini file containing clusters' name and parameters
  path_clusters_ini <- file.path(inputPath, "ST-storages", "clusters", tolower(area), "list.ini")
  
  # read previous content of ini
  previous_params <- readIniFile(file = path_clusters_ini)
  
  if (tolower(cluster_name) %in% tolower(names(previous_params)) & !overwrite){
    stop(paste(cluster_name, "already exist"))
  } else if (tolower(cluster_name) %in% tolower(names(previous_params)) & overwrite){
    ind_cluster <- which(tolower(names(previous_params)) %in% tolower(cluster_name))[1]
    previous_params[[ind_cluster]] <- params_cluster
    names(previous_params)[[ind_cluster]] <- cluster_name
  } else {
    previous_params[[cluster_name]] <- params_cluster
  }
  
  # params_cluster <- c(previous_params, params_cluster)
  
  writeIni(
    listData = previous_params,
    pathIni = path_clusters_ini,
    overwrite = TRUE
  )
  
  
  # initialize series
  dir.create(
    path = file.path(inputPath, "ST-storages", "series", tolower(area), tolower(cluster_name)),
    recursive = TRUE, showWarnings = FALSE
  )
  
  for (name in names(storage_value)){
    if (is.null(get(name))) {
      k <- matrix(storage_value[[name]]$N,8760)
    
    fwrite(
      x = k, row.names = FALSE, col.names = FALSE, sep = "\t",
      file = file.path(inputPath, "ST-storages", "series", tolower(area), tolower(cluster_name), paste0(storage_value[[name]]$string, ".txt"))
    )
    } 
  }
  
  # Update simulation options object
  suppressWarnings({
    res <- antaresRead::setSimulationPath(path = opts$studyPath, simulation = "input")
  })
  
  invisible(res)

}
