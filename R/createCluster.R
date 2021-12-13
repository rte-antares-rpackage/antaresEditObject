
#' @title Create a cluster
#' 
#' @description Create a new thermal or RES (renewable energy source) cluster.
#'
#' @param area The area where to create the cluster.
#' @param cluster_name Name for the cluster, it will prefixed by area name, unless you set `add_prefix = FALSE`.
#' @param group Group of the cluster, depends on cluster type:
#'  - thermal cluster, one of: Gas, Hard coal, Lignite, Mixed fuel, Nuclear, Oil, Other, Other 2, Other 3, Other 4.
#'  - renewable cluster, one of: Wind Onshore, Wind Offshore, Solar Thermal, Solar PV, Solar Rooftop, Other RES 1, Other RES 2, Other RES 3, Other RES 4.
#' @param ... Parameters to write in the Ini file. Careful!
#'  Some parameters must be set as `integers` to avoid warnings in Antares, for example, 
#'  to set \code{unitcount}, you'll have to use `unitcount = 1L`.
#' @param time_series the "ready-made" 8760-hour time-series available for simulation purposes.
#' @param prepro_data Pre-process data, a `data.frame` or `matrix`, 
#'  default is a matrix with 365 rows and 6 columns.
#' @param prepro_modulation Pre-process modulation, a `data.frame` or `matrix`, 
#'  if specified, must have 8760 rows and 1 or 4 columns.
#' @param add_prefix If `TRUE` (the default), `cluster_name` will be prefixed by area name.
#' @param overwrite Logical, overwrite the cluster or not.
#' @param opts
#'   List of simulation parameters returned by the function
#'  [antaresRead::setSimulationPath()]
#'
#' @return An updated list containing various information about the simulation.
#' 
#' @seealso [editCluster()] or [editClusterRES()] to edit existing clusters, [removeCluster()] or [removeClusterRES()] to remove clusters.
#' 
#' @export
#' 
#' @name create-cluster
#' 
#' @importFrom antaresRead simOptions
#' @importFrom stats setNames
#' @importFrom utils read.table write.table
#' @importFrom data.table setcolorder year yday month setnames
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
#' # To prevent this, use `add_prefix`
#' createCluster(
#'   area = "fr", 
#'   cluster_name = "my_cluster",
#'   add_prefix = FALSE,
#'   group = "other", 
#'   marginal_cost = 50
#' )
#' levels(readClusterDesc()$cluster)
#' # > "my_cluster"
#' 
#' 
#' # Create a RES cluster :
#' createClusterRES(
#'   area = "fr", 
#'   cluster_name = "my_cluster_res",
#'   group = "other", 
#'   unitcount = 1L, # or as.integer(1)
#'   nominalcapacity = 50,
#'   ts_interpretation = "power-generation"
#' ) 
#' 
#' # You can also specify that the Time-Series of the RES cluster are
#' # production factors :
#' createClusterRES(
#'   area = "fr", 
#'   cluster_name = "my_cluster_res",
#'   group = "other", 
#'   unitcount = 1L, # or as.integer(1)
#'   nominalcapacity = 50,
#'   ts_interpretation = "production-factor"
#' )
#' 
#' 
#' # Pre-process data : 
#' 
#' # this is the default data :
#' createCluster(
#'   area = "fr", 
#'   cluster_name = "my_cluster",
#'   prepro_data = matrix(
#'     data = c(rep(1, times = 365 * 2),
#'              rep(0, times = 365 * 4)), 
#'     ncol = 6
#'   )
#' )
#' 
#' # with a data.frame
#' createCluster(
#'   area = "fr", 
#'   cluster_name = "my_cluster",
#'   prepro_data = data.frame(
#'     v1 = rep(7, 365), # column name doesn't matter
#'     v2 = rep(27, 365),
#'     v3 = rep(0.05, 365),
#'     v4 = rep(0.12, 365),
#'     v5 = rep(0, 365),
#'     v6 = rep(1, 365)
#'   )
#' )
#' 
#' 
#' # Pre-process modulation : 
#' # this is the default data
#' createCluster(
#'   area = "fr", 
#'   cluster_name = "my_cluster",
#'   prepro_modulation =  = matrix(
#'     data = c(rep(1, times = 365 * 24 * 3),
#'              rep(0, times = 365 * 24 * 1)),
#'     ncol = 4
#'   )
#' )
#' 
#' # with a data.frame
#' createCluster(
#'   area = "fr", 
#'   cluster_name = "my_cluster",
#'   prepro_modulation = data.frame(
#'     var1 = rep(0, 8760), # column name doesn't matter
#'     var2 = rep(1, 8760),
#'     var3 = rep(0, 8760),
#'     var4 = rep(1, 8760)
#'   )
#' )
#' 
#' }
createCluster <- function(area, 
                          cluster_name, 
                          group = "Other",
                          ...,
                          time_series = NULL,
                          prepro_data = NULL,
                          prepro_modulation = NULL,
                          add_prefix = TRUE, 
                          overwrite = FALSE,
                          opts = antaresRead::simOptions()) {
  thermal_group <- c("Gas",
                     "Hard coal",
                     "Lignite",
                     "Mixed fuel",
                     "Nuclear",
                     "Oil",
                     "Other",
                     "Other 2",
                     "Other 3",
                     "Other 4")
  if (!is.null(group) && !group %in% thermal_group) 
    warning(
      "Group: '", group, "' is not a valid name recognized by Antares,",
      " you should be using one of: ", paste(thermal_group, collapse = ", ")
    )
  .createCluster(
    area = area, 
    cluster_name = cluster_name, 
    group = group,
    ...,
    time_series = time_series,
    prepro_data = prepro_data,
    prepro_modulation = prepro_modulation,
    add_prefix = add_prefix, 
    overwrite = overwrite,
    cluster_type = "thermal",
    opts = opts
  )
}

#' @export
#' 
#' @rdname create-cluster
createClusterRES <- function(area, 
                             cluster_name, 
                             group = "Other RES 1",
                             ...,
                             time_series = NULL,
                             add_prefix = TRUE, 
                             overwrite = FALSE,
                             opts = antaresRead::simOptions()) {
  assertthat::assert_that(inherits(opts, "simOptions"))
  check_active_RES(opts)
  renewables_group <- c("Wind Onshore",
                        "Wind Offshore",
                        "Solar Thermal",
                        "Solar PV",
                        "Solar Rooftop",
                        "Other RES 1",
                        "Other RES 2",
                        "Other RES 3",
                        "Other RES 4")
  if (!is.null(group) && !group %in% renewables_group) 
    warning(
      "Group: '", group, "' is not a valid name recognized by Antares,",
      " you should be using one of: ", paste(renewables_group, collapse = ", ")
    )
  initialize_RES(opts)
  .createCluster(
    area = area, 
    cluster_name = cluster_name,
    group = group,
    ...,
    time_series = time_series,
    prepro_data = NULL,
    prepro_modulation = NULL,
    add_prefix = add_prefix, 
    overwrite = overwrite,
    cluster_type = "renewables",
    opts = opts
  )
}


.createCluster <- function(area, 
                           cluster_name, 
                           ...,
                           time_series = NULL,
                           prepro_data = NULL,
                           prepro_modulation = NULL,
                           add_prefix = TRUE, 
                           overwrite = FALSE,
                           cluster_type = c("thermal", "renewables"),
                           opts = antaresRead::simOptions()) {
  
  # Input path
  inputPath <- opts$inputPath
  assertthat::assert_that(!is.null(inputPath) && file.exists(inputPath))
  cluster_type <- match.arg(cluster_type)
  
  check_area_name(area, opts)
  
  if (!NROW(time_series) %in% c(0, 8736, 8760)) {
    stop("Number of rows for time series must be 0 or 8760")
  }
  
  if (!NROW(prepro_modulation) %in% c(0, 8736, 8760)) {
    stop("Number of rows for modulation data must be 0 or 8760")
  }
  if (!NCOL(prepro_modulation) %in% c(1, 4)) {
    stop("Number of cols for modulation data must be 0 or 4")
  }
  
  # Cluster's parameters
  params_cluster <- hyphenize_names(list(...))
  if (add_prefix)
    cluster_name <- paste(area, cluster_name, sep = "_")
  params_cluster$name <- cluster_name
  
  # named list for writing ini file
  # params_cluster <- stats::setNames(object = list(params_cluster), nm = cluster_name)
  
  # path to ini file containing clusters' name and parameters
  path_clusters_ini <- file.path(inputPath, cluster_type, "clusters", tolower(area), "list.ini")
  
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
  dir.create(path = file.path(
    inputPath, cluster_type, "series", tolower(area), tolower(cluster_name)),
    recursive = TRUE, showWarnings = FALSE
  )
  
  if (is.null(time_series))
    time_series <- character(0)
  
  if (NROW(time_series) == 8736) {
    fill_mat <- matrix(
      data = rep(0, times = 24 * ncol(time_series)), 
      ncol = ncol(time_series),
      dimnames = list(NULL, colnames(time_series))
    )
    time_series <- rbind(time_series, fill_mat)
  }
  
  utils::write.table(
    x = time_series, row.names = FALSE, col.names = FALSE, sep = "\t",
    file = file.path(inputPath, cluster_type, "series", tolower(area), tolower(cluster_name), "series.txt")
  )
  
  
  # prepro
  if (identical(cluster_type, "thermal")) {
    dir.create(
      path = file.path(inputPath, cluster_type, "prepro", tolower(area), tolower(cluster_name)),
      recursive = TRUE, showWarnings = FALSE
    )
    
    if (is.null(prepro_data))
      prepro_data <- matrix(data = c(rep(1, times = 365 * 2), rep(0, times = 365 * 4)), ncol = 6)
    utils::write.table(
      x = prepro_data, row.names = FALSE, col.names = FALSE, sep = "\t",
      file = file.path(inputPath, cluster_type, "prepro", tolower(area), tolower(cluster_name), "data.txt")
    )
    
    
    if (is.null(prepro_modulation))
      prepro_modulation <- matrix(data = c(rep(1, times = 365 * 24 * 3), rep(0, times = 365 * 24 * 1)), ncol = 4)
    
    utils::write.table(
      x = prepro_modulation, row.names = FALSE, col.names = FALSE, sep = "\t",
      file = file.path(inputPath, cluster_type, "prepro", tolower(area), tolower(cluster_name), "modulation.txt")
    )
  }
  
  
  # Update simulation options object
  suppressWarnings({
    res <- antaresRead::setSimulationPath(path = opts$studyPath, simulation = "input")
  })
  
  invisible(res)
}




# # ex
# turb_d_generator <- list(
#   name = "turb_d_generator",
#   group = "other",
#   unitcount = 1,
#   nominalcapacity = 10600.000000,
#   `marginal-cost` = 0.010000,
#   `market-bid-cost` = 0.010000
# )
#
# l <- createCluster(
#   area = "fr", cluster_name = "turb_d_generator",
#   group = "other",
#   unitcount = 1,
#   nominalcapacity = 10600.000000,
#   `marginal-cost` = 0.010000,
#   `market-bid-cost` = 0.010000
# )


