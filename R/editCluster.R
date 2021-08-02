
#' @title Edit an existing cluster
#' 
#' @description Edit parameters of an existing cluster (thermal or renewable).
#'
#' @param area The area where the cluster is.
#' @param cluster_name cluster name.
#' @param ... Parameters to write in the Ini file.
#' @param time_series the "ready-made" 8760-hour time-series available for simulation purposes.
#' @param prepro_data Pre-process data, a `data.frame` or `matrix`, 
#'  default is a matrix with 365 rows and 6 columns.
#' @param prepro_modulation Pre-process modulation, a `data.frame` or `matrix`, 
#'  if specified, must have 8760 rows and 1 or 4 columns.
#' @param add_prefix If \code{TRUE}, cluster_name will be prefixed by area name.
#' @param opts
#'   List of simulation parameters returned by the function
#'   [antaresRead::setSimulationPath()]
#'
#' @return An updated list containing various information about the simulation.
#' 
#' @seealso [createCluster()] or [createClusterRenewable()] to create new clusters, [removeCluster()] or [removeClusterRenewable()] to remove clusters.
#' 
#' @export
#' 
#' @name edit-cluster
#' 
#' @importFrom antaresRead setSimulationPath
#' @importFrom assertthat assert_that
#' @importFrom utils modifyList write.table
#'
#' @examples
#' \dontrun{
#' 
#' # Update only nominalCapacity for an existing cluster
#' editCluster(
#'   area = "myarea", 
#'   cluster_name = "mycluster", 
#'   nominalcapacity = 10600.000
#' )
#' 
#' }
editCluster <- function(area,
                        cluster_name, 
                        ..., 
                        time_series = NULL,
                        prepro_data = NULL,
                        prepro_modulation = NULL,
                        add_prefix = TRUE, 
                        opts = antaresRead::simOptions()) {
  .editCluster(
    area = area,
    cluster_name = cluster_name, 
    ..., 
    time_series = time_series,
    prepro_data = prepro_data,
    prepro_modulation = prepro_modulation,
    add_prefix = add_prefix, 
    cluster_type = "thermal",
    opts = opts
  )
}

#' @export
#' 
#' @rdname edit-cluster
editClusterRenewable <- function(area,
                                 cluster_name, 
                                 ..., 
                                 time_series = NULL,
                                 add_prefix = TRUE, 
                                 opts = antaresRead::simOptions()) {
  assertthat::assert_that(class(opts) == "simOptions")
  checkClustersRenewables(opts)
  .editCluster(
    area = area,
    cluster_name = cluster_name, 
    ..., 
    time_series = time_series,
    add_prefix = add_prefix, 
    cluster_type = "renewables",
    opts = opts
  )
}


.editCluster <- function(area,
                         cluster_name, 
                         ..., 
                         time_series = NULL,
                         prepro_data = NULL,
                         prepro_modulation = NULL,
                         add_prefix = TRUE, 
                         cluster_type = c("thermal", "renewables"),
                         opts = antaresRead::simOptions()) {
  # Input path
  inputPath <- opts$inputPath
  assertthat::assert_that(!is.null(inputPath) && file.exists(inputPath))
  cluster_type <- match.arg(cluster_type)
  
  if (!tolower(area) %in% opts$areaList)
    stop(paste(area, "is not a valid area"))
  
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
  params_cluster <- list(...)
  if (add_prefix)
    cluster_name <- paste(area, cluster_name, sep = "_")
  params_cluster$name <- cluster_name
  
  # path to ini file
  path_clusters_ini <- file.path(inputPath, cluster_type, "clusters", tolower(area), "list.ini")
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
  
  ind_cluster <- which(tolower(names(previous_params)) %in% tolower(cluster_name))[1]
  previous_params[[ind_cluster]] <- utils::modifyList(x = previous_params[[ind_cluster]], val = params_cluster)
  names(previous_params)[[ind_cluster]] <- cluster_name
  
  # write modified ini file
  writeIni(
    listData = previous_params,
    pathIni = path_clusters_ini,
    overwrite = TRUE
  )
  
  # datas associated with cluster
  
  if (!is.null(time_series)) {
    utils::write.table(
      x = time_series, row.names = FALSE, col.names = FALSE, sep = "\t",
      file = file.path(inputPath, cluster_type, "series", tolower(area), tolower(cluster_name), "series.txt")
    )
  }
  
  if (!is.null(prepro_data)) {
    utils::write.table(
      x = prepro_data, row.names = FALSE, col.names = FALSE, sep = "\t",
      file = file.path(inputPath, cluster_type, "prepro", tolower(area), tolower(cluster_name), "data.txt")
    )
  }
  
  if (!is.null(prepro_modulation)) {
    utils::write.table(
      x = prepro_modulation, row.names = FALSE, col.names = FALSE, sep = "\t",
      file = file.path(inputPath, cluster_type, "prepro", tolower(area), tolower(cluster_name), "modulation.txt")
    )
  }
  
  # Maj simulation
  suppressWarnings({
    res <- antaresRead::setSimulationPath(path = opts$studyPath, simulation = "input")
  })
  
  invisible(res)
}
