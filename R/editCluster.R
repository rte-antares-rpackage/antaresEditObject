
#' Edit an existing cluster
#'
#' @param area The area where to create the cluster.
#' @param cluster_name cluster name.
#' @param ... Parameters to write in the Ini file.
#' @param time_series the "ready-made" 8760-hour time-series available for simulation purposes.
#' @param prepro_data Pre-process data, a \code{data.frame} or \code{matrix}, 
#'  default is a matrix with 365 rows and 6 columns.
#' @param prepro_modulation Pre-process modulation, a \code{data.frame} or \code{matrix}, 
#'  if specified, must have 8760 rows and 1 or 4 columns.
#' @param add_prefix If \code{TRUE}, cluster_name will be prefixed by area name.
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#'
#' @return An updated list containing various information about the simulation.
#' @export
#' 
#' @importFrom antaresRead setSimulationPath
#' @importFrom assertthat assert_that
#' @importFrom utils modifyList write.table
#'
#' @examples
#' \dontrun{
#' 
#' # Update only nominalCapacity for an existing cluster
#' editCluster(area = "myarea", cluster_name = "mycluster", nominalcapacity = 10600.000)
#' 
#' }
editCluster <- function(area, cluster_name, ..., time_series = NULL,
                        prepro_data = NULL, prepro_modulation = NULL,
                        add_prefix = TRUE, 
                        opts = antaresRead::simOptions()) {
  # Input path
  inputPath <- opts$inputPath
  assertthat::assert_that(!is.null(inputPath) && file.exists(inputPath))
  
  if (!tolower(area) %in% opts$areaList)
    stop(paste(area, "is not a valid area"))
  
  if (! NROW(time_series) %in% c(0, 8736, 8760)) {
    stop("Number of rows for time series must be 0 or 8760")
  }
  
  if (! NROW(prepro_modulation) %in% c(0, 8736, 8760)) {
    stop("Number of rows for modulation data must be 0 or 8760")
  }
  if (! NCOL(prepro_modulation) %in% c(1, 4)) {
    stop("Number of cols for modulation data must be 0 or 4")
  }
  
  # Cluster's parameters
  params_cluster <- list(...)
  if (add_prefix)
    cluster_name <- paste(area, cluster_name, sep = "_")
  params_cluster$name <- cluster_name
  
  # path to ini file
  path_clusters_ini <- file.path(inputPath, "thermal", "clusters", tolower(area), "list.ini")
  
  # read previous content of ini
  previous_params <- readIniFile(file = path_clusters_ini)
  
  if (!tolower(cluster_name) %in% tolower(names(previous_params))){
    stop(paste(cluster_name, "doesn't exist!"))
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
      file = file.path(inputPath, "thermal", "series", tolower(area), tolower(cluster_name), "series.txt")
    )
  }
  
  if (!is.null(prepro_data)) {
    utils::write.table(
      x = prepro_data, row.names = FALSE, col.names = FALSE, sep = "\t",
      file = file.path(inputPath, "thermal", "prepro", tolower(area), tolower(cluster_name), "data.txt")
    )
  }
  
  if (!is.null(prepro_modulation)) {
    utils::write.table(
      x = prepro_modulation, row.names = FALSE, col.names = FALSE, sep = "\t",
      file = file.path(inputPath, "thermal", "prepro", tolower(area), tolower(cluster_name), "modulation.txt")
    )
  }
  
  # Maj simulation
  suppressWarnings({
    res <- antaresRead::setSimulationPath(path = opts$studyPath, simulation = "input")
  })
  
  invisible(res)
}
