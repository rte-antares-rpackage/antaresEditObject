#' Create a thermal cluster
#'
#' @param area The area where to create the cluster
#' @param cluster_name cluster name
#' @param ... Parameters to write in the Ini file
#' @param time_series the "ready-made" 8760-hour time-series available for simulation purposes
#' @param prepro_data Preprocess data
#' @param prepro_modulation Preprocess modulation
#' @param overwrite Logical, overwrite the cluster or not.
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#'
#' @return An upddated list containing various information about the simulation.
#' @export
#' 
#' @importFrom antaresRead simOptions
#' @importFrom stats setNames
#' @importFrom utils read.table write.table
#'
#' @examples
#' \dontrun{
#' createCluster(area = "fr", cluster_name = "fr_gas",
#'               group = "other", `marginal-cost` = 50)
#' }
createCluster <- function(area, cluster_name, ..., time_series = NULL,
                          prepro_data = NULL, prepro_modulation = NULL,
                          overwrite = FALSE,
                          opts = antaresRead::simOptions()) {

  # Input path
  inputPath <- opts$inputPath
  assertthat::assert_that(!is.null(inputPath) && file.exists(inputPath))
  
  if (!area %in% opts$areaList)
    stop(paste(area, "is not a valid area"))
  
  if (! NROW(time_series) %in% c(0, 8760)) {
    stop("Number of rows for time series must be 0 or 8760")
  }
  
  if (! NROW(prepro_modulation) %in% c(0, 8760)) {
    stop("Number of rows for modulation data must be 0 or 8760")
  }
  if (! NCOL(prepro_modulation) %in% c(1, 4)) {
    stop("Number of cols for modulation data must be 0 or 4")
  }

  # Cluster's parameters
  params_cluster <- list(...)

  params_cluster$name <- cluster_name

  # named list for writing ini file
  # params_cluster <- stats::setNames(object = list(params_cluster), nm = cluster_name)

  # path to ini file
  path_clusters_ini <- file.path(inputPath, "thermal", "clusters", area, "list.ini")

  # read previous content of ini
  previous_params <- readIniFile(file = path_clusters_ini)
  
  if (cluster_name %in% names(previous_params) & !overwrite)
    stop(paste(cluster_name, "already exist"))
  
  # params_cluster <- c(previous_params, params_cluster)
  previous_params[[cluster_name]] <- params_cluster

  writeIni(
    listData = previous_params,
    pathIni = path_clusters_ini,
    overwrite = TRUE
  )


  # initialize series
  dir.create(path = file.path(inputPath, "thermal", "series", area, cluster_name), recursive = TRUE, showWarnings = FALSE)
  
  if (is.null(time_series))
    time_series <- character(0)
  
  
  utils::write.table(
    x = time_series, row.names = FALSE, col.names = FALSE, sep = "\t",
    file = file.path(inputPath, "thermal", "series", area, cluster_name, "series.txt")
  )


  # prepro
  dir.create(path = file.path(inputPath, "thermal", "prepro", area, cluster_name), recursive = TRUE, showWarnings = FALSE)
  
  if (is.null(prepro_data))
    prepro_data <- matrix(data = c(rep(1, times = 365 * 2), rep(0, times = 365 * 4)), ncol = 5)
  utils::write.table(
    x = prepro_data, row.names = FALSE, col.names = FALSE, sep = "\t",
    file = file.path(inputPath, "thermal", "prepro", area, cluster_name, "data.txt")
  )
  
  
  if (is.null(prepro_modulation))
    prepro_modulation <- matrix(data = c(rep(1, times = 365 * 24 * 3), rep(0, times = 365 * 24 * 1)), ncol = 4)
  
  utils::write.table(
    x = prepro_modulation, row.names = FALSE, col.names = FALSE, sep = "\t",
    file = file.path(inputPath, "thermal", "prepro", area, cluster_name, "modulation.txt")
  )


  # Maj simulation
  res <- antaresRead::setSimulationPath(path = opts$studyPath, simulation = "input")
  
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


