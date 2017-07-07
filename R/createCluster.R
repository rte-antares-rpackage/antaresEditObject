#' Create a thermal cluster
#'
#' @param area The area where to create the cluster
#' @param cluster_name cluster name
#' @param ... Parameters to write in the Ini file
#' @param time_series Cluster's time series
#' @param prepro_data Preprocess data
#' @param prepro_modulation Preprocess modulation
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#'
#' @return Nothing
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
                          opts = antaresRead::simOptions()) {

  # Input path
  inputPath <- opts$inputPath

  # Cluster's parameters
  params_cluster <- list(...)

  params_cluster$name <- cluster_name

  # named list for writing ini file
  params_cluster <- stats::setNames(object = list(params_cluster), nm = cluster_name)

  # path to ini file
  path_clusters_ini <- file.path(inputPath, "thermal", "clusters", area, "list.ini")

  # read previous content of ini
  previous_params <- readIniFile(file = path_clusters_ini)
  params_cluster <- c(previous_params, params_cluster)

  writeIni(
    listData = params_cluster,
    pathIni = path_clusters_ini,
    overwrite = TRUE
  )


  # initialize series
  dir.create(path = file.path(inputPath, "thermal", "series", area, cluster_name), showWarnings = FALSE)
  if (is.null(time_series))
    time_series <- character(0)
  utils::write.table(
    x = time_series, row.names = FALSE, col.names = FALSE, sep = "\t",
    file = file.path(inputPath, "thermal", "series", area, cluster_name, "series.txt")
  )


  # prepro
  dir.create(path = file.path(inputPath, "thermal", "prepro", area, cluster_name), showWarnings = FALSE)
  if (is.null(prepro_data))
    prepro_data <- matrix(data = c(rep(1, times = 365 * 2), rep(0, times = 365 * 4)), ncol = 5)
  utils::write.table(
    x = prepro_data, row.names = FALSE, col.names = FALSE, sep = "\t",
    file = file.path(inputPath, "thermal", "prepro", area, cluster_name, "data.txt")
  )
  if (is.null(prepro_modulation))
    prepro_modulation <- matrix(data = c(rep(1, times = 365 * 24 * 3), rep(0, times = 365 * 24 * 1)), ncol = 1)
  utils::write.table(
    x = prepro_modulation, row.names = FALSE, col.names = FALSE, sep = "\t",
    file = file.path(inputPath, "thermal", "prepro", area, cluster_name, "modulation.txt")
  )


  invisible()
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


