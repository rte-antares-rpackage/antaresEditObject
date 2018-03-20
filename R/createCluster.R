#' Create a thermal cluster
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
#' @param overwrite Logical, overwrite the cluster or not.
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#'
#' @return An updated list containing various information about the simulation.
#' @export
#' 
#' @importFrom antaresRead simOptions
#' @importFrom stats setNames
#' @importFrom utils read.table write.table
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
#'   `marginal-cost` = 50
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
#'   `marginal-cost` = 50
#' )
#' 
#' 
#' # Pre-process data : 
#' 
#' #Æ this is the default data :
#' createCluster(
#'   area = "fr", 
#'   cluster_name = "my_cluster",
#'   prepro_data = matrix(
#'     data = c(rep(1, times = 365 * 2), rep(0, times = 365 * 4)), 
#'     ncol = 6
#'   )
#' )
#' 
#' # with a data.frame
#' createCluster(
#'   area = "fr", 
#'   cluster_name = "my_cluster",
#'   prepro_data = data.frame(
#'     v1 = rep(7, 365), # colum name doesn't matter
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
#'     data = c(rep(1, times = 365 * 24 * 3), rep(0, times = 365 * 24 * 1)),
#'     ncol = 4
#'   )
#' )
#' 
#' # with a data.frame
#' createCluster(
#'   area = "fr", 
#'   cluster_name = "my_cluster",
#'   prepro_modulation = data.frame(
#'     var1 = rep(0, 8760), # colum name doesn't matter
#'     var2 = rep(1, 8760),
#'     var3 = rep(0, 8760),
#'     var4 = rep(1, 8760)
#'   )
#' )
#' 
#' }
createCluster <- function(area, cluster_name, ..., time_series = NULL,
                          prepro_data = NULL, prepro_modulation = NULL,
                          add_prefix = TRUE, overwrite = FALSE,
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

  # named list for writing ini file
  # params_cluster <- stats::setNames(object = list(params_cluster), nm = cluster_name)

  # path to ini file
  path_clusters_ini <- file.path(inputPath, "thermal", "clusters", tolower(area), "list.ini")

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
    inputPath, "thermal", "series", tolower(area), tolower(cluster_name)),
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
    file = file.path(inputPath, "thermal", "series", tolower(area), tolower(cluster_name), "series.txt")
  )


  # prepro
  dir.create(
    path = file.path(inputPath, "thermal", "prepro", tolower(area), tolower(cluster_name)),
    recursive = TRUE, showWarnings = FALSE
  )
  
  if (is.null(prepro_data))
    prepro_data <- matrix(data = c(rep(1, times = 365 * 2), rep(0, times = 365 * 4)), ncol = 6)
  utils::write.table(
    x = prepro_data, row.names = FALSE, col.names = FALSE, sep = "\t",
    file = file.path(inputPath, "thermal", "prepro", tolower(area), tolower(cluster_name), "data.txt")
  )
  
  
  if (is.null(prepro_modulation))
    prepro_modulation <- matrix(data = c(rep(1, times = 365 * 24 * 3), rep(0, times = 365 * 24 * 1)), ncol = 4)
  
  utils::write.table(
    x = prepro_modulation, row.names = FALSE, col.names = FALSE, sep = "\t",
    file = file.path(inputPath, "thermal", "prepro", tolower(area), tolower(cluster_name), "modulation.txt")
  )


  # Maj simulation
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


