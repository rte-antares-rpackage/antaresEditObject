#' Remove a cluster
#'
#' @param area Area from which to remove a cluster.
#' @param cluster_name Cluster to remove.
#' @param add_prefix If \code{TRUE}, cluster_name will be prefixed by area's name.
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#'
#' @return An updated list containing various information about the simulation.
#' @export
#'
#' @examples
#' \dontrun{
#' createCluster(area = "fr", cluster_name = "fr_gas",
#'               group = "other", `marginal-cost` = 50)
#' 
#' removeCluster(area = "fr", cluster_name = "fr_gas")
#' 
#' }
#' 
#' 
removeCluster <- function(area, cluster_name, add_prefix = TRUE, 
                          opts = antaresRead::simOptions()) {
  
  
  # Input path
  inputPath <- opts$inputPath
  
  if (add_prefix)
    cluster_name <- paste(area, cluster_name, sep = "_")
  
  # Remove from Ini file
  # path to ini file
  path_clusters_ini <- file.path(inputPath, "thermal", "clusters", tolower(area), "list.ini")
  
  # read previous content of ini
  previous_params <- readIniFile(file = path_clusters_ini)
  
  
  # cluster indice
  ind <- which(tolower(names(previous_params)) %in% tolower(cluster_name))
  
  # Remove
  previous_params[ind] <- NULL
  
  # write
  writeIni(
    listData = previous_params,
    pathIni = path_clusters_ini,
    overwrite = TRUE
  )
  
  if (length(previous_params) > 0) {
    # remove series
    unlink(x = file.path(inputPath, "thermal", "series", tolower(area), tolower(cluster_name)), recursive = TRUE)
    
    # remove prepro
    unlink(x = file.path(inputPath, "thermal", "prepro", tolower(area), tolower(cluster_name)), recursive = TRUE)
  } else {
    # remove series
    unlink(x = file.path(inputPath, "thermal", "series", tolower(area)), recursive = TRUE)
    
    # remove prepro
    unlink(x = file.path(inputPath, "thermal", "prepro", area), recursive = TRUE)
  }
  
  
  
  # Maj simulation
  suppressWarnings({
    res <- antaresRead::setSimulationPath(path = opts$studyPath, simulation = "input")
  })
  
  invisible(res)
}
