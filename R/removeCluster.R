
#' @title Remove a cluster
#' 
#' @description Remove a cluster (thermal or renewable) and all its data.
#'
#' @param area Area from which to remove a cluster.
#' @param cluster_name Cluster to remove.
#' @param add_prefix If \code{TRUE}, cluster_name will be prefixed by area's name.
#' @param opts
#'   List of simulation parameters returned by the function
#'   [antaresRead::setSimulationPath]
#'
#' @return An updated list containing various information about the simulation.
#' 
#' @seealso [createCluster()] or [createClusterRenewable()] to create new clusters, [editCluster()] or [editClusterRenewable()] to edit existing clusters.
#' 
#' @export
#' 
#' @name remove-cluster
#'
#' @examples
#' \dontrun{
#' createCluster(area = "fr", cluster_name = "fr_gas",
#'               group = "other", `marginal-cost` = 50)
#' 
#' removeCluster(area = "fr", cluster_name = "fr_gas")
#' 
#' }
removeCluster <- function(area, 
                          cluster_name, 
                          add_prefix = TRUE, 
                          opts = antaresRead::simOptions()) {
  .removeCluster(
    area = area, 
    cluster_name = cluster_name, 
    add_prefix = add_prefix, 
    cluster_type = "thermal",
    opts = opts
  )
}

#' @export
#' 
#' @rdname remove-cluster
removeClusterRenewable <- function(area, 
                                   cluster_name, 
                                   add_prefix = TRUE, 
                                   opts = antaresRead::simOptions()) {
  .removeCluster(
    area = area, 
    cluster_name = cluster_name, 
    add_prefix = add_prefix, 
    cluster_type = "renewables",
    opts = opts
  )
}


.removeCluster <- function(area, 
                           cluster_name, 
                           add_prefix = TRUE,
                           cluster_type = c("thermal", "renewables"),
                           opts = antaresRead::simOptions()) {
  
  cluster_type <- match.arg(cluster_type)
  
  # Input path
  inputPath <- opts$inputPath
  
  if (add_prefix)
    cluster_name <- paste(area, cluster_name, sep = "_")
  
  # Remove from Ini file
  # path to ini file
  path_clusters_ini <- file.path(inputPath, cluster_type, "clusters", tolower(area), "list.ini")
  
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
    unlink(x = file.path(inputPath, cluster_type, "series", tolower(area), tolower(cluster_name)), recursive = TRUE)
    
    # remove prepro
    unlink(x = file.path(inputPath, cluster_type, "prepro", tolower(area), tolower(cluster_name)), recursive = TRUE)
  } else {
    # remove series
    unlink(x = file.path(inputPath, cluster_type, "series", tolower(area)), recursive = TRUE)
    
    # remove prepro
    unlink(x = file.path(inputPath, cluster_type, "prepro", area), recursive = TRUE)
  }
  
  # Maj simulation
  suppressWarnings({
    res <- antaresRead::setSimulationPath(path = opts$studyPath, simulation = "input")
  })
  
  invisible(res)
}
