#' @title Remove a cluster
#' 
#' @description 
#' `r antaresEditObject:::badge_api_ok()` (thermal clusters only)
#' 
#' Remove a cluster, thermal or RES (renewable energy source), and all its data.
#' 
#'
#' @inheritParams createCluster
#' @template opts
#' 
#' @seealso [createCluster()], [createClusterRES()] or [createClusterST()] to create new clusters,
#'  [editCluster()] or [editClusterRES()] or [editClusterST()] to edit existing clusters.
#' 
#' @export
#' 
#' @name removeCluster
#'
#' @examples
#' \dontrun{
#' createCluster(
#'   area = "fr", 
#'   cluster_name = "fr_gas",
#'   group = "other", 
#'   `marginal-cost` = 50
#' )
#' 
#' removeCluster(area = "fr", cluster_name = "fr_gas")
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
#' @rdname removeCluster
removeClusterRES <- function(area, 
                             cluster_name, 
                             add_prefix = TRUE, 
                             opts = antaresRead::simOptions()) {
  assertthat::assert_that(inherits(opts, "simOptions"))
  check_active_RES(opts, check_dir = TRUE)
  .removeCluster(
    area = area, 
    cluster_name = cluster_name, 
    add_prefix = add_prefix, 
    cluster_type = "renewables",
    opts = opts
  )
}

#' @export
#' 
#' @rdname removeCluster
removeClusterST <- function(area, 
                            cluster_name, 
                            add_prefix = TRUE, 
                            opts = antaresRead::simOptions()) {
  assertthat::assert_that(inherits(opts, "simOptions"))
  .removeCluster(
    area = area, 
    cluster_name = cluster_name, 
    add_prefix = add_prefix, 
    cluster_type = "st-storage",
    opts = opts
  )
}


.removeCluster <- function(area, 
                           cluster_name, 
                           add_prefix = TRUE,
                           cluster_type = c("thermal", "renewables","st-storage"),
                           opts = antaresRead::simOptions()) {
  
  cluster_type <- match.arg(cluster_type)
  
  area <- tolower(area)
  
  # Input path
  inputPath <- opts$inputPath
  
  if (add_prefix)
    cluster_name <- paste(area, cluster_name, sep = "_")
  
  if (is_api_study(opts)) {
    # Factorization with sprintf() once each type is implemented
    # Renewables
    if (identical(cluster_type, "renewables")) {
      cmd <- api_command_generate(
        action = "remove_renewables_cluster",
        area_id = area,
        cluster_id = cluster_name
      )
      api_command_register(cmd, opts = opts)
      `if`(
        should_command_be_executed(opts), 
        api_command_execute(cmd, opts = opts, text_alert = "{.emph remove_renewables_cluster}: {msg_api}"),
        cli_command_registered("remove_renewables_cluster")
      )
    }
    
    # ST Storage
    if (identical(cluster_type, "st-storage"))
      stop("st-storage clusters not implemented with the API yet.")
    
    # Thermal
    if (identical(cluster_type, "thermal")) {
      cmd <- api_command_generate(
        action = "remove_cluster",
        area_id = area,
        cluster_id = cluster_name
      )
      api_command_register(cmd, opts = opts)
      `if`(
        should_command_be_executed(opts), 
        api_command_execute(cmd, opts = opts, text_alert = "{.emph remove_cluster}: {msg_api}"),
        cli_command_registered("remove_cluster")
      )
    }
    
    return(invisible(opts))
  }
  
  # Remove from Ini file
  # path to ini file
  path_clusters_ini <- file.path(inputPath, cluster_type, "clusters", tolower(area), "list.ini")
  
  # read previous content of ini
  previous_params <- readIniFile(file = path_clusters_ini)
  
  # cluster indice
  ind <- which(tolower(names(previous_params)) %in% tolower(cluster_name))
  if (length(ind) < 1)
    warning("Cluster '", cluster_name, "' you want to remove doesn't seem to exist in area '", area, "'.")
  
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
    if (identical(cluster_type, "thermal")) {
      # remove prepro
      unlink(x = file.path(inputPath, cluster_type, "prepro", area), recursive = TRUE)
    }
  } else {
    # remove series
    unlink(x = file.path(inputPath, cluster_type, "series", tolower(area)), recursive = TRUE)
    if (identical(cluster_type, "thermal")) {
      # remove prepro
      unlink(x = file.path(inputPath, cluster_type, "prepro", area), recursive = TRUE)
    }
  }
  
  
  # Maj simulation
  suppressWarnings({
    res <- antaresRead::setSimulationPath(path = opts$studyPath, simulation = "input")
  })
  
  invisible(res)
}
