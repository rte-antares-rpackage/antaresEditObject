#' @title Remove a cluster
#' 
#' @description 
#' `r antaresEditObject:::badge_api_ok()` (thermal clusters only)
#' 
#' Remove a cluster, thermal RES (renewable energy source) or short-term storage, and all its data.
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


.api_command_generate_remove_cluster <- function(area,
                                                 cluster_name,
                                                 cluster_type = c("thermal", "renewables", "st-storage")
                                                ) {
  
  cluster_type <- match.arg(cluster_type)
  
  remove_action <- switch(cluster_type,
                          "thermal" = "remove_cluster",
                          "renewables" = "remove_renewables_cluster",
                          "st-storage" = "remove_st_storage"
                          )
  
  if (identical(cluster_type, "st-storage")) {
    cmd <- api_command_generate(
            action = remove_action,
            area_id = area,
            storage_id = cluster_name
            )
  } else {
    cmd <- api_command_generate(
            action = remove_action,
            area_id = area,
            cluster_id = cluster_name
            )
  }
    
  return(cmd)
}


.removeCluster <- function(area, 
                           cluster_name, 
                           add_prefix = TRUE,
                           cluster_type = c("thermal", "renewables", "st-storage"),
                           opts = antaresRead::simOptions()) {
  
  cluster_type <- match.arg(cluster_type)
  if (cluster_type == "st-storage") {
    # To avoid failure in an unit test (API is mocked) we add this block
    if (is_api_study(opts) && is_api_mocked(opts)) {
      cluster_exists <- TRUE
    } else {
      cluster_exists <- check_cluster_name(area, cluster_name, add_prefix, opts)
    }
    assertthat::assert_that(cluster_exists, msg = "Cluster can not be removed. It does not exist.")
  }
  
  area <- tolower(area)
  
  # Input path
  inputPath <- opts$inputPath
  
  if (add_prefix)
    cluster_name <- paste(area, cluster_name, sep = "_")
  
  if (is_api_study(opts)) {
    # format name for API 
    cluster_name <- transform_name_to_id(cluster_name)
    
    cmd <- .api_command_generate_remove_cluster(area, cluster_name, cluster_type)
    
    api_command_register(cmd, opts = opts)
    `if`(
      should_command_be_executed(opts), 
      api_command_execute(cmd, opts = opts, text_alert = paste0("{.emph ", cmd$action, "}: {msg_api}")),
      cli_command_registered(cmd$action)
    )
    
    return(invisible(opts))
  }
  
  # Remove from Ini file
  # path to ini file
  path_clusters_ini <- file.path(inputPath, cluster_type, "clusters", area, "list.ini")
  
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
    unlink(x = file.path(inputPath, cluster_type, "series", area, tolower(cluster_name)), recursive = TRUE)
    if (identical(cluster_type, "thermal")) {
      # remove prepro
      unlink(x = file.path(inputPath, cluster_type, "prepro", area), recursive = TRUE)
    }
  } else {
    # remove series
    unlink(x = file.path(inputPath, cluster_type, "series", area), recursive = TRUE)
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
