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
  
  area <- tolower(area)
  check_area_name(area, opts)
  api_study <- is_api_study(opts)
  api_mocked <- is_api_mocked(opts)
  is_thermal <- identical(cluster_type, "thermal")
  
  # check cluster short-term storage existence
  if (identical(cluster_type,"st-storage")) {
    # To avoid failure in an unit test (API is mocked) we add this block
    if (api_study && api_mocked) {
      cluster_exists <- TRUE
    } else {
      cluster_exists <- check_cluster_name(area, cluster_name, add_prefix, opts)
    }
    assertthat::assert_that(cluster_exists, msg = "Cluster can not be removed. It does not exist.")
  }
  
  cluster_name <- generate_cluster_name(area, cluster_name, add_prefix)
  
  # check if the cluster can be removed safely, i.e. the cluster is not referenced in a binding constraint
  if (is_thermal) {
    if (!api_study | (api_study && !api_mocked)) {
      bc_not_remove <- detect_pattern_in_binding_constraint(pattern = paste0(area, ".", cluster_name), opts = opts)
      if (!identical(bc_not_remove, character(0))) {
        message("The following binding constraints have the cluster to remove as a coefficient : ", paste0(bc_not_remove, collapse = ", "))
        stop("Can not remove the cluster ", cluster_name, " in the area ", area, ".")
      }
    }
  }
  
  if (api_study) {
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
  
  # Input path
  clustertypePath <- file.path(opts$inputPath, cluster_type)
  
  # Remove from Ini file
  # path to ini file
  path_clusters_ini <- file.path(clustertypePath, "clusters", area, "list.ini")
  
  # read previous content of ini
  previous_params <- readIniFile(file = path_clusters_ini)
  
  # cluster indice
  idx <- which(tolower(names(previous_params)) %in% cluster_name)
  if (length(idx) < 1)
    warning("Cluster '", cluster_name, "' you want to remove doesn't seem to exist in area '", area, "'.")
  
  # Remove entry in list.ini
  previous_params[idx] <- NULL
  
  writeIni(
    listData = previous_params,
    pathIni = path_clusters_ini,
    overwrite = TRUE
  )
  
  # Remove series
  if (length(previous_params) > 0) {
    dirs_to_remove <- file.path(clustertypePath, "series", area, cluster_name)
  } else {
    dirs_to_remove <- file.path(clustertypePath, "series", area)
  }
  
  # Remove prepro
  if (is_thermal) {
    dirs_to_remove <- c(dirs_to_remove, file.path(clustertypePath, "prepro", area))
  }
  lapply(dirs_to_remove, unlink, recursive = TRUE)
  # Maj simulation
  suppressWarnings({
    res <- antaresRead::setSimulationPath(path = opts$studyPath, simulation = "input")
  })
  
  invisible(res)
}
