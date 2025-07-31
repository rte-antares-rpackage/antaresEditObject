#' @title Remove a cluster
#' 
#' @description 
#' `r antaresEditObject:::badge_api_ok()` 
#' 
#' Delete cluster(s), thermal, renewable (renewable energy source) or short-term storage, 
#' along with all its data (properties + TS).
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
                           cluster_type = c("thermal", "renewables", "st-storage"),
                           opts = antaresRead::simOptions()) {
  
  cluster_type <- match.arg(cluster_type)
  
  # tolower area ----
  area <- tolower(area)
  #check area ----
  check_area_name(area, opts)
  api_study <- is_api_study(opts)
  is_thermal <- identical(cluster_type, "thermal")
  
  # add prefix to cluster's name
  cluster_name <- generate_cluster_name(area, 
                                        cluster_name, 
                                        add_prefix)
  
  # check if the cluster can be removed safely, i.e. the cluster is not referenced in a binding constraint
  if (is_thermal) {
    if (!api_study) {
      bc_not_remove <- detect_pattern_in_binding_constraint(
        pattern = paste0(area, ".", cluster_name), 
        opts = opts)
      if (!identical(bc_not_remove, character(0))) 
        warning("The following binding constraints have the cluster to remove as a coefficient : ", 
                paste0(bc_not_remove, collapse = ", "))
    }
  }
  
  if (api_study) {
    # format name for API 
    cluster_name <- transform_name_to_id(cluster_name)
    
    # adapt type for api 
    api_type <- switch(
      cluster_type,
      "thermal" = "clusters/thermal",
      "renewables" = "clusters/renewable",
      "st-storage" = "storages"
    )
    
    # body request
    body <- jsonlite::toJSON(cluster_name)
    
    # delete
    api_delete(opts = opts, 
               endpoint =  file.path(opts$study_id, 
                                     "areas", 
                                     area, 
                                     api_type), 
               body = body,
               encode = "raw")
    
    cli::cli_alert_success("Endpoint {.emph {'Delete Cluster'}} {.emph 
                          [{cluster_type}] {.strong {cluster_name}}} success")
    
    return(invisible(opts))
  }
  
  # Input path
  clustertypePath <- file.path(opts$inputPath, cluster_type)
  
  # Remove from Ini file
  # path to ini file
  path_clusters_ini <- file.path(clustertypePath, "clusters", area, "list.ini")
  
  # read previous content of ini
  previous_params <- readIniFile(file = path_clusters_ini)
  
  # check cluster name ----
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
  
  # Remove directories recursively
  subdirs_to_remove <- c("series")
  if (is_thermal) {
    subdirs_to_remove <- c(subdirs_to_remove, "prepro")
  }
  
  dirs_to_remove <- file.path(clustertypePath, subdirs_to_remove, area)
  
  # remove dir "constraints" (st-storage)
  if(opts$antaresVersion>=920)
    dirs_to_remove <- file.path(clustertypePath, 
                                append(subdirs_to_remove, "constraints"), 
                                area)
  
  # if list.ini is empty, area/series area/constraints can be removed
  if (length(previous_params) > 0) {
    dirs_to_remove <- file.path(dirs_to_remove, cluster_name)
  }
  
  lapply(dirs_to_remove, unlink, recursive = TRUE)
  
  # Maj simulation
  suppressWarnings({
    res <- antaresRead::setSimulationPath(path = opts$studyPath, simulation = "input")
  })
  
  invisible(res)
}
