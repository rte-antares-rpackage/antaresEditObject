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
  if (!api_study | (api_study && !is_api_mocked(opts))) {
    check_cluster_in_binding_constraint(area = area, cluster_name = cluster_name, add_prefix = add_prefix, opts = opts)
  }
  
  if (add_prefix)
    cluster_name <- paste(area, cluster_name, sep = "_")
  
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
  inputPath <- opts$inputPath
  clustertypePath <- file.path(inputPath, cluster_type)
  
  # Remove from Ini file
  # path to ini file
  path_clusters_ini <- file.path(clustertypePath, "clusters", area, "list.ini")
  
  # read previous content of ini
  previous_params <- readIniFile(file = path_clusters_ini)
  
  # cluster indice
  lower_cluster_name <- tolower(cluster_name)
  idx <- which(tolower(names(previous_params)) %in% lower_cluster_name)
  if (length(idx) < 1)
    warning("Cluster '", lower_cluster_name, "' you want to remove doesn't seem to exist in area '", area, "'.")
  
  # Remove entry in list.ini
  previous_params[idx] <- NULL
  
  writeIni(
    listData = previous_params,
    pathIni = path_clusters_ini,
    overwrite = TRUE
  )
  
  # Remove series
  if (length(previous_params) > 0) {
    dirs_to_remove <- file.path(clustertypePath, "series", area, lower_cluster_name)
  } else {
    dirs_to_remove <- file.path(clustertypePath, "series", area)
  }
  
  # Remove prepro
  if (identical(cluster_type, "thermal")) {
    dirs_to_remove <- c(dirs_to_remove, file.path(clustertypePath, "prepro", area))
  }
  lapply(dirs_to_remove, unlink, recursive = TRUE)
  # Maj simulation
  suppressWarnings({
    res <- antaresRead::setSimulationPath(path = opts$studyPath, simulation = "input")
  })
  
  invisible(res)
}


#' @title Check if a cluster is referenced in a binding constraint as a coefficient.
#'
#' @param area The area where to create the cluster.
#' @param cluster_name Name for the cluster, it will prefixed by area name, unless you set `add_prefix = FALSE`.
#' @param add_prefix If `TRUE` (the default), `cluster_name` will be prefixed by area name.
#'  
#' @template opts
#'
#' @importFrom antaresRead readBindingConstraints
check_cluster_in_binding_constraint <- function(area, cluster_name, add_prefix, opts = antaresRead::simOptions()) {
  
  bc <- readBindingConstraints(opts = opts)
  if (length(bc) > 0) {
    area <- tolower(area)
    cluster_name <- tolower(cluster_name)
    if (add_prefix) {
      cluster_name <- paste(area, cluster_name, sep = "_")
    }
    cluster_name_pattern <- paste(area, cluster_name, sep = ".")
    bc_coefs <- lapply(bc, "[[", "coefs")
    names_bc_coefs <- lapply(bc_coefs, names)
    cluster_in_names_bc_coefs <- lapply(names_bc_coefs, FUN = function(coef_name){cluster_name_pattern %in% coef_name})
    bc_not_remove <- cluster_in_names_bc_coefs[which(cluster_in_names_bc_coefs == TRUE)]
    
    bc_not_remove <- names(bc_not_remove)
    if(length(bc_not_remove) > 0) {
      message("The following binding constraints have the cluster_name to remove as a coefficient : ", paste0(bc_not_remove, collapse = ","))
      stop("Can not remove the cluster ", cluster_name, " in the area ", area)
    }
  }
}
