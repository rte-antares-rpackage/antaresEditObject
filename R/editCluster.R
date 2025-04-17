#' @title Edit an existing cluster
#' 
#' @description 
#' `r antaresEditObject:::badge_api_ok()` (thermal clusters only)
#' 
#' Edit parameters, pre-process data and time series of an existing cluster, thermal or RES (renewable energy source).
#'
#'
#' @inheritParams createCluster
#' @template opts
#' 
#' @seealso [createCluster()] or [createClusterRES()] to create new clusters, [removeCluster()] or [removeClusterRES()] to remove clusters.
#' 
#' @note 
#' Parameter `list_pollutants` is only available for Antares studies >= v8.6.0.  
#' 
#' You must provide named `list` (numerical values or NULL ) :    
#' 
#' `list(
#' "nh3"= 0.25, "nox"= 0.45, "pm2_5"= 0.25,  
#' "pm5"= 0.25, "pm10"= 0.25, "nmvoc"= 0.25, "so2"= 0.25, 
#' "op1"= 0.25, "op2"= 0.25, "op3"= 0.25,  
#' "op4"= 0.25, "op5"= NULL, "co2"= NULL)`
#' 
#' @export
#' 
#' @name editCluster
#' 
#' @importFrom antaresRead setSimulationPath
#' @importFrom assertthat assert_that
#' @importFrom utils modifyList write.table
#'
#' @examples
#' \dontrun{
#' 
#' # Update only nominalCapacity for an existing cluster
#' editCluster(
#'   area = "myarea", 
#'   cluster_name = "mycluster", 
#'   nominalcapacity = 10600.000
#' )
#' 
#' }
editCluster <- function(area,
                        cluster_name, 
                        ..., 
                        list_pollutants = NULL,
                        time_series = NULL,
                        prepro_data = NULL,
                        prepro_modulation = NULL,
                        add_prefix = TRUE, 
                        opts = antaresRead::simOptions()) {
  .editCluster(
    area = area,
    cluster_name = cluster_name, 
    ..., 
    list_pollutants = list_pollutants,
    time_series = time_series,
    prepro_data = prepro_data,
    prepro_modulation = prepro_modulation,
    add_prefix = add_prefix, 
    cluster_type = "thermal",
    opts = opts
  )
}

#' @export
#' 
#' @rdname editCluster
editClusterRES <- function(area,
                           cluster_name, 
                           ..., 
                           time_series = NULL,
                           add_prefix = TRUE, 
                           opts = antaresRead::simOptions()) {
  assertthat::assert_that(inherits(opts, "simOptions"))
  check_active_RES(opts, check_dir = TRUE)
  .editCluster(
    area = area,
    cluster_name = cluster_name, 
    ..., 
    time_series = time_series,
    add_prefix = add_prefix, 
    cluster_type = "renewables",
    opts = opts
  )
}

#' @importFrom data.table fwrite as.data.table
.editCluster <- function(area,
                         cluster_name, 
                         ..., 
                         list_pollutants = NULL,
                         time_series = NULL,
                         prepro_data = NULL,
                         prepro_modulation = NULL,
                         add_prefix = TRUE, 
                         cluster_type = c("thermal", "renewables"),
                         opts = antaresRead::simOptions()) {
  # Input path
  inputPath <- opts$inputPath
  cluster_type <- match.arg(cluster_type)
  
  area <- tolower(area)
  
  # Cluster's parameters
  params_cluster <- hyphenize_names(list(...))
  if (add_prefix)
    cluster_name <- paste(area, cluster_name, sep = "_")
  
  lower_cluster_name <- tolower(cluster_name)
  
  # v860 pollutants
  if(opts$antaresVersion >= 860){
    params_cluster <- append(params_cluster, list_pollutants)
  }
  else{
    if(!is.null(list_pollutants))
      stop("antaresVersion should be >= v8.6.0 to use parameter 'list_pollutants'.")
  }
  # Handle case sensitivity in name of clusters API 
  clusters <- names(readIni(file.path("input", cluster_type, "clusters", area, "list"), 
                            opts= opts))
  
  if (!cluster_name %in% clusters){
    if (lower_cluster_name %in% tolower(clusters)){
      cluster_idx <- which(tolower(clusters) %in% lower_cluster_name)
      cluster_name <- clusters[cluster_idx]
      if (length(cluster_name) > 1) 
        warning("detected multiple clusters : ", do.call(paste, as.list(cluster_name)), ", only the first one will be edited.")
      cluster_name <- cluster_name[1]
    } else {
      stop(
        "'", cluster_name, "' doesn't exist, it can't be edited. You can create cluster with createCluster().",
        call. = FALSE
      )
    }
  }
  
  params_cluster$name <- cluster_name
  
  if (!NROW(time_series) %in% c(0, 8736, 8760)) {
    stop("Number of rows for time series must be 0 or 8760")
  }
  
  if (!NROW(prepro_modulation) %in% c(0, 8736, 8760)) {
    stop("Number of rows for modulation data must be 0 or 8760")
  }
  if (!NCOL(prepro_modulation) %in% c(0, 1, 4)) {# issue 115 NCOL NULL return
    stop("Number of cols for modulation data must be 0 or 4")
  }
  
  if (is_api_study(opts)) {
    thermal_type <- identical(cluster_type, "thermal")
    renewables_type <- identical(cluster_type, "renewables")
    
    # update parameters if something else than name
    if (length(params_cluster) > 1) {
      
      if (thermal_type) {
        suffix_endpoint <- "thermal"
      } else if (renewables_type) {
        suffix_endpoint <- "renewable"
      }
      
      body <- transform_list_to_json_for_createCluster(cluster_parameters = params_cluster, cluster_type = cluster_type)
      result <- api_patch(opts = opts, 
                          endpoint = file.path(opts[["study_id"]], "areas", area, "clusters", suffix_endpoint, cluster_name), 
                          body = body, 
                          encode = "raw")
      cli::cli_alert_success("Endpoint Edit {.emph {.strong {suffix_endpoint}}} (properties) {.emph {.strong {cluster_name}}} success"
                            )
    }
    
    cmd <- NULL
    
    if (renewables_type) {
      if (!is.null(time_series)) {
        cmd <- api_command_generate(
            action = "replace_matrix",
            target = sprintf("input/renewables/series/%s/%s/series", area, lower_cluster_name),
            matrix = time_series
        )
      }
    }
    
    if (thermal_type) {
      thermal_time_series <- list("prepro_data" = list("path" = "input/thermal/prepro/%s/%s/data",
                                                       "matrix" = prepro_data
                                                       ),
                                  "prepro_modulation" = list("path" = "input/thermal/prepro/%s/%s/modulation",
                                                             "matrix" = prepro_modulation
                                                       ),
                                  "thermal_availabilities" = list("path" = "input/thermal/series/%s/%s/series",
                                                                  "matrix" = time_series)
                                  )
      not_null_matrix <- sapply(thermal_time_series, FUN = function(l) {!is.null(l[["matrix"]])})
      thermal_time_series <- thermal_time_series[not_null_matrix]
      
      if (length(thermal_time_series) > 0) {
        actions <- lapply(
          X = seq_along(thermal_time_series),
          FUN = function(i) {
            list(
              target = sprintf(thermal_time_series[[i]][["path"]], area, lower_cluster_name),
              matrix = thermal_time_series[[i]][["matrix"]]
            )
          }
        )
        actions <- setNames(actions, rep("replace_matrix", length(actions)))
        cmd <- do.call(api_commands_generate, actions)
      }
    }
    
    if (!is.null(cmd)) {
      api_command_register(cmd, opts = opts)
      `if`(
        should_command_be_executed(opts), 
        api_command_execute(cmd, opts = opts, text_alert = "Updating cluster's series: {msg_api}"),
        cli_command_registered("replace_matrix")
      )
    }
    
    return(invisible(opts))
  }
  
  assertthat::assert_that(!is.null(inputPath) && file.exists(inputPath))
  check_area_name(area, opts)
  
  
  # path to ini file
  path_clusters_ini <- file.path(inputPath, cluster_type, "clusters", area, "list.ini")
  if (!file.exists(path_clusters_ini))
    stop("'", cluster_name, "' in area '", area, "' doesn't seems to exist.")
  
  # read previous content of ini
  previous_params <- readIniFile(file = path_clusters_ini)
  
  if (!lower_cluster_name %in% tolower(names(previous_params))){
    stop(
      "'", cluster_name, "' doesn't exist, it can't be edited. You can create cluster with createCluster().",
      call. = FALSE
    )
  }
  
  ind_cluster <- which(tolower(names(previous_params)) %in% tolower(cluster_name))[1]
  previous_params[[ind_cluster]] <- utils::modifyList(x = previous_params[[ind_cluster]], val = params_cluster)
  names(previous_params)[[ind_cluster]] <- cluster_name
  
  # write modified ini file
  writeIni(
    listData = previous_params,
    pathIni = path_clusters_ini,
    overwrite = TRUE
  )
  
  # datas associated with cluster
  
  if (!is.null(time_series)) {
    fwrite(
      x = as.data.table(time_series), row.names = FALSE, col.names = FALSE, sep = "\t",
      file = file.path(inputPath, cluster_type, "series", area, lower_cluster_name, "series.txt")
    )
  }
  
  if (!is.null(prepro_data)) {
    fwrite(
      x = as.data.table(prepro_data), row.names = FALSE, col.names = FALSE, sep = "\t",
      file = file.path(inputPath, cluster_type, "prepro", area, lower_cluster_name, "data.txt")
    )
  }
  
  if (!is.null(prepro_modulation)) {
    fwrite(
      x = as.data.table(prepro_modulation), row.names = FALSE, col.names = FALSE, sep = "\t",
      file = file.path(inputPath, cluster_type, "prepro", area, lower_cluster_name, "modulation.txt")
    )
  }
  
  # Maj simulation
  suppressWarnings({
    res <- antaresRead::setSimulationPath(path = opts$studyPath, simulation = "input")
  })
  
  invisible(res)
}
