#' @title Create a cluster
#' 
#' @description 
#' `r antaresEditObject:::badge_api_ok()` (thermal clusters only)
#' 
#' Create a new thermal or RES (renewable energy source) cluster.
#'
#' @param area The area where to create the cluster.
#' @param cluster_name Name for the cluster, it will prefixed by area name, unless you set `add_prefix = FALSE`.
#' @param group Group of the cluster, depends on cluster type:
#'  - thermal cluster, one of: Gas, Hard coal, Lignite, Mixed fuel, Nuclear, Oil, Other, Other 2, Other 3, Other 4.
#'  - renewable cluster, one of: Wind Onshore, Wind Offshore, Solar Thermal, Solar PV, Solar Rooftop, Other RES 1, Other RES 2, Other RES 3, Other RES 4.
#' @param ... Parameters to write in the Ini file. Careful!
#'  Some parameters must be set as `integers` to avoid warnings in Antares, for example, 
#'  to set `unitcount`, you'll have to use `unitcount = 1L`.
#' @param list_pollutants `list` named with specific pollutants (only for Antares version >= 860)  
#' @param time_series the "ready-made" 8760-hour time-series available for simulation purposes.
#' @param prepro_data Pre-process data, a `data.frame` or `matrix`, 
#'  default is a matrix with 365 rows and 6 columns.
#' @param prepro_modulation Pre-process modulation, a `data.frame` or `matrix`, 
#'  if specified, must have 8760 rows and 1 or 4 columns.
#' @param add_prefix If `TRUE` (the default), `cluster_name` will be prefixed by area name.
#' @param overwrite Logical, overwrite the cluster or not.
#' 
#' @template opts
#' 
#' @seealso [editCluster()] or [editClusterRES()] to edit existing clusters, [removeCluster()] or [removeClusterRES()] to remove clusters.
#' 
#' @export
#' 
#' @name createCluster
#' 
#' @importFrom antaresRead simOptions
#' @importFrom stats setNames
#' @importFrom utils read.table write.table
#' @importFrom data.table setcolorder year yday month setnames
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
#'   unitcount = 1L, # or as.integer(1)
#'   marginal_cost = 50
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
#'   marginal_cost = 50
#' )
#' levels(readClusterDesc()$cluster)
#' # > "my_cluster"
#' 
#' 
#' # Create a RES cluster :
#' createClusterRES(
#'   area = "fr", 
#'   cluster_name = "my_cluster_res",
#'   group = "other", 
#'   unitcount = 1L, # or as.integer(1)
#'   nominalcapacity = 50,
#'   ts_interpretation = "power-generation"
#' ) 
#' 
#' # You can also specify that the Time-Series of the RES cluster are
#' # production factors :
#' createClusterRES(
#'   area = "fr", 
#'   cluster_name = "my_cluster_res",
#'   group = "other", 
#'   unitcount = 1L, # or as.integer(1)
#'   nominalcapacity = 50,
#'   ts_interpretation = "production-factor"
#' )
#' 
#' 
#' # Pre-process data : 
#' 
#' # this is the default data :
#' createCluster(
#'   area = "fr", 
#'   cluster_name = "my_cluster",
#'   prepro_data = matrix(
#'     data = c(rep(1, times = 365 * 2),
#'              rep(0, times = 365 * 4)), 
#'     ncol = 6
#'   )
#' )
#' 
#' # with a data.frame
#' createCluster(
#'   area = "fr", 
#'   cluster_name = "my_cluster",
#'   prepro_data = data.frame(
#'     v1 = rep(7, 365), # column name does not matter
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
#'   prepro_modulation = matrix(
#'     data = c(rep(1, times = 365 * 24 * 3),
#'              rep(0, times = 365 * 24 * 1)),
#'     ncol = 4
#'   )
#' )
#' 
#' # with a data.frame
#' createCluster(
#'   area = "fr", 
#'   cluster_name = "my_cluster",
#'   prepro_modulation = data.frame(
#'     var1 = rep(0, 8760), # column name does not matter
#'     var2 = rep(1, 8760),
#'     var3 = rep(0, 8760),
#'     var4 = rep(1, 8760)
#'   )
#' )
#' 
#' }
createCluster <- function(area, 
                          cluster_name, 
                          group = "Other",
                          ...,
                          list_pollutants = NULL,
                          time_series = NULL,
                          prepro_data = NULL,
                          prepro_modulation = NULL,
                          add_prefix = TRUE, 
                          overwrite = FALSE,
                          opts = antaresRead::simOptions()) {
  # check study parameters
  assertthat::assert_that(inherits(opts, "simOptions"))
  
  # static name of list parameters of pollutants
  name_list_param_poll <- names(list_pollutants_values())
  
  # check v860
  # check list pollutants parameters
  if(opts$antaresVersion >= 860){
    if(!is.null(list_pollutants) & 
       !(inherits(x = list_pollutants, 
                  what = "list")))
      stop("Parameter 'list_pollutants' must be a 'list'")
    
    if(!all(names(list_pollutants) %in% name_list_param_poll))
      stop(append("Parameter 'list_pollutants' must be named with the following elements: ", 
                  paste0(name_list_param_poll, collapse= ", ")))
  }
  else{
    if(!is.null(list_pollutants))
      stop("antaresVersion should be >= v8.6.0 to use parameter 'list_pollutants'.")
  }
  
  
  # statics groups
  thermal_group <- c("Gas",
                     "Hard coal",
                     "Lignite",
                     "Mixed fuel",
                     "Nuclear",
                     "Oil",
                     "Other",
                     "Other 2",
                     "Other 3",
                     "Other 4")
  
  if (!is.null(group) && !tolower(group) %in% tolower(thermal_group))
    warning(
      "Group: '", group, "' is not a valid name recognized by Antares,",
      " you should be using one of: ", paste(thermal_group, collapse = ", ")
    )
  
  .createCluster(
    area = area, 
    cluster_name = cluster_name, 
    group = group,
    ...,
    list_pollutants = list_pollutants,
    time_series = time_series,
    prepro_data = prepro_data,
    prepro_modulation = prepro_modulation,
    add_prefix = add_prefix, 
    overwrite = overwrite,
    cluster_type = "thermal",
    opts = opts
  )
}

#' @export
#' 
#' @rdname createCluster
createClusterRES <- function(area, 
                             cluster_name, 
                             group = "Other RES 1",
                             ...,
                             time_series = NULL,
                             add_prefix = TRUE, 
                             overwrite = FALSE,
                             opts = antaresRead::simOptions()) {
  assertthat::assert_that(inherits(opts, "simOptions"))
  check_active_RES(opts)
  renewables_group <- c("Wind Onshore",
                        "Wind Offshore",
                        "Solar Thermal",
                        "Solar PV",
                        "Solar Rooftop",
                        "Other RES 1",
                        "Other RES 2",
                        "Other RES 3",
                        "Other RES 4")
  if (!is.null(group) && !tolower(group) %in% tolower(renewables_group))
    warning(
      "Group: '", group, "' is not a valid name recognized by Antares,",
      " you should be using one of: ", paste(renewables_group, collapse = ", ")
    )
  initialize_RES(opts)
  .createCluster(
    area = area, 
    cluster_name = cluster_name,
    group = group,
    ...,
    time_series = time_series,
    prepro_data = NULL,
    prepro_modulation = NULL,
    add_prefix = add_prefix, 
    overwrite = overwrite,
    cluster_type = "renewables",
    opts = opts
  )
}

#' @importFrom data.table fwrite as.data.table
.createCluster <- function(area, 
                           cluster_name, 
                           ...,
                           list_pollutants = NULL,
                           time_series = NULL,
                           prepro_data = NULL,
                           prepro_modulation = NULL,
                           add_prefix = TRUE, 
                           overwrite = FALSE,
                           cluster_type = c("thermal", "renewables"),
                           opts = antaresRead::simOptions()) {
  
  # Input path
  inputPath <- opts$inputPath
  cluster_type <- match.arg(cluster_type)
  
  if (!NROW(time_series) %in% c(0, 8736, 8760)) {
    stop("Number of rows for time series must be 0 or 8760")
  }
  
  if (!NROW(prepro_modulation) %in% c(0, 8736, 8760)) {
    stop("Number of rows for modulation data must be 0 or 8760")
  }
  if (!NCOL(prepro_modulation) %in% c(0, 1, 4)) { # issue 115 NCOL NULL return
    stop("Number of cols for modulation data must be 0 or 4")
  }
  
  area <- tolower(area)
  
  # Cluster's parameters
  params_cluster <- hyphenize_names(list(...))
  if (add_prefix)
    cluster_name <- paste(area, cluster_name, sep = "_")
  params_cluster$name <- cluster_name
  lower_cluster_name <- tolower(cluster_name)
  
  # v860 pollutants
  if(opts$antaresVersion >= 860)
    params_cluster <- append(params_cluster, list_pollutants)
  
  # API block
  if (is_api_study(opts)) {
    
    if (cluster_type == "thermal") {
      cmd <- api_command_generate(
        action = "create_cluster",
        area_id = area,
        cluster_name = cluster_name,
        prepro = prepro_data,
        modulation = prepro_modulation,
        parameters = params_cluster
      )
    } else {
      cmd <- api_command_generate(
        action = "create_renewables_cluster",
        area_id = area,
        cluster_name = cluster_name,
        parameters = params_cluster
      )
    }
    
    api_command_register(cmd, opts = opts)
    `if`(
      should_command_be_executed(opts), 
      api_command_execute(cmd, opts = opts, text_alert = "{.emph create_cluster}: {msg_api}"),
      cli_command_registered("create_cluster")
    )
    
    if (!is.null(time_series)) {
      currPath <- ifelse(identical(cluster_type, "renewables"), "input/renewables/series/%s/%s/series", "input/thermal/series/%s/%s/series")
      cmd <- api_command_generate(
        action = "replace_matrix",
        target = sprintf(currPath, area, lower_cluster_name),
        matrix = time_series
      )
      api_command_register(cmd, opts = opts)
      `if`(
        should_command_be_executed(opts), 
        api_command_execute(cmd, opts = opts, text_alert = "Writing cluster's series: {msg_api}"),
        cli_command_registered("replace_matrix")
      )
    }
    
    return(invisible(opts))
  }
  
  assertthat::assert_that(!is.null(inputPath) && file.exists(inputPath))
  check_area_name(area, opts)
  
  # named list for writing ini file
  # params_cluster <- stats::setNames(object = list(params_cluster), nm = cluster_name)
  
  # path to ini file containing clusters' name and parameters
  path_clusters_ini <- file.path(inputPath, cluster_type, "clusters", area, "list.ini")
  
  # read previous content of ini
  previous_params <- readIniFile(file = path_clusters_ini)
  
  if (lower_cluster_name %in% tolower(names(previous_params)) & !overwrite){
    stop(paste(cluster_name, "already exist"))
  } else if (lower_cluster_name %in% tolower(names(previous_params)) & overwrite){
    ind_cluster <- which(tolower(names(previous_params)) %in% lower_cluster_name)[1]
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
  dir.create(
    path = file.path(inputPath, cluster_type, "series", area, lower_cluster_name),
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
  
  fwrite(
    x = as.data.table(time_series), row.names = FALSE, col.names = FALSE, sep = "\t",
    file = file.path(inputPath, cluster_type, "series", area, lower_cluster_name, "series.txt")
  )
  
  
  # prepro
  if (identical(cluster_type, "thermal")) {
    dir.create(
      path = file.path(inputPath, cluster_type, "prepro", area, lower_cluster_name),
      recursive = TRUE, showWarnings = FALSE
    )
    
    if (is.null(prepro_data))
      prepro_data <- matrix(data = c(rep(1, times = 365 * 2), rep(0, times = 365 * 4)), ncol = 6)
    fwrite(
      x = as.data.table(prepro_data), row.names = FALSE, col.names = FALSE, sep = "\t",
      file = file.path(inputPath, cluster_type, "prepro", area, lower_cluster_name, "data.txt")
    )
    
    
    if (is.null(prepro_modulation))
      prepro_modulation <- matrix(data = c(rep(1, times = 365 * 24 * 3), rep(0, times = 365 * 24 * 1)), ncol = 4)
    
    fwrite(
      x = as.data.table(prepro_modulation), row.names = FALSE, col.names = FALSE, sep = "\t",
      file = file.path(inputPath, cluster_type, "prepro", area, lower_cluster_name, "modulation.txt")
    )
  }
  
  
  # Update simulation options object
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


#' Output pollutants list for thermal clusters
#'
#' @param multi_values put values to init list values, default as `NULL`
#'
#' @return a named list
#' @export
#'
#' @examples
#' list_pollutants_values()
list_pollutants_values <- function(multi_values = NULL) {
  list("nh3"= multi_values, 
       "nox"= multi_values, 
       "pm2_5"= multi_values, 
       "pm5"= multi_values, 
       "pm10"= multi_values, 
       "nmvoc"= multi_values, 
       "so2"= multi_values, 
       "op1"= multi_values, 
       "op2"= multi_values, 
       "op3"= multi_values, 
       "op4"= multi_values, 
       "op5"= multi_values, 
       "co2"= multi_values)
}
