#' @title Create a short-term storage cluster 
#' 
#' @description 
#' `r antaresEditObject:::badge_api_ok()`
#' 
#' Create a new ST-storage cluster for >= v8.6.0 Antares studies.
#'
#' @param area The area where to create the cluster.
#' @param cluster_name Name for the cluster, it will prefixed by area name, unless you set `add_prefix = FALSE`.
#' @param group Group of the cluster, one of : "PSP_open", "PSP_closed", "Pondage", "Battery", "Other". It corresponds to the type of stockage.
#' @param storage_parameters `list ` Parameters to write in the Ini file (see `Note`). 
#' @param PMAX_injection modulation of charging capacity on an 8760-hour basis. The values are float between 0 and 1.
#' @param PMAX_withdrawal modulation of discharging capacity on an 8760-hour basis. The values are float between 0 and 1.
#' @param inflows imposed withdrawals from the stock for other uses, The values are integer.
#' @param lower_rule_curve This is the lower limit for filling the stock imposed each hour. The values are float between 0 and 1.
#' @param upper_rule_curve This is the upper limit for filling the stock imposed each hour. The values are float between 0 and 1.
#' @param add_prefix If `TRUE` (the default), `cluster_name` will be prefixed by area name.
#' @param overwrite Logical, overwrite the cluster or not.
#' 
#' @template opts
#' @note   
#' To write parameters to the `list.ini` file. You have function `storage_values_default()` who is called by default.
#' This function return `list` containing six parameters for cluster `st-storage`.
#' See example section.
#' 
#' To write data (.txt file), you have parameter for each output file :   
#'  - PMAX-injection.txt 
#'  - PMAX-withdrawal.txt
#'  - inflows.txt
#'  - lower-rule-curve.txt
#'  - upper-rule-curve.txt
#'  
#' @seealso [editClusterST()] to edit existing clusters, [readClusterSTDesc()] to read cluster,
#' [removeClusterST()] to remove clusters.
#' 
#' @export
#' 
#' @importFrom antaresRead simOptions
#' @importFrom stats setNames
#' @importFrom utils read.table write.table
#' @importFrom data.table setcolorder year yday month setnames fwrite
#'
#' @examples
#' \dontrun{
#' 
#' # list for cluster parameters : 
#' storage_values_default()
#' 
#' # create a cluster by default (with default parameters values + default data values):
#' createClusterST(area = "my_area", 
#'                "my_cluster") 
#'   
#' # Read cluster in study                            
#'  # by default, cluster name is prefixed 
#'  # by the area name
#' levels(readClusterSTDesc()$cluster)
#' # > "my_area_my_cluster"
#' 
#' # create cluster with custom parameter and data
#' my_parameters <- storage_values_default()
#' my_parameters$efficiency <- 0.5
#' my_parameters$reservoircapacity <- 10000
#' 
#' 
#' inflow_data <- matrix(3, 8760)
#' ratio_data <- matrix(0.7, 8760)
#' createClusterST(area = "my_area", 
#'                 "my_cluster",
#'                 storage_parameters = my_parameters,
#'                 PMAX_withdrawal = ratio_data, 
#'                 inflows = inflow_data, 
#'                 PMAX_injection = ratio_data, 
#'                 lower_rule_curve = ratio_data, 
#'                 upper_rule_curve = ratio_data)
#' }
#'
createClusterST <- function(area,
                            cluster_name, 
                            group = "Other1",
                            storage_parameters = storage_values_default(),
                            PMAX_injection = NULL,
                            PMAX_withdrawal = NULL,
                            inflows = NULL,
                            lower_rule_curve = NULL,
                            upper_rule_curve = NULL,
                            add_prefix = TRUE, 
                            overwrite = FALSE,
                            opts = antaresRead::simOptions()) {

  # check study parameters
  assertthat::assert_that(inherits(opts, "simOptions"))
  
  # check study version
  check_active_ST(opts = opts)
  
  # statics groups
  st_storage_group <- c("PSP_open", 
                        "PSP_closed", 
                        "Pondage", 
                        "Battery",
                        paste0("Other", 
                               seq(1,5)))
  
  # check group
  if (!is.null(group) && !tolower(group) %in% tolower(st_storage_group))
    warning(
      "Group: '", group, "' is not a valid name recognized by Antares,",
      " you should be using one of: ", paste(st_storage_group, collapse = ", ")
    )
  
  # check area exsiting in current study
  check_area_name(area, opts)  
  area <- tolower(area)
  
  ##
  # check parameters (ini file)
  ##
  assertthat::assert_that(inherits(storage_parameters, "list"))
  
  # static name of list parameters 
  names_parameters <- names(storage_values_default(opts = opts))
  
  if(!all(names(storage_parameters) %in% names_parameters))
    stop(append("Parameter 'st-storage' must be named with the following elements: ", 
                paste0(names_parameters, collapse= ", ")))
  
  # check values parameters
  .st_mandatory_params(list_values = storage_parameters, opts = opts)
  
  
  # DATA parameters : default value + name txt file
  storage_value <- list(PMAX_injection = list(N=1, string = "PMAX-injection"),
                        PMAX_withdrawal = list(N=1, string = "PMAX-withdrawal"),
                        inflows = list(N=0, string = "inflows"),
                        lower_rule_curve = list(N=0, string = "lower-rule-curve"),
                        upper_rule_curve = list(N=1, string = "upper-rule-curve"))
  
  # check data 
  for (name in names(storage_value)){
    if (!(is.null(dim(get(name))) || (identical(dim(get(name)), c(8760L, 1L))))){
      stop(paste0("Input data for ", name, " must be 8760*1"))
    } 
  }
  
  # check syntax ini parameters
  params_cluster <- hyphenize_names(storage_parameters)
  if (add_prefix)
    cluster_name <- paste(area, cluster_name, sep = "_")
  params_cluster <- c(list(name = cluster_name, group = group),
                      params_cluster)
  
  ################# -
  # API block
  if (is_api_study(opts)) {
    # format name for API 
    cluster_name <- transform_name_to_id(cluster_name)
    
    # /!\ temporary solution /!\ 
      # as the endpoint does not return an error if the cluster already exist 
    if(!is_api_mocked(opts)){
      exists <- FALSE
      clusters <- readClusterSTDesc(opts = opts)
      if (nrow(clusters) > 0) {
        clusters_filtered <- clusters[clusters$area == tolower(area) & 
                                        clusters$cluster == cluster_name,]
        exists <- nrow(clusters_filtered) > 0
      }
      if(exists) 
        stop("Cluster already exists. Edit it with editClusterST().")
    }
    
    params_cluster$name <- cluster_name
    
    cmd <- api_command_generate(
      action = "create_st_storage",
      area_id = area,
      parameters = params_cluster
    )
    
    api_command_register(cmd, opts = opts)
    `if`(
      should_command_be_executed(opts),
      api_command_execute(cmd, opts = opts, text_alert = "{.emph create_st_storage}: {msg_api}"),
      cli_command_registered("create_st_storage")
    )
    
    for (i in names(storage_value)){
      if (!is.null(get(i))) {
        # format name for API 
        data_param_name <- transform_name_to_id(storage_value[[i]]$string, 
                                                id_dash = TRUE)
        
        currPath <- paste0("input/st-storage/series/%s/%s/",data_param_name)
        cmd <- api_command_generate(
          action = "replace_matrix",
          target = sprintf(currPath, area, cluster_name),
          matrix = get(i)
        )
        api_command_register(cmd, opts = opts)
        `if`(
          should_command_be_executed(opts),
          api_command_execute(cmd, 
                              opts = opts, 
                              text_alert = paste0("Writing ", 
                                                  i, 
                                                  " cluster's series: {msg_api}")),
          cli_command_registered("replace_matrix")
        )
      }
    }
    
    return(invisible(opts))
  }
  ########################## -
  
  
  ##
  # parameters traitements
  ##
  
  inputPath <- opts$inputPath
  assertthat::assert_that(!is.null(inputPath) && file.exists(inputPath))
  
  # path to ini file containing clusters' name and parameters
  path_clusters_ini <- file.path(inputPath, "st-storage", "clusters", tolower(area), "list.ini")
  
  # read previous content of ini
  previous_params <- readIniFile(file = path_clusters_ini)
  
  # already exists ? 
  if (tolower(cluster_name) %in% tolower(names(previous_params)) 
      & !overwrite)
    stop(paste(cluster_name, "already exist"))
    
  # overwrite
  if(overwrite){
    if(tolower(cluster_name) %in% tolower(names(previous_params))){
      ind_cluster <- which(tolower(names(previous_params)) %in% 
                             tolower(cluster_name))[1]
      previous_params[[ind_cluster]] <- params_cluster
      names(previous_params)[[ind_cluster]] <- cluster_name
    }
  }
    
  # add properties 
  previous_params[[cluster_name]] <- params_cluster
  
  # write properties (all properties are overwritten)
  writeIni(
    listData = previous_params,
    pathIni = path_clusters_ini,
    overwrite = TRUE
  )
  
  # initialize series
  dir.create(
    path = file.path(inputPath, "st-storage", "series", tolower(area), tolower(cluster_name)),
    recursive = TRUE, showWarnings = FALSE
  )
  
  for (name in names(storage_value)){
    if (is.null(get(name))) {
      k <- matrix(storage_value[[name]]$N,8760)
      
      fwrite(
        x = k, row.names = FALSE, col.names = FALSE, sep = "\t",
        file = file.path(inputPath, "st-storage", "series", tolower(area), tolower(cluster_name), paste0(storage_value[[name]]$string, ".txt"))
      )
    } else {
      # write data 
      fwrite(
        x = get(name), row.names = FALSE, col.names = FALSE, sep = "\t",
        file = file.path(inputPath, "st-storage", "series", tolower(area), 
                         tolower(cluster_name), 
                         paste0(storage_value[[name]]$string, ".txt"))
      )
    }
  }
  
  # Update simulation options object
  suppressWarnings({
    res <- antaresRead::setSimulationPath(path = opts$studyPath, simulation = "input")
  })
  
  invisible(res)
  
}


# check parameters (`list`)
.st_mandatory_params <- function(list_values, opts){
  .is_ratio(list_values[["efficiency"]], 
            "efficiency")
  
  .check_capacity(list_values[["reservoircapacity"]], 
                  "reservoircapacity")
  
  .is_ratio(list_values[["initiallevel"]], 
            "initiallevel")
  
  .check_capacity(list_values[["withdrawalnominalcapacity"]], 
                  "withdrawalnominalcapacity")
  
  .check_capacity(list_values[["injectionnominalcapacity"]], 
                  "injectionnominalcapacity")
  
  if(!is.null(list_values[["initialleveloptim"]]))
    assertthat::assert_that(inherits(list_values[["initialleveloptim"]], 
                                     "logical"))
  
  if (opts$antaresVersion >= 880)
    if(!is.null(list_values[["enabled"]]))
      assertthat::assert_that(inherits(list_values[["enabled"]], 
                                       "logical"))
}

.is_ratio <- function(x, mess){
  if(!is.null(x)){
    assertthat::assert_that(inherits(x, "numeric"))
    if(!(x>=0 && x<=1))
      stop(paste0(mess, " must be in range 0-1"), 
           call. = FALSE)
  }
}

.check_capacity <- function(x, mess){
  if(!is.null(x)){
    assertthat::assert_that(inherits(x, "numeric"))
    if(!(x>=0))
      stop(paste0(mess, " must be >= 0"), 
           call. = FALSE)
  }
}

#' Short Term Storage Property List
#'
#' @description
#' Default values are returned according to study version
#'
#' @template opts
#' @return a named list
#' @export
#'
#' @examples
#' \dontrun{
#' storage_values_default()
#' }
storage_values_default <- function(opts = simOptions()) {
  lst_parameters <- list(efficiency = 1,
                         reservoircapacity = 0,
                         initiallevel = 0,
                         withdrawalnominalcapacity = 0,
                         injectionnominalcapacity = 0,
                         initialleveloptim = FALSE)
  
  if (opts$antaresVersion >= 880){
    lst_parameters$initiallevel <- 0.5
    lst_parameters$enabled <- TRUE
  }
  
  return(lst_parameters)
}
