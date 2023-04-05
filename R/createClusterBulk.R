
#' @title Create serial thermal cluster
#' @description For each area, the thermal cluster data are generated :  
#'  - Writing `.ini` files  
#'  - Writing time_series files  
#'  - Writing prepro_data files  
#'  - Writing prepro_modulation files
#' @param cluster_object \code{list} mutiple list containing the parameters for writing each cluster 
#' @param add_prefix \code{logical} prefix cluster name with area name
#' @param area_zone \code{character} name of area to create cluster
#' 
#' @template opts
#' 
#' @details see the example to write a cluster object, 
#' see the original function [createCluster()]  
#' 
#' Structure of `cluster_object` :  
#'   
#'  The list must be structured with named items 
#'  \itemize{
#'  \item \code{parameter} : `list` of paramaters to write in .ini file
#'  \item \code{overwrite} : `logical` to choose to overwrite an existing cluster (if not present, set to `FALSE`)
#'  \item \code{time_series} : `matrix` or `data.frame` the "ready-made" 8760-hour time-series 
#'  \item \code{prepro_data} : `matrix` or `data.frame` Pre-process data
#'  \item \code{prepro_modulation} : `matrix` or `data.frame` Pre-process modulation
#'  }
#'      
#'  Details for sublist `cluster_object[["parameter"]]` :      
#' \itemize{
#' \item \code{name} : Name for the cluster, 
#' it will prefixed by area name, unless you set add_prefix = FALSE
#' \item \code{group} : Group of the cluster, depends on cluster type
#' \item \code{...} : Parameters to write in the Ini file
#' }
#' 
#' @return \code{list} containing meta information about the simulation
#' @export
#' @examples
#' \dontrun{
#' 
#' # /!\/!\/!\ use or create a study /!\/!\/!\
#' 
#' # data preparation for sutructures
#' ts <- matrix(rep(c(0, 8000), each = 24*364), 
#'              ncol = 2)
#' 
#' df_pd <- matrix(rep(c(1, 1, 1, 0), each = 24*365), 
#'                 ncol = 4)
#' 
#' df_pm <- matrix(data = c(rep(1, times = 365 * 24 * 3), rep(0, times = 365 * 24 * 1)), 
#'                 ncol = 4)
#'
#' 
#' # Example cluster object
#' zone_test_1 <- list(
#'   `CCGT old 1`= list(
#'   parameter= list(
#'   name= "CCGT old 1",
#'   group = "Other",
#'   unitcount= 10L,
#'   nominalcapacity= 100,
#'    enabled= "true",
#'    `min-stable-power`= 80L,
#'    `min-up-time`= 20L,
#'    `min-down_time`= 30L),
#'    overwrite= TRUE,
#'    time_series = ts_8760,
#'    prepro_data = df_pd,
#'    prepro_modulation = df_pm))
#'  
#'  # overwrite existing cluster
#'zone_test_2 <- list(
#'  `PEAK`= list(parameter= list(
#'    name= "PEAK",
#'    group = "Other"),
#'    overwrite= TRUE,
#'    time_series = ts,
#'    prepro_data = df_pd,
#'    prepro_modulation = df_pm))
#' 
#' # Create multiple areas with multiple clusters
#' list_areas <- antaresRead::getAreas()[1:5]
#' 
#' lapply(list_areas, createClusterBulk,
#' cluster_object = c(zone_test_1, zone_test_2),
#' add_prefix = TRUE)
#' 
#' }
#' 
createClusterBulk <- function(cluster_object,
                              add_prefix= TRUE,
                              area_zone,
                              opts = antaresRead::simOptions()){
  
  # checks parameters
  assertthat::assert_that(inherits(opts, "simOptions"))
  assertthat::assert_that(inherits(cluster_object, "list"))
  assertthat::assert_that(!is.null(opts$inputPath) && 
                            file.exists(opts$inputPath))
  check_area_name(area_zone, opts)
  
  if(add_prefix)
    names(cluster_object) <- paste0(area_zone, "_", names(cluster_object))
  
  # Ini file path
  pathIni <- file.path(opts$inputPath, "thermal", "clusters", area_zone, "list.ini")
  
  # check existing cluster names
  existing_cluster <- readIniFile(pathIni, stringsAsFactors = FALSE)
  
  # for each cluster "object"
    # writing thermal files
  list_full_cluster <- lapply(cluster_object, .createClusterBulk,
         area_zone= area_zone,
         add_prefix= add_prefix, 
         existing_params= existing_cluster, 
         opts_study = opts)
  
  # add existing cluster + only rewritten cluster
  updated_names <- setdiff(names(existing_cluster), names(list_full_cluster))
  
  final_list <- c(existing_cluster[updated_names], list_full_cluster)
  final_list <- final_list[order(names(final_list))]
  
  # check names parameters
  final_list <- lapply(final_list, hyphenize_names)
  
  # writing 
  writeIni(listData = final_list, pathIni = pathIni, overwrite = TRUE)
  
  # Update simulation options object
  suppressWarnings({
    res <- antaresRead::setSimulationPath(path = opts$studyPath, simulation = "input")
  })
  invisible(res)
}


#' @param ... \code{list} named `list` of cluster parameter
#' @param add_prefix \code{logical} prefix cluster name with area name
#' @param area_zone \code{character} name of area to create cluster
#' @param existing_params \code{list} existing cluster's parameters of study
#' @template opts
#' @importFrom data.table fwrite
.createClusterBulk <- function(...,
                               area_zone,
                               add_prefix,
                               existing_params,
                               opts = antaresRead::simOptions()){
  
  # re-adjustment of list parameters
  list_params = list(...)[[1]]
  
  if(!"overwrite" %in% names(list_params)){
    list_params$overwrite <- FALSE
  }
  
  # check parameters required to list.ini file
  if(!"name" %in% names(list_params$parameter) & 
                       "group" %in% names(list_params$parameter))
    stop("Please enter required parameters 'names' and 'group' in [['parameter']]")
  
  # check names cluster
  if (list_params$parameter$name %in% names(existing_params) & !list_params$overwrite){
    stop(paste("cluster : ", list_params$parameter$name, "already exist"))
  }
    
  
  # check time series values
  if (!NROW(list_params$time_series) %in% c(0, 8736, 8760)) {
    stop("Number of rows for time series must be 0 or 8760")
  }
  
  if (!NROW(list_params$prepro_modulation) %in% c(0, 8736, 8760)) {
    stop("Number of rows for modulation data must be 0 or 8760")
  }
  if (!NCOL(list_params$prepro_modulation) %in% c(1, 4)) {
    stop("Number of cols for modulation data must be 0 or 4")
  }
  
  
  # add prefix
  if (add_prefix)
    list_params$parameter$name <- paste(area_zone, 
                          list_params$parameter$name, 
                          sep = "_")
  
  # Writing files
  
  # initialize series
  dir.create(
    path = file.path(opts$inputPath, "thermal", "series", 
                     tolower(area_zone), tolower(list_params$parameter$name)),
    recursive = TRUE, showWarnings = FALSE
  )
  
  # default case
  if (is.null(list_params$time_series))
    time_series <- list(character(0))
  
  if (NROW(list_params$time_series) == 8736) {
    fill_mat <- as.data.table(
      matrix(
      data = rep(0, times = 24 * ncol(list_params$time_series)), 
      ncol = ncol(list_params$time_series),
      dimnames = list(NULL, colnames(list_params$time_series))
    )
    )
    time_series <- rbind(list_params$time_series, fill_mat)
  }else
    time_series <-as.data.table(list_params$time_series)
  
  # writing series
  fwrite(
    x = time_series, row.names = FALSE, col.names = FALSE, sep = "\t",
    file = file.path(opts$inputPath, "thermal", "series", 
                     tolower(area_zone), tolower(list_params$parameter$name), "series.txt")
  )
  
  # prepro [DATA + MODULATION]
  dir.create(
    path = file.path(opts$inputPath, "thermal", "prepro", 
                     tolower(area_zone), tolower(list_params$parameter$name)),
    recursive = TRUE, showWarnings = FALSE
  )
  
  # default case
  if (is.null(list_params$prepro_data))
    list_params$prepro_data <- as.data.table(
      matrix(data = c(rep(1, times = 365 * 2), 
                                   rep(0, times = 365 * 4)), 
                          ncol = 6)
    )else
      list_params$prepro_data <- as.data.table(list_params$prepro_data)
  
  # writing data
  fwrite(
    x = list_params$prepro_data, row.names = FALSE, col.names = FALSE, sep = "\t",
    file = file.path(opts$inputPath, "thermal", "prepro", 
                     tolower(area_zone), tolower(list_params$parameter$name), "data.txt")
  )
  
  # default case
  if (is.null(list_params$prepro_modulation))
    list_params$prepro_modulation <- as.data.table(
      matrix(data = c(rep(1, times = 365 * 24 * 3), 
                                         rep(0, times = 365 * 24 * 1)), 
                                ncol = 4)
    )else
      list_params$prepro_modulation <- as.data.table(list_params$prepro_modulation)
      
  # writing modulation
  fwrite(
    x = list_params$prepro_modulation, row.names = FALSE, col.names = FALSE, sep = "\t",
    file = file.path(opts$inputPath, "thermal", "prepro", 
                     tolower(area_zone), tolower(list_params$parameter$name), "modulation.txt")
  )

  return(list_params$parameter)
  
}



