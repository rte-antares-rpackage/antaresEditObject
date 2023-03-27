
#' @title Create serial thermal cluster
#' @description For each area, the thermal cluster data are generated
#' @param cluster_object \code{list} mutiple list containing the parameters for writing each cluster 
#' @param add_prefix \code{logical} prefix cluster name with area name
#' @param area_zone \code{character} name of area to create cluster
#' 
#' @template opts
#' 
#' @details see the example to write a cluster object, 
#' see the original function [createCluster()]
#' 
#' @return \code{list} containing meta information about the simulation
#' @export
#' @examples
#' \dontrun{
#' 
#' library(antaresRead)
#' library(antaresEditObject)
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
# '
#' 
#' # data structure for an area
#' zone_test_1 <- list(
#'   `CCGT old 1`= list(parameter= list(
#'     name= "CCGT old 1",
#'     group = "Other",
#'     unitcount= 10L,
#'     nominalcapacity= 100,
#'     enabled= "true",
#'    `min-stable-power`= 80L,
#'     `min-up-time`= 20L,
#'     `min-down_time`= 30L),
#'     time_series = ts,
#'     prepro_data = df_pd,
#'     prepro_modulation = df_pm),
#'   
#'   `CCGT old 2`= list(parameter= list(
#'     name= "CCGT old 2",
#'     group = "Other"),
#'     time_series = ts,
#'     prepro_data = df_pd,
#'     prepro_modulation = df_pm)
#' )
#' 
#' # Create multiple areas with multiple clusters
#' list_areas <- antaresRead::getAreas()[1:5]
#' 
#' lapply(list_areas, createClusterBulk,
#' cluster_object = c(zone_test_1, zone_test_2),
#' add_prefix = TRUE, 
#' opts = opts_temp)
#' 
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
  
  # for each cluster "object"
    # writing thermal files
  lapply(cluster_object, .createClusterBulk,
         area_zone= area_zone, add_prefix= add_prefix, opts_study = opts)
  
  # Ini file path
  pathIni <- file.path(opts$inputPath, "thermal", "clusters", area_zone, "list.ini")
  
  # extract only the pamateres
  ini_params <- lapply(cluster_object, '[[', "parameter")
  
  # check names
  ini_params <- lapply(ini_params, hyphenize_names)
  
  # writing 
  writeIni(listData = ini_params, pathIni = pathIni, overwrite = TRUE)
  
  # Update simulation options object
  suppressWarnings({
    res <- antaresRead::setSimulationPath(path = opts$studyPath, simulation = "input")
  })
  invisible(res)
}


#' @param ... \code{list} named `list` of cluster parameter
#' @importFrom data.table fwrite
.createClusterBulk <- function(...,
                               area_zone,
                               add_prefix,
                               opts = antaresRead::simOptions()){
  
  # re-adjustment of list parameters
  list_params = list(...)[[1]]
  
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
    cluster_name <- paste(area_zone, 
                          list_params$parameter$name, 
                          sep = "_")
  
  # Writing files
  
  # initialize series
  dir.create(
    path = file.path(opts$inputPath, "thermal", "series", 
                     tolower(area_zone), tolower(cluster_name)),
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
                     tolower(area_zone), tolower(cluster_name), "series.txt")
  )
  
  # prepro [DATA + MODULATION]
  dir.create(
    path = file.path(opts$inputPath, "thermal", "prepro", 
                     tolower(area_zone), tolower(cluster_name)),
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
                     tolower(area_zone), tolower(cluster_name), "data.txt")
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
                     tolower(area_zone), tolower(cluster_name), "modulation.txt")
  )

  
  
}



