
#' @title Create serial thermal cluster
#' @description For each area, the thermal cluster data are generated
#' @param cluster_object \code{list} native components of cluster
#' @param add_prefix \code{logical} prefix cluster name with area name
#' @param area_zone \code{character} name of area to create cluster
#' 
#' @template opts
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
                              area_zone= "fr",
                              opts = antaresRead::simOptions()){
  
  # check class parametre
  assertthat::assert_that(inherits(opts, "simOptions"))
  
  # pour chaque "objet" cluster
    # ecriture des fichiers thermal
 
   # for (i in seq_along(cluster_object)) {
  #   # multi ecriture
  #   multi_cluster <- do.call(".createClusterBulk", c(
  #     cluster_object[[i]],
  #       list(
  #         area_zone= area_zone,
  #         add_prefix= add_prefix,
  #         opts_study = opts)))
  # }
  
  lapply(cluster_object, .createClusterBulk,
         area_zone= area_zone, add_prefix= add_prefix, opts_study = opts)
  
  ## Ini file
  pathIni <- file.path(opts$inputPath, "thermal", "clusters", area_zone, "list.ini")
  
  # selection depuis la structure des parametres pour ecriture fichier .ini
  ini_params <- lapply(cluster_object, '[[', "parameter")
  
  # ecriture "list.ini" en bloc
    # renommage des parametres (uniquement separateur "-" )
  ini_params <- lapply(ini_params, hyphenize_names)
  
  # ecriture pour une zone (area)
  writeIni(listData = ini_params, pathIni = pathIni, overwrite = TRUE)
  
  # return
  suppressWarnings({
    res <- antaresRead::setSimulationPath(path = opts$studyPath, simulation = "input")
  })
  invisible(res)
}


#' @param ... \code{list} of several named `list` thermal data to ini parameters.
#'  **Warning** all arguments for creating thermal cluster must be provided, see examples.
#' @export

.createClusterBulk <- function(...,
                               area_zone,
                               add_prefix,
                               opts = antaresRead::simOptions()){
  
  ##
  # tests TS
  ##
  
  list_params = list(...)[[1]]
  
  if (!NROW(list_params$time_series) %in% c(0, 8736, 8760)) {
    stop("Number of rows for time series must be 0 or 8760")
  }
  
  if (!NROW(list_params$prepro_modulation) %in% c(0, 8736, 8760)) {
    stop("Number of rows for modulation data must be 0 or 8760")
  }
  if (!NCOL(list_params$prepro_modulation) %in% c(1, 4)) {
    stop("Number of cols for modulation data must be 0 or 4")
  }
  
  
  # save nom cluster
  cluster_name <- list_params$parameter$name
  
  # maj des parametres si prefix
  if (add_prefix)
    list_params$parameter$name <- paste(area_zone, 
                                                list_params$parameter$name, 
                                                sep = "_")
  
  # check ou stop de l'etude
  assertthat::assert_that(!is.null(opts$inputPath) && file.exists(opts$inputPath))
  check_area_name(area_zone, opts)
  
  ##
  # ECRITURES fichiers
  ##
  
  # initialize series [TS]
  dir.create(
    path = file.path(opts$inputPath, "thermal", "series", tolower(area_zone), tolower(cluster_name)),
    recursive = TRUE, showWarnings = FALSE
  )
  
  if (is.null(list_params$time_series))
    time_series <- character(0)
  
  if (NROW(list_params$time_series) == 8736) {
    fill_mat <- matrix(
      data = rep(0, times = 24 * ncol(list_params$time_series)), 
      ncol = ncol(list_params$time_series),
      dimnames = list(NULL, colnames(list_params$time_series))
    )
    time_series <- rbind(list_params$time_series, fill_mat)
  }
  
  utils::write.table(
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
  
  # default is null
  if (is.null(list_params$prepro_data))
    prepro_data <- matrix(data = c(rep(1, times = 365 * 2), 
                                   rep(0, times = 365 * 4)), 
                          ncol = 6)
  
  # ecriture prepro data
  utils::write.table(
    x = list_params$prepro_data, row.names = FALSE, col.names = FALSE, sep = "\t",
    file = file.path(opts$inputPath, "thermal", "prepro", 
                     tolower(area_zone), tolower(cluster_name), "data.txt")
  )
  
  # default is null
  if (is.null(list_params$prepro_modulation))
    prepro_modulation <- matrix(data = c(rep(1, times = 365 * 24 * 3), 
                                         rep(0, times = 365 * 24 * 1)), 
                                ncol = 4)
  
  utils::write.table(
    x = list_params$prepro_modulation, row.names = FALSE, col.names = FALSE, sep = "\t",
    file = file.path(opts$inputPath, "thermal", "prepro", 
                     tolower(area_zone), tolower(cluster_name), "modulation.txt")
  )

  
  
}



