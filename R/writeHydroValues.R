#' @title Write Hydro Values
#' 
#' @description 
#' `r antaresEditObject:::badge_api_ok()`
#' 
#' Write waterValues, reservoirLevels, maxpower, inflowPattern and creditModulations data for a given area.
#'
#' @param area The area where to add the values.
#' @param type Type of hydro file, it can be "waterValues", "reservoir", "maxpower", "inflowPattern" or "creditmodulations".
#' @param data The data must have specific dimension depending on the type of file :
#' \itemize{
#'   \item{waterValues}{: a 365x101 numeric matrix:
#'   marginal values for the stored energy based on date (365 days)
#'   and on the reservoir level (101 round percentage values ranging from
#'   0% to 100%). OR a 3-column matrix with 365x101 rows. In this latter case the 3 columns must
#'   be 'date', 'level' and 'value' (in this order), and the rows must be sorted by:
#'   ascending day, ascending level.}
#'   \item{reservoir}{: a 365x3 numeric matrix. The columns contains respectively the levels min, avg and max.}
#'   \item{maxpower}{: a 365x4 numeric matrix.}
#'   \item{inflowPattern}{: a 365x1 numeric matrix.}
#'   \item{creditmodulations}{: a 2x101 numeric matrix.}
#'   }
#'   
#' @param overwrite Logical. Overwrite the values if a file already exists.
#' @param opts List of simulation parameters returned by the function
#'   [antaresRead::setSimulationPath()].
#'
#' @export
#'
#' @importFrom antaresRead simOptions
#' @importFrom assertthat assert_that
#' @importFrom data.table dcast data.table fwrite
#'
#' @examples
#' \dontrun{
#'
#' writeHydroValues("fictive_area", type = "inflowPattern", data = matrix(rep(0, 365*1), nrow = 365))
#'
#' }
writeHydroValues <- function(area,
                             type,
                             data, 
                             overwrite = TRUE,
                             opts = antaresRead::simOptions()) {
  
  check_area_name(area, opts)
  
  type <- match.arg(type, c("waterValues", "reservoir", "maxpower", "inflowPattern", "creditmodulations"))
  assertthat::assert_that(inherits(opts, "simOptions"))
  
  dims <- switch(type,
                 "reservoir" = c(365L, 3L),
                 "maxpower" = c(365L, 4L),
                 "inflowPattern" = c(365L, 1L),
                 "creditmodulations" = c(2L, 101L))
  
  #Case waterValues    
  if (type=="waterValues"){
    if (!(identical(dim(data), c(365L, 101L)) || identical(dim(data), c(36865L, 3L))))
      stop("waterValues 'data' must be either a 365*101 or (365*101)*3 matrix.", call. = FALSE)
    
    if (ncol(data) == 3) {
      data <- data.table::data.table(data)
      names(data) <- c("date", "level", "value")
      data <- data.table::dcast(data, formula = date ~ level, value.var = "value")
      data$date <- NULL
    }
  } else {
    #Other cases
    if (!(identical(dim(data), dims)))
      stop(type, " 'data' must be a ", 
           do.call(paste, as.list(c(dims, sep = "*"))), " matrix.", call. = FALSE)
  }
  
  # API block
  if (is_api_study(opts)) {
    cmd <- api_command_generate(
      action = "replace_matrix",
      target = sprintf("input/hydro/common/capacity/%s_%s", type, area),
      matrix = data
    )
    api_command_register(cmd, opts = opts)
    `if`(
      should_command_be_executed(opts), 
      api_command_execute(cmd, opts = opts, text_alert = sprintf("Write %s values: {msg_api}", type)),
      cli_command_registered("replace_matrix")
    )
    
    return(invisible(opts))
  }  
  
  # Input path
  inputPath <- opts$inputPath
  assertthat::assert_that(!is.null(inputPath) && file.exists(inputPath))
  
  values_file <- file.path(inputPath, "hydro", "common", "capacity", paste0(type, "_", tolower(area), ".txt"))
  
  if (isTRUE(file.size(values_file) > 0) && !overwrite)
    stop(type," Data already exist for this area. Use overwrite=TRUE if you want to overwrite them.",
         call. = FALSE)
  
  fwrite(x = data, row.names = FALSE, col.names = FALSE, sep = "\t", file = values_file)
}


#' @title Get default hydro.ini values 
get_default_hydro_ini_values <- function(){
  
  default_hydro_params <- list(
        "inter-daily-breakdown" = 1,
        "intra-daily-modulation" = 24,
        "inter-monthly-breakdown" = 1,
        "leeway low" = 1,
        "leeway up" = 1,
        "pumping efficiency" = 1,
        "initialize reservoir date" = 0,
        "follow load" = TRUE,
        "use heuristic" = TRUE,
        "use water" = FALSE,
        "hard bounds" = FALSE,
        "use leeway" = FALSE,
        "power to level" = FALSE,
        "reservoir" = FALSE,
        "reservoir capacity" = 0
  )
  
  return(default_hydro_params)
}


#' @title Edit hydro.ini values 
#' 
#' @description 
#' `r antaresEditObject:::badge_api_ok()`
#' 
#' For a given area, write its data in the hydro.ini file.
#' @param area The area where to edit the values.
#' @param params The list data must have specific names and specific types :
#' \itemize{
#'      \item{follow load}{ : logical or NULL}
#'      \item{use heuristic}{ : logical or NULL}
#'      \item{use water}{ : logical or NULL}
#'      \item{hard bounds}{ : logical or NULL}
#'      \item{use leeway}{ : logical or NULL}
#'      \item{power to level}{ : logical or NULL}
#'      \item{reservoir}{ : logical or NULL}
#'      \item{inter-daily-breakdown}{ : numeric, integer or NULL}
#'      \item{intra-daily-modulation}{ : numeric, integer or NULL}
#'      \item{inter-monthly-breakdown}{ : numeric, integer or NULL}
#'      \item{leeway low}{ : numeric, integer or NULL}
#'      \item{leeway up}{ : numeric, integer or NULL}
#'      \item{pumping efficiency}{ : numeric, integer or NULL}
#'      \item{initialize reservoir date}{ : numeric, integer or NULL}
#'      \item{reservoir capacity}{ : numeric, integer or NULL}
#'   }
#' @param with_check_area Enable the control of the areas' existence. Useful when you create a new area.
#' @param opts List of simulation parameters returned by the function
#'   [antaresRead::setSimulationPath()].
#'
#' @export
#'
#' @importFrom antaresRead simOptions readIni
#' @importFrom assertthat assert_that
#'
#' @examples
#' \dontrun{
#' opts <- setSimulationPath(studypath, simulation = "input")
#' createArea("fictive_area") 
#' writeIniHydro(area = "fictive_area"
#' , params = list("leeway low" = 2.5, "leeway up" = 25))
#'
#' }
writeIniHydro <- function(area, params, with_check_area = TRUE, opts = antaresRead::simOptions()){
  
  assertthat::assert_that(inherits(opts, "simOptions"))
  if(with_check_area){
    check_area_name(area, opts)
  }
  
  # Allowed names/types for sections
  expected_params <- list("inter-daily-breakdown" = c("numeric", "integer", "NULL"),
                          "intra-daily-modulation" = c("numeric", "integer", "NULL"),
                          "inter-monthly-breakdown" = c("numeric", "integer", "NULL"),
                          "leeway low" = c("numeric", "integer", "NULL"),
                          "leeway up" = c("numeric", "integer", "NULL"),
                          "pumping efficiency" = c("numeric", "integer", "NULL"),
                          "initialize reservoir date" = c("numeric", "integer", "NULL"),
                          "follow load" = c("logical", "NULL"),
                          "use heuristic" = c("logical", "NULL"),
                          "use water" = c("logical", "NULL"),
                          "hard bounds" = c("logical", "NULL"),
                          "use leeway" = c("logical", "NULL"),
                          "power to level" = c("logical", "NULL"),
                          "reservoir" = c("logical", "NULL"),
                          "reservoir capacity" = c("numeric", "integer", "NULL")
  )
  
  params_names <- names(params)
  expected_params_names <- names(expected_params)
  
  # Name control
  if(!all(params_names %in% expected_params_names)){
    stop(append("Parameter params must be named with the following elements:\n ", 
                paste0(expected_params_names, collapse = "\n ")))
  }
  
  # Type control
  check_param_types <- sapply(params_names, FUN = function(x) {
    inherits(params[[x]], what = expected_params[[x]])
  })
  
  bad_param_types <- check_param_types[!check_param_types]
  if(length(bad_param_types) > 0){
    stop(append("The following parameters have a wrong type:\n ", 
                paste0(names(bad_param_types), collapse = "\n ")))
  }
  
  # Previous data
  path_ini_hydro <- file.path("input", "hydro", "hydro.ini")
  ini_hydro_data <- readIni(path_ini_hydro, opts = opts)
  
  # Edit hydro data
  for(name in params_names){
    ini_hydro_data[[name]][[area]] <- params[[name]]
  }
  
  writeIni(ini_hydro_data, pathIni = path_ini_hydro, opts = opts, overwrite = TRUE)
}




#' @title Write default values in hydro.ini file if the section is empty.
#' 
#' @description 
#' `r antaresEditObject:::badge_api_ok()`
#'
#' For a given area, if the data is empty, write TRUE for `use heuristic` and `follow load` sections and FALSE for `reservoir` section.
#' 
#' @param area The area where to write the value, i.e. lhs in the section.
#' @param opts List of simulation parameters returned by the function
#'   [antaresRead::setSimulationPath()].
#'
#' @importFrom antaresRead simOptions readIni
#'
fill_empty_hydro_ini_file <- function(area, opts = antaresRead::simOptions()){
  
  if (opts$antaresVersion >= 860) {
    path_ini_hydro <- file.path("input", "hydro", "hydro.ini")
    ini_hydro_data <- antaresRead::readIni(path_ini_hydro, opts = opts)
    if (is.null(ini_hydro_data[["use heuristic"]][[area]])) {
      writeIniHydro(area, list("use heuristic" = TRUE), opts = opts)
      ini_hydro_data <- antaresRead::readIni(path_ini_hydro, opts = opts)
    }
    if (is.null(ini_hydro_data[["follow load"]][[area]])) {
      writeIniHydro(area, list("follow load" = TRUE), opts = opts)
      ini_hydro_data <- antaresRead::readIni(path_ini_hydro, opts = opts)
    }
    if (is.null(ini_hydro_data[["reservoir"]][[area]])) {
      writeIniHydro(area, list("reservoir" = FALSE), opts = opts)
      ini_hydro_data <- antaresRead::readIni(path_ini_hydro, opts = opts)
    }
  }
}



#' @title Write default input time series if file is empty.
#' 
#' @param area The area where to write the input time series.
#' @param opts List of simulation parameters returned by the function
#'   [antaresRead::setSimulationPath()].
#'
fill_empty_hydro_ts_file <- function(area, opts = antaresRead::simOptions()){
  
  if (opts$antaresVersion >= 860 & opts$typeLoad != "api") {
    file_mingen <- file.path(opts$studyPath, "input", "hydro", "series", area, "mingen.txt")
    file_mod <- file.path(opts$studyPath, "input", "hydro", "series", area, "mod.txt")
    if (file.info(file_mingen)$size == 0) {
      writeInputTS(area = area, data = rep(0, 8760), type = "mingen", opts = opts)
    }
    if (file.info(file_mod)$size == 0) {
      writeInputTS(area = area, data = rep(0, 365), type = "hydroSTOR", opts = opts)
    }
  }
}



#' @title Get the type of control to execute using the 3 necessary booleans.
#'
#' @param hydro_params a list of 3 booleans to compute the type of control to make.
#'
#' @importFrom assertthat assert_that
#'
#' @return 
#' a list containing the type of control to execute and the variables to use for aggregation.
get_type_check_mingen_vs_hydrostorage <- function(hydro_params){
  
  assertthat::assert_that(inherits(hydro_params[["use heuristic"]], "logical"))
  assertthat::assert_that(inherits(hydro_params[["follow load"]], "logical"))
  assertthat::assert_that(inherits(hydro_params[["reservoir"]], "logical"))
  
  type_control <- NULL
  
  use_heuristic <- hydro_params[["use heuristic"]]
  follow_load <- hydro_params[["follow load"]]
  reservoir <- hydro_params[["reservoir"]]
  
  if (use_heuristic & !follow_load) {
    type_control <- list("type" = "weekly", "level_aggregation" = c("tsId", "week"))
  }  
  if (use_heuristic & follow_load) {
    if (!reservoir) {
      type_control <- list("type" = "monthly", "level_aggregation" = c("tsId", "month"))
    } else {
      type_control <- list("type" = "yearly", "level_aggregation" = c("tsId"))
    }
  }
  
  return(type_control)
}



#' @title Get the type of control to execute between mingen data and hydro storage data.
#' 
#' @description 
#'
#' Compute the type of control to make between :
#' \itemize{
#'      \item{`input/hydro/series/<area>/mingen.txt`}
#'      \item{`input/hydro/series/<area>/mod.txt`}
#' }
#' This control is implemented in Antares too.
#' 
#' @param area The area where the type of control must be computed.
#' @param opts List of simulation parameters returned by the function
#'   [antaresRead::setSimulationPath()].
#'
#' @importFrom antaresRead simOptions readIni
#' @importFrom assertthat assert_that
#'
#' @return 
#' a list containing the type of control to execute and the variables to use for aggregation.
get_type_check_mingen_vs_hydrostorage_to_trigger <- function(area, opts = antaresRead::simOptions()){
  
  assertthat::assert_that(inherits(opts, "simOptions"))
  check_area_name(area, opts)
  
  type_control <- NULL
  
  if (opts$antaresVersion >= 860) {
    fill_empty_hydro_ini_file(area, opts)
    path_ini_hydro <- file.path("input", "hydro", "hydro.ini")
    ini_hydro_data <- antaresRead::readIni(path_ini_hydro, opts = opts)
    mandatory_params <- c("use heuristic", "follow load", "reservoir")
    ini_hydro_data_fi <- ini_hydro_data[which(names(ini_hydro_data) %in% mandatory_params)]
    ini_hydro_data_area <- sapply(names(ini_hydro_data_fi), FUN = function(name){
      ini_hydro_data_fi[[name]][[area]]},
      simplify = FALSE
    )
    type_control <- get_type_check_mingen_vs_hydrostorage(ini_hydro_data_area)
  }
  
  return(type_control)
}


#' @title Replicate a data.table as many times as needed to get the same number of time series between 2 data.tables.
#'
#' @param x a data.table of time series type to replicate if necessary.
#' @param y a data.table of time series type to use as reference to match its number of time series.
#'
#' @return 
#' the data.table x replicated to match the number of time series of y.
replicate_missing_ts <- function(xts, yts){
  
  nb_ts_x <- max(xts$tsId)
  nb_ts_y <- max(yts$tsId)
  
  if (nb_ts_x == 1 & nb_ts_y > 1) {
    lst_dt <- list()
    lst_dt[[1]] <- xts
    replicated_x <- lapply(seq(1, nb_ts_y - nb_ts_x), FUN = function(id_ts){
      dt <- xts
      dt$tsId <- id_ts + 1
      return(dt)
    })
  lst_dt <- append(lst_dt, replicated_x) 
  xts <- do.call("rbind", lst_dt)   
  }
  
  return(xts)
}


#' @title Add week number column to a data.time of time series type.
#' 
#' @description 
#' If timeId column exists, add the week of the number. A week is 168 consecutive hours (= 24 * 7).
#'
#' @param xts a data.table of time series type.
#'
#' @return 
#' the data.table xts with a new column week.
add_week_number_column_to_ts <- function(xts){
  
  if ("timeId" %in% colnames(xts)) {
    nb_hours_per_week <- 24 * 7
    xts <- xts[, week := (timeId-1)%/%nb_hours_per_week + 1]
  }
  
  return(xts)
}


#' @title Check if mingen data and maxpower data are consistent.
#' 
#' @description
#' At each hourly time step, mingen must be less or equal than maxpower.
#' 
#' @param area The area to check.
#' @param opts List of simulation parameters returned by the function
#'   [antaresRead::setSimulationPath()].
#'
#'
#' @importFrom antaresRead simOptions readInputTS
#' @importFrom assertthat assert_that
#'
check_mingen_vs_hydro_storage <- function(area, opts = antaresRead::simOptions()){
  
  assertthat::assert_that(inherits(opts, "simOptions"))
  check_area_name(area, opts)
  
  check <- TRUE
  msg <- ""
  
  if (opts$antaresVersion >= 860) {
    type_control <- get_type_check_mingen_vs_hydrostorage_to_trigger(area, opts)
    if (!is.null(type_control)) {
      fill_empty_hydro_ts_file(area, opts)
      
      mingen_data <- antaresRead::readInputTS(mingen = area, opts = opts)
      mod_data <- antaresRead::readInputTS(hydroStorage = area, opts = opts)
      
      mingen_data <- replicate_missing_ts(mingen_data, mod_data)
      mod_data <- replicate_missing_ts(mod_data, mingen_data)
    
      dt_merged <- merge(mingen_data, mod_data[,c("timeId", "tsId", "hydroStorage")], by = c("timeId", "tsId"))
    
      dt_agg <- dt_merged[, .(mingen = sum(mingen), hydroStorage = sum(hydroStorage)), by = eval(type_control[["level_aggregation"]])]
      dt_agg$is_hydroST_gt_mingen <- dt_agg$hydroStorage >= dt_agg$mingen
      check <- all(dt_agg$is_hydroST_gt_mingen)
      
      if (!check) {
        msg <- sprintf("Data does not respect the %s condition.\nYou must check input/hydro/series/%s/mingen.txt and input/hydro/series/%s/mod.txt\n"
        , type_control[["type"]], area, area
        )
      }
    }
  }
  
  return(list("check" = check, "msg" = msg))
}
