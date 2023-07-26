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
#' @section Warning:
#'
#' For an **Antares version >= 860**, control of data consistency between `mingen.txt` and `maxpower_<area>.txt` can be executed.
#'
#' This control depends on the values you find in `hydro.ini` file.
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
  
  if (opts$antaresVersion >= 860 & type == "maxpower") {
    data_ori <- antaresRead:::fread_antares(opts = opts, file = values_file)
  }
  
  # v860 - save the original data
  if (isTRUE(file.size(values_file) > 0) && !overwrite)
    stop(type," Data already exist for this area. Use overwrite=TRUE if you want to overwrite them.",
         call. = FALSE)
  
  fwrite(x = data, row.names = FALSE, col.names = FALSE, sep = "\t", file = values_file)
  
  # v860 - rollback to original data if necessary
  if (opts$antaresVersion >= 860 & type == "maxpower") {
    comp_mingen_vs_maxpower <- check_mingen_vs_maxpower(area, opts)
    if (!comp_mingen_vs_maxpower$check){
      cat(comp_mingen_vs_maxpower$msg)
      rollback_to_previous_data(area = area, prev_data = data_ori, rollback_type = type, opts = opts)
    }
  }
}


#' @title Get default hydro.ini values 
get_default_hydro_ini_values <- function(){
  
  default_hydro_params <- list(
        "inter-daily-breakdown" = 1, #
        "intra-daily-modulation" = 24, #
        "inter-monthly-breakdown" = 1, #
        "leeway low" = 1, #v7
        "leeway up" = 1, #v7
        "pumping efficiency" = 1, #v7
        "initialize reservoir date" = 0, #v7
        "follow load" = TRUE,
        "use heuristic" = TRUE,
        "use water" = FALSE, # v7
        "hard bounds" = FALSE,
        "use leeway" = FALSE, #v7
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
#' @param mode Execution mode. Useful when you create a new area or remove an existing area to avoid control on hydro data.
#' @param opts List of simulation parameters returned by the function
#'   [antaresRead::setSimulationPath()].
#'
#' @export
#'
#' @importFrom antaresRead simOptions readIni
#' @importFrom assertthat assert_that
#'
#' @section Warning:
#' For an **Antares version >= 860**, control of data consistency between `mingen.txt` and `mod.txt` can be executed.
#'
#' For an **Antares version >= 860**, control of data consistency between `mingen.txt` and `maxpower_<area>.txt` can be executed.
#'
#' These controls depend on the values you find in `hydro.ini` file.
#'
#' @examples
#' \dontrun{
#' opts <- setSimulationPath(studypath, simulation = "input")
#' createArea("fictive_area") 
#' writeIniHydro(area = "fictive_area"
#' , params = list("leeway low" = 2.5, "leeway up" = 25))
#'
#' }
#'
writeIniHydro <- function(area, params, mode = "other", opts = antaresRead::simOptions()){
  
  assertthat::assert_that(inherits(opts, "simOptions"))
  assertthat::assert_that(mode %in% c("createArea", "removeArea", "other"), msg = "Impossible value")
  
  if (mode %in% c("removeArea", "other")) {
    check_area_name(area, opts)
  }
  area <- tolower(area)
  
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
  if (!all(params_names %in% expected_params_names)) {
    stop(append("Parameter params must be named with the following elements:\n ", 
                paste0(expected_params_names, collapse = "\n ")))
  }
  
  # Type control
  check_param_types <- sapply(params_names, FUN = function(x) {
    inherits(params[[x]], what = expected_params[[x]])
  })
  
  bad_param_types <- check_param_types[!check_param_types]
  if (length(bad_param_types) > 0) {
    stop(append("The following parameters have a wrong type:\n ", 
                paste0(names(bad_param_types), collapse = "\n ")))
  }
  
  # Previous data
  path_ini_hydro <- file.path("input", "hydro", "hydro.ini")
  ini_hydro_data <- readIni(path_ini_hydro, opts = opts)
  # v860 - save the original data
  ini_hydro_data_ori <- ini_hydro_data
  
  # Edit hydro data
  for(name in params_names){
    ini_hydro_data[[name]][[area]] <- params[[name]]
  }
  
  writeIni(ini_hydro_data, pathIni = path_ini_hydro, opts = opts, overwrite = TRUE)
  
  # v860 - rollback to original data if necessary
  if (opts$antaresVersion >= 860 & mode == "other") {
    comp_mingen_vs_hydro_storage <- check_mingen_vs_hydro_storage(area, opts)
    comp_mingen_vs_maxpower <- check_mingen_vs_maxpower(area, opts)
    if (!comp_mingen_vs_hydro_storage$check){
      cat(comp_mingen_vs_hydro_storage$msg)
      rollback_to_previous_data(area = area, prev_data = ini_hydro_data_ori, rollback_type = "iniHydro", opts = opts)
    }
    if (!comp_mingen_vs_maxpower$check){
      cat(comp_mingen_vs_maxpower$msg)
      rollback_to_previous_data(area = area, prev_data = ini_hydro_data_ori, rollback_type = "iniHydro", opts = opts)
    }
  }
}


#' @title Rollback to previous hydro data if the data is not consistent
#' 
#' @description 
#' Rollback the data to previous one if the check is KO. 
#' For a given area, check if the data is consistent and rollback to previous data if the check is KO.
#'
#' @param area The area where to execute the control and rollback the data.
#' @param prev_data The original data to restore if necessary.
#' @param rollback_type The file to restore and the control(s) to execute.
#' @param opts List of simulation parameters returned by the function
#'   [antaresRead::setSimulationPath()].
#'
#' @note
#' Function called only for an **Antares version >= 860**.
#'
#' @importFrom antaresRead simOptions
#' @importFrom assertthat assert_that
#' @importFrom data.table fwrite
#'
rollback_to_previous_data <- function(area, prev_data, rollback_type, opts = antaresRead::simOptions()){
  
  assertthat::assert_that(inherits(opts, "simOptions"))
  assertthat::assert_that(rollback_type %in% c("iniHydro", "maxpower", "mingen", "hydroSTOR"), msg = "Impossible value")
  check_area_name(area, opts)
  
  area <- tolower(area)
  
  # File to write the previous data
  rollback_filepath <- switch(rollback_type,
                              "iniHydro" = file.path("input",
                                                     "hydro",
                                                     "hydro.ini"),
                              "hydroSTOR" = file.path(opts$inputPath,
                                                      "hydro",
                                                      "series",
                                                      area,
                                                      "mod.txt"),
                              "mingen" = file.path(opts$inputPath,
                                                   "hydro",
                                                   "series",
                                                   area,
                                                   "mingen.txt"),
                              "maxpower" = file.path(opts$inputPath,
                                                     "hydro",
                                                     "common",
                                                     "capacity",
                                                     paste0("maxpower_", area, ".txt"))
  )
  # Displayed message if data is not consistent
  stop_message <- sprintf("File %s can not be updated", rollback_filepath)
  
  if (rollback_type == "iniHydro") {
    writeIni(prev_data, pathIni = rollback_filepath, opts = opts, overwrite = TRUE)
  }
  if (rollback_type %in% c("hydroSTOR", "mingen", "maxpower")) {
    fwrite(
      x = as.data.table(prev_data),
      row.names = FALSE, 
      col.names = FALSE, 
      sep = "\t",
      file = rollback_filepath
    )
  }
  stop(stop_message, call. = FALSE)
}


#' @title Write default values in hydro.ini file if the section is empty
#' 
#' @description
#' For a given area, if the data is empty, pick value from default values for `use heuristic`, `follow load` and `reservoir` sections.
#' 
#' @param area The area where to write the value, i.e. lhs in the section.
#' @param opts List of simulation parameters returned by the function
#'   [antaresRead::setSimulationPath()].
#'
#' @importFrom antaresRead simOptions readIni
#'
fill_empty_hydro_ini_file <- function(area, opts = antaresRead::simOptions()){
  
  assertthat::assert_that(inherits(opts, "simOptions"))
  check_area_name(area, opts)
  
  area <- tolower(area)
  
  path_ini_hydro <- file.path("input", "hydro", "hydro.ini")
  ini_hydro_data <- antaresRead::readIni(path_ini_hydro, opts = opts)
  default_params <- get_default_hydro_ini_values()
  # use heuristic
  if (is.null(ini_hydro_data[["use heuristic"]][[area]])) {
    ini_hydro_data[["use heuristic"]][[area]] <- default_params[["use heuristic"]]
    writeIni(ini_hydro_data, pathIni = path_ini_hydro, opts = opts, overwrite = TRUE)
    ini_hydro_data <- antaresRead::readIni(path_ini_hydro, opts = opts)
  }
  # follow load
  if (is.null(ini_hydro_data[["follow load"]][[area]])) {
    ini_hydro_data[["follow load"]][[area]] <- default_params[["follow load"]]
    writeIni(ini_hydro_data, pathIni = path_ini_hydro, opts = opts, overwrite = TRUE)
    ini_hydro_data <- antaresRead::readIni(path_ini_hydro, opts = opts)
  }
  # reservoir
  if (is.null(ini_hydro_data[["reservoir"]][[area]])) {
    ini_hydro_data[["reservoir"]][[area]] <- default_params[["reservoir"]]
    writeIni(ini_hydro_data, pathIni = path_ini_hydro, opts = opts, overwrite = TRUE)
    ini_hydro_data <- antaresRead::readIni(path_ini_hydro, opts = opts)
  }
}


#' @title Write default input time series if `mingen.txt` or/and `mod.txt` is empty
#' 
#' @param area The area where to write the input time series.
#' @param opts List of simulation parameters returned by the function
#'   [antaresRead::setSimulationPath()].
#'
#' @importFrom antaresRead simOptions
#' @importFrom data.table fwrite
#'
fill_empty_hydro_ts_file <- function(area, opts = antaresRead::simOptions()){

  assertthat::assert_that(inherits(opts, "simOptions"))
  check_area_name(area, opts)
  
  area <- tolower(area)
  
  if (opts$typeLoad == "txt") {
    file_mod <- file.path(opts$inputPath, "hydro", "series", area, "mod.txt")
    if (file.info(file_mod)$size == 0) {
      fwrite(
            x = as.data.table(rep(0, 365)),
            row.names = FALSE, 
            col.names = FALSE, 
            sep = "\t",
            file = file_mod
      )  
    }
    if (opts$antaresVersion >= 860) {
      file_mingen <- file.path(opts$inputPath, "hydro", "series", area, "mingen.txt")
      if (file.info(file_mingen)$size == 0) {
        fwrite(
              x = as.data.table(rep(0, 8760)),
              row.names = FALSE, 
              col.names = FALSE, 
              sep = "\t",
              file = file_mingen
        )  
      }
    }
  }
}


#' @title Get the type of control to execute using the 3 necessary booleans
#'
#' @param hydro_params a list of 3 booleans to compute the type of control to make.
#'
#' @importFrom assertthat assert_that
#'
#' @return 
#' a character containing the type of control to execute.
#'
get_type_check_mingen_vs_hydrostorage <- function(hydro_params){
  
  assertthat::assert_that(inherits(hydro_params[["use heuristic"]], "logical"))
  assertthat::assert_that(inherits(hydro_params[["follow load"]], "logical"))
  assertthat::assert_that(inherits(hydro_params[["reservoir"]], "logical"))
  
  timeStep_control <- NULL
  
  use_heuristic <- hydro_params[["use heuristic"]]
  follow_load <- hydro_params[["follow load"]]
  reservoir <- hydro_params[["reservoir"]]
  
  if (use_heuristic & !follow_load) {
    timeStep_control <- "weekly"
  }  
  if (use_heuristic & follow_load) {
    if (!reservoir) {
      timeStep_control <- "monthly"
    } else {
      timeStep_control <- "annual"
    }
  }
  
  return(timeStep_control)
}


#' @title Get the type of control to execute between mingen data and hydro storage data
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
#' @note
#' Function called only for an **Antares version >= 860**.
#'
#' @importFrom antaresRead simOptions readIni
#' @importFrom assertthat assert_that
#'
#' @return 
#' a character containing the type of control to execute.
#'
get_type_check_mingen_vs_hydrostorage_to_trigger <- function(area, opts = antaresRead::simOptions()){
  
  assertthat::assert_that(inherits(opts, "simOptions"))
  check_area_name(area, opts)
  
  area <- tolower(area)
  
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
  
  return(type_control)
}


#' @title Replicate a data.table as many times as needed to get the same number of time series between 2 data.tables
#'
#' @param xts a data.table of time series type to replicate if necessary.
#' @param yts a data.table of time series type to use as reference to match its number of time series.
#'
#' @return 
#' the data.table x replicated to match the number of time series of y.
#'
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


#' @title Add week number column to a data.time of time series type
#' 
#' @description 
#' If timeId column exists, add a week number column. A week is 168 consecutive hours (= 24 * 7).
#'
#' @param xts a data.table of time series type.
#'
#' @return 
#' the data.table xts with a new column week.
#'
add_week_number_column_to_ts <- function(xts){
  
  if ("timeId" %in% colnames(xts)) {
    nb_hours_per_week <- 24 * 7
    xts <- xts[, week := (timeId-1)%/%nb_hours_per_week + 1]
  }
  
  return(xts)
}


#' @title Check if mingen data and hydro storage data are consistent
#' 
#' @description
#' At each weekly/monthly/annual time step, mingen must be less or equal than hydro storage.
#' 
#' @param area The area where to check the data.
#' @param opts List of simulation parameters returned by the function
#'   [antaresRead::setSimulationPath()].
#'
#' @note
#' Function called only for an **Antares version >= 860**.
#'
#' @importFrom antaresRead simOptions readInputTS
#' @importFrom assertthat assert_that
#'
#' @return 
#' a list containing the boolean if the check is ok and the message to display.
#'
check_mingen_vs_hydro_storage <- function(area, opts = antaresRead::simOptions()){
  
  assertthat::assert_that(inherits(opts, "simOptions"))
  check_area_name(area, opts)
  
  area <- tolower(area)
  
  check <- TRUE
  msg <- ""
  mingen <- NULL
  hydroStorage <- NULL
  
  timeStep_control <- get_type_check_mingen_vs_hydrostorage_to_trigger(area, opts)
  if (!is.null(timeStep_control)) {
    # Fill ts files if empty
    fill_empty_hydro_ts_file(area, opts)
    # Read the mingen data and hydro storage data
    if (timeStep_control %in% c("monthly", "annual")) {
      mingen_data <- antaresRead::readInputTS(mingen = area, opts = opts, timeStep = timeStep_control)
      mod_data <- antaresRead::readInputTS(hydroStorage = area, opts = opts, timeStep = timeStep_control)
    }
    # weekly is not used as argument in readInputTS
    # for this control, a week is a 168 consecutive hours from the first hour
    # parameters from settings/generaldata.ini are not used here and they are used in the implementation of readInputTS/changetimeStep
    if (timeStep_control == "weekly") {
      mingen_data <- antaresRead::readInputTS(mingen = area, opts = opts, timeStep = "hourly")
      mod_data <- antaresRead::readInputTS(hydroStorage = area, opts = opts, timeStep = "hourly")
      mingen_data <- add_week_number_column_to_ts(mingen_data)      
    }
    # Replicate the ts to have the same number of ts for the 2 data tables
    mingen_data <- replicate_missing_ts(mingen_data, mod_data)
    mod_data <- replicate_missing_ts(mod_data, mingen_data)
    # Join the data
    dt_merged <- merge(mingen_data,
                       mod_data[,c("timeId", "tsId", "hydroStorage")],
                       by = c("timeId", "tsId")
                      )
    if (timeStep_control == "weekly") {
      dt_merged <- dt_merged[,
                             list(mingen = sum(mingen), hydroStorage = sum(hydroStorage)),
                             by = c("tsId", "week")
                             ]
    }
    # Check consistency
    check <- all(dt_merged$hydroStorage >= dt_merged$mingen)
    
    if (!check) {
      msg <- sprintf("Data does not respect the %s condition.\nYou must check input/hydro/series/%s/mingen.txt and input/hydro/series/%s/mod.txt\n"
      , timeStep_control, area, area
      )
    }
  }
  
  return(list("check" = check, "msg" = msg))
}


#' @title Get the type of control to execute between mingen data and maxpower data
#' 
#' @description 
#'
#' Compute the type of control to make between :
#' \itemize{
#'      \item{`input/hydro/series/<area>/mingen.txt`}
#'      \item{`input/hydro/common/capacity/maxpower_<area>.txt`}
#' }
#' This control is implemented in Antares too.
#' No control to execute if `reservoir` section in hydro.ini for the area is set to TRUE. 
#' 
#' @param area The area where the type of control must be computed.
#' @param opts List of simulation parameters returned by the function
#'   [antaresRead::setSimulationPath()].
#'
#' @note
#' Function called only for an **Antares version >= 860**.
#'
#' @importFrom antaresRead simOptions readIni
#' @importFrom assertthat assert_that
#'
#' @return 
#' a character containing the type of control to execute.
#'
get_type_check_mingen_vs_maxpower_to_trigger <- function(area, opts = antaresRead::simOptions()){
  
  assertthat::assert_that(inherits(opts, "simOptions"))
  check_area_name(area, opts)
  
  timeStep_control <- NULL
  
  area <- tolower(area)
  fill_empty_hydro_ini_file(area, opts)
  path_ini_hydro <- file.path("input", "hydro", "hydro.ini")
  ini_hydro_data <- antaresRead::readIni(path_ini_hydro, opts = opts)
  
  reservoir <- ini_hydro_data[["reservoir"]][[area]]
  
  if (!reservoir) {
    timeStep_control <- "hourly"
  }
    
  return(timeStep_control)
}


#' @title Check if mingen data and maxpower data are consistent
#' 
#' @description 
#' At each hourly time step, mingen must be less or equal than generatingMaxPower.
#' 
#' @param area The area where to check the data.
#' @param opts List of simulation parameters returned by the function
#'   [antaresRead::setSimulationPath()].
#'
#' @note
#' Function called only for an **Antares version >= 860**.
#'
#' @importFrom antaresRead simOptions readIni readInputTS
#' @importFrom assertthat assert_that
#'
#' @return 
#' a list containing the boolean if the check is ok and the message to display.
#'
check_mingen_vs_maxpower <- function(area, opts = antaresRead::simOptions()){
  
  assertthat::assert_that(inherits(opts, "simOptions"))
  check_area_name(area, opts)
  
  area <- tolower(area)
  
  check <- TRUE
  msg <- ""
  
  timeStep_control <- get_type_check_mingen_vs_maxpower_to_trigger(area, opts)
  if (!is.null(timeStep_control)) {
    # Fill ts files if empty
    fill_empty_hydro_ts_file(area, opts)
    # Read the mingen data and maxpower data
    mingen_data <- antaresRead::readInputTS(mingen = area, opts = opts)
    maxpower_data <- antaresRead::readInputTS(hydroStorageMaxPower = area, opts = opts)
    # Join the data
    dt_merged <- merge(mingen_data, maxpower_data[,c("timeId","generatingMaxPower")], by = "timeId")
    # Check consistency
    check <- all(dt_merged$generatingMaxPower >= dt_merged$mingen)
    if (!check) {
      msg <- sprintf("Data does not respect the hourly condition.\nYou must check input/hydro/series/%s/mingen.txt and input/hydro/common/capacity/maxpower_%s.txt\n"
      , area, area)
    }
  }
  
  return(list("check" = check, "msg" = msg))
}
