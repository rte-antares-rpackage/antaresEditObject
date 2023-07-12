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
