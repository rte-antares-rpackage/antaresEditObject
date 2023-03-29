#' @title Write Hydro Values
#' 
#' @description 
#' `r antaresEditObject:::badge_api_ok()`
#' 
#' Write water, reservoirLevels, maxpower, inflowPattern and creditModulations values for a given area.
#'
#' @param area The area where to add the values.
#' @param type Type of hydro file, it can be "waterValues", "reservoir", "maxpower", "inflowPattern" or "creditmodulations".
#' @param data The data must have specific dimension depending on the type of file. 
#'   For waterValues, a 365x101 numeric matrix:
#'   marginal values for the stored energy based on date (365 days)
#'   and on the reservoir level (101 round percentage values ranging from
#'   0% to 100%). OR a 3-column matrix with 365x101 rows. In this latter case the 3 columns must
#'   be 'date', 'level' and 'value' (in this order), and the rows must be sorted by:
#'   ascending day, ascending level.
#'   For reservoir, a 365x3 numeric matrix. The columns contains respectively the levels min, avg and max.
#'   For maxpower, a 365x4 numeric matrix.
#'   For inflowPattern, a 365x1 numeric matrix.
#'   For creditmodulations, a 2x101 numeric matrix.
#'   
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
#' writeValues("fictive_area", data = matrix(rep(0, 365*101), nrow = 365))
#'
#' }
writeHydroValues <- function(area,
                             type,
                             data = NULL, 
                             overwrite = TRUE,
                             opts = antaresRead::simOptions()) {
  
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
    if (type!="WaterValues")
      if (!(identical(dim(data), dims)))
        stop(type, " 'data' must be a ", 
             do.call(paste, as.list(c(dims, sep = " x "))), " matrix.", call. = FALSE)
  }
  
  check_area_name(area, opts)
  
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
