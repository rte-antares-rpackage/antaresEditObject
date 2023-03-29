#' @title Write Hydro Values
#' 
#' @description 
#' `r antaresEditObject:::badge_api_ok()`
#' 
#' Write water, reservoirLevels, maxpower, inflowPattern and creditModulations values for a given area.
#'
#' @param area The area where to add the values.
#' @param data A 365x101 numeric matrix: table of marginal values for the stored energy, which depends
#'   on the date (365 days) and on the reservoir level (101 round percentage values ranging from
#'   0% to 100%). OR a 3-column matrix with 365x101 rows. In this latter case the 3 columns must
#'   be 'date', 'level' and 'value' (in this order), and the rows must be sorted by:
#'   ascending day, ascending level.
#' @param overwrite Logical. Overwrite the values if a file already exists.
#' @param opts List of simulation parameters returned by the function
#'   [antaresRead::setSimulationPath()].
#'
#' @export
#'
#' @importFrom antaresRead simOptions
#' @importFrom assertthat assert_that
#' @importFrom data.table dcast data.table
#' @importFrom utils write.table
#'
#' @examples
#' \dontrun{
#'
#' writeValues("fictive_area", data = matrix(rep(0, 365*101), nrow = 365))
#'
#' }


writeValues <- function(area,
                        data = NULL, 
                        type=c("waterValues ","reservoirLevels", "maxpower", "inflowPattern", "creditModulations"),
                        overwrite = TRUE,
                        opts = antaresRead::simOptions()) {
  type <- match.arg(type)
  
  assertthat::assert_that(inherits(opts, "simOptions"))

#Case waterValues    
  if (type=="waterValues")
    if (!(identical(dim(data), c(365L, 101L)) || identical(dim(data), c(36865L, 3L))))
      stop("waterValues 'data' must be either a 365*101 or (365*101)*3 matrix.", call. = FALSE)
    
    if (ncol(data) == 3) {
      data <- data.table::data.table(data)
      names(data) <- c("date", "level", "value")
      data <- data.table::dcast(data, formula = date ~ level, value.var = "value")
      data$date <- NULL
    }
    # API block
    if (is_api_study(opts)) {
      
      cmd <- api_command_generate(
        action = "replace_matrix",
        target = sprintf("input/hydro/common/capacity/waterValues_%s", area),
        matrix = data
      )
      api_command_register(cmd, opts = opts)
      `if`(
        should_command_be_executed(opts), 
        api_command_execute(cmd, opts = opts, text_alert = "Write water values: {msg_api}"),
        cli_command_registered("replace_matrix")
      )
      
      return(invisible(opts))
    }
  
#Case reservoirLevels    
  if (type=="reservoirLevels")
    if (!(identical(dim(data), c(365L, 3L))))
      stop("reservoirLevels 'data' must be a 365*3 matrix.", call. = FALSE)
  
    if (ncol(data) == 3) {
      data <- data.table::data.table(data)
      names(data) <- c("min", "average", "max")
      data <- data.table::dcast(data, formula = date ~ level, value.var = "value")
      data$date <- NULL
    }
    # API block
    if (is_api_study(opts)) {
      
      cmd <- api_command_generate(
        action = "replace_matrix",
        target = sprintf("input/hydro/common/capacity/reservoirLevels_%s", area),
        matrix = data
      )
      api_command_register(cmd, opts = opts)
      `if`(
        should_command_be_executed(opts), 
        api_command_execute(cmd, opts = opts, text_alert = "Write reservoir levels values: {msg_api}"),
        cli_command_registered("replace_matrix")
      )
      
      return(invisible(opts))
    }
  
#Case maxpower    
  if (type=="maxpower")
    if (!(identical(dim(data), c(365L, 4L))))
      stop("maxpower 'data' must be a 365*4 matrix.", call. = FALSE)
  
    # API block
    if (is_api_study(opts)) {
      
      cmd <- api_command_generate(
        action = "replace_matrix",
        target = sprintf("input/hydro/common/capacity/maxpower_%s", area),
        matrix = data
      )
      api_command_register(cmd, opts = opts)
      `if`(
        should_command_be_executed(opts), 
        api_command_execute(cmd, opts = opts, text_alert = "Write maxpower values: {msg_api}"),
        cli_command_registered("replace_matrix")
      )
      
      return(invisible(opts))
    } 

#Case inflowPattern    
  if (type=="inflowPattern")
    if (!(identical(dim(data), c(365L, 1L))))
      stop("inflowPattern 'data' must be a 365*1 matrix.", call. = FALSE)

    # API block
    if (is_api_study(opts)) {
      
      cmd <- api_command_generate(
        action = "replace_matrix",
        target = sprintf("input/hydro/common/capacity/inflowPattern_%s", area),
        matrix = data
      )
      api_command_register(cmd, opts = opts)
      `if`(
        should_command_be_executed(opts), 
        api_command_execute(cmd, opts = opts, text_alert = "Write inflowPattern values: {msg_api}"),
        cli_command_registered("replace_matrix")
      )
      
      return(invisible(opts))
    }
    
#Case creditModulations    
  if (type=="creditModulations")
    if (!(identical(dim(data), c(2L, 101L))))
      stop("creditModulations 'data' must be a 2*101 matrix.", call. = FALSE)
  
    # API block
    if (is_api_study(opts)) {
      
      cmd <- api_command_generate(
        action = "replace_matrix",
        target = sprintf("input/hydro/common/capacity/creditModulations_%s", area),
        matrix = data
      )
      api_command_register(cmd, opts = opts)
      `if`(
        should_command_be_executed(opts), 
        api_command_execute(cmd, opts = opts, text_alert = "Write creditModulations values: {msg_api}"),
        cli_command_registered("replace_matrix")
      )
      
      return(invisible(opts))
    }  
  

  # Input path
  inputPath <- opts$inputPath
  assertthat::assert_that(!is.null(inputPath) && file.exists(inputPath))
  
  check_area_name(area, opts)
  
  values_file <- file.path(inputPath, "hydro", "common", "capacity", paste0(paste(type,"_",sep = ""), tolower(area), ".txt"))
  
  if (isTRUE(file.size(values_file) > 0) && !overwrite)
    stop(paste(type,"values already exist for this area. Use overwrite=TRUE if you want to overwrite them."),
         call. = FALSE)
  
  utils::write.table(x = data, row.names = FALSE, col.names = FALSE, sep = "\t", file = values_file)
}
