#' @title Write input time series
#'
#' @description 
#' `r antaresEditObject::badge_api_ok()`
#' 
#' This function writes input time series in an Antares project.
#'
#' @param data A 8760*N matrix of hourly time series, except when `type` is
#'  `"hydroSTOR"`. In this latter case, `data` must either be 365*N
#'  (Antares v7) or 12*N (v6 and earlier).
#' @param type Serie to write: `"load"`, `"hydroROR"`, `"hydroSTOR"`,
#'  `"wind"` or `"solar"`.
#' @param area The area where to write the input time series.
#' @param link Link for which writing transmission capacities time series, 
#'  must like `"area01%area02"` or `c("area01", "area02")`.
#' @param overwrite Logical. Overwrite the values if a file already exists.
#' 
#' @template opts
#'
#' @export
#'
#' @importFrom antaresRead simOptions
#' @importFrom assertthat assert_that
#' @importFrom data.table fwrite
#'
#' @examples
#' \dontrun{
#'
#' # Write solar time series
#' writeInputTS(
#'   area = "fictive_area",
#'   type = "solar",
#'   data = matrix(rep(4, 8760*2), nrow = 8760)
#' )
#'
#' }
writeInputTS <- function(data, 
                         type = c("load", "hydroROR", "hydroSTOR", "wind", "solar", "tsLink"),
                         area = NULL, 
                         link = NULL,
                         overwrite = TRUE, 
                         opts = antaresRead::simOptions()) {
  
  type <- match.arg(type)
  
  assertthat::assert_that(inherits(opts, "simOptions"))
  
  if (is_api_study(opts)) {
    
    if (type %in% c("load", "wind", "solar")) {
      cmd <- api_command_generate(
        action = "replace_matrix",
        target = sprintf("input/%s/series/%s_%s", type, type, area),
        matrix = data
      )
    } else if (type == "hydroROR") {
      cmd <- api_command_generate(
        action = "replace_matrix",
        target = sprintf("input/hydro/series/%s/ror", area),
        matrix = data
      )
    } else if (type == "hydroSTOR") {
      cmd <- api_command_generate(
        action = "replace_matrix",
        target = sprintf("input/hydro/series/%s/mod", area),
        matrix = data
      )
    }
    api_command_register(cmd, opts = opts)
    `if`(
      should_command_be_executed(opts), 
      api_command_execute(cmd, opts = opts, text_alert = "Writing time-series: {msg_api}"),
      cli_command_registered("replace_matrix")
    )
    
    return(invisible(opts))
  }
  
  # Edit link time-series
  if (!is.null(link)) {
    stopifnot(
      "link must be a character, like 'area01%area02' or c('area01', 'area02')" = is.character(link)
    )
    if (length(link) == 1)
      link <- strsplit(x = link, split = "%")[[1]]
    
    stopifnot(
      "Invalid link specification, must be 'area01%area02' or c('area01', 'area02')" = length(link) == 2
    )
    
    editLink(from = link[1], to = link[2], tsLink = data)
    return(invisible(opts))
  }
  
  if (identical(type, "tsLink"))
    stop("type = \"tsLink\" can only be used if link argument is provided")
  
  check_area_name(area, opts)
  
  inputPath <- opts$inputPath
  assertthat::assert_that(!is.null(inputPath) && file.exists(inputPath))
  
  if (type %in% c("load", "wind", "solar")) {
    values_file <- file.path(inputPath, type, "series", paste0(type, "_", tolower(area), ".txt"))
  } else if (type == "hydroROR") {
    values_file <- file.path(inputPath, "hydro", "series", area, "ror.txt")
  } else if (type == "hydroSTOR") {
    values_file <- file.path(inputPath, "hydro", "series", area, "mod.txt")
  }
  
  if (isTRUE(file.size(values_file) > 0) && !overwrite)
    stop("Time series already exist for this area. Use overwrite=TRUE if you want to overwrite them.",
         call. = FALSE)
  
  if (type %in% c("load", "hydroROR", "wind", "solar")) {
    if (NROW(data) != 8760)
      stop("'data' must be a 8760*N matrix.", call. = FALSE)
  } else {
    if (is_antares_v7(opts)) {
      if (NROW(data) != 365)
        stop("'data' must be a 365*N matrix.", call. = FALSE)
    } else {
      if (NROW(data) != 12)
        stop("'data' must be a 12*N matrix.", call. = FALSE)
    }
  }
  
  fwrite(
    x = as.data.table(data),
    row.names = FALSE, 
    col.names = FALSE, 
    sep = "\t",
    file = values_file
  )
  
  # Maj simulation
  suppressWarnings({
    res <- antaresRead::setSimulationPath(path = opts$studyPath, simulation = "input")
  })
  
  invisible(res)
}
