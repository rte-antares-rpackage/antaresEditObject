#' Write input time series
#'
#' This function writes input time series in an Antares project.
#'
#' @param area The area where to write the input time series.
#' @param type Serie to write: \code{"load"}, \code{"hydroROR"}, \code{"hydroSTOR"},
#'  \code{"wind"} or \code{"solar"}.
#' @param data A 8760*N matrix of hourly time series, except when \code{type} is
#'  \code{"hydroSTOR"}. In this latter case, \code{data} must either be 365*N
#'  (Antares v7) or 12*N (v6 and earlier).
#' @param overwrite Logical. Overwrite the values if a file already exists.
#' @param opts List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}.
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
#' writeInputTS("fictive_area", type = "solar", data = matrix(rep(4, 8760*2), nrow = 8760))
#'
#' }
writeInputTS <- function(area, type = c("load", "hydroROR", "hydroSTOR", "wind", "solar"),
                         data, overwrite = TRUE, opts = antaresRead::simOptions()) {
  
  type <- match.arg(type)
  
  assertthat::assert_that(class(opts) == "simOptions")
  
  if (!tolower(area) %in% opts$areaList)
    stop(paste(area, "is not a valid area"), call. = FALSE)
  
  inputPath <- opts$inputPath
  assertthat::assert_that(!is.null(inputPath) && file.exists(inputPath))
  
  if (type %in% c("load", "wind", "solar")) {
    values_file <- file.path(inputPath, type, "series", paste0(type, "_", tolower(area), ".txt"))
  }
  else if (type == "hydroROR") {
    values_file <- file.path(inputPath, "hydro", "series", area, "ror.txt")
  }
  else {
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
    x = as.data.table(data), row.names = FALSE, col.names = FALSE, sep = "\t",
    file = values_file
  )
}
