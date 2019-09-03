#' Write input time series
#'
#' This function writes input time series in an Antares project.
#'
#' @param area The area where to write the input time series.
#' @param type Serie to write: \code{"load"}, \code{"thermal"}, \code{"hydro"},
#'  \code{"wind"} or \code{"solar"}.
#' @param data A 8760*N matrix of hourly time series.
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
#' writeInputTS("fictive_area", type = "thermal", data = matrix(rep(4, 8760*2), nrow = 8760))
#'
#' }
writeInputTS <- function(area, type = c("load", "thermal", "hydro", "wind", "solar"),
                         data, overwrite = TRUE, opts = antaresRead::simOptions()) {
  
  type <- match.arg(type)
  
  assertthat::assert_that(class(opts) == "simOptions")
  
  if (!tolower(area) %in% opts$areaList)
    stop(paste(area, "is not a valid area"), call. = FALSE)
  
  inputPath <- opts$inputPath
  assertthat::assert_that(!is.null(inputPath) && file.exists(inputPath))
  
  values_file <- file.path(inputPath, type, "series", paste0(type, "_", tolower(area), ".txt"))
  
  if (isTRUE(file.size(values_file) > 0) && !overwrite)
    stop("Time series already exist for this area. Use overwrite=TRUE if you want to overwrite them.",
         call. = FALSE)
  
  if (NROW(data) != 8760)
    stop("'data' must be a 8760*N matrix.", call. = FALSE)
  
  fwrite(
    x = as.data.table(data), row.names = FALSE, col.names = FALSE, sep = "\t",
    file = values_file
  )
}
