#' Write prepro data
#'
#' This function allows to write load, wind and solar prepro data.
#'
#' @param area The area where to write prepro data.
#' @param type "load", "wind" or "solar".
#' @param coefficients A 12*6 matrix of monthly values for the primary
#'   parameters alpha, beta, gamma, delta, theta and mu.
#' @param daily_profile A 24*12 matrix of hourly / monthly coefficients K(hm)
#'   that are used to modulate the values of the stationary stochastic process
#'   by which the actual process is approximated.
#' @param translation A vector of length 8760 (or 8760*1 matrix) to add to the
#'   time-series generated, prior or after scaling.
#' @param conversion A 2*N matrix (with 1<=N<=50) that is used to turn the
#'   initial time-series produced by the generators into final data. See Antares
#'   General Reference Guide.
#' @param overwrite Logical. Overwrite the values if a file already exists.
#' @param opts List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}.
#'
#' @export
#'
#' @importFrom antaresRead simOptions
#' @importFrom assertthat assert_that
#' @importFrom data.table fread fwrite
#'
#' @examples
#' \dontrun{
#'
#' writeSeriesPrepro("fictive_area", type = "solar", daily_profile = matrix(rep(1, 24*12), nrow = 24))
#'
#' }
writeSeriesPrepro <- function(
  area,
  type = c("load", "wind", "solar"),
  coefficients = NULL,
  daily_profile = NULL,
  translation = NULL,
  conversion = NULL,
  overwrite = TRUE,
  opts = antaresRead::simOptions()
) {
  
  assertthat::assert_that(class(opts) == "simOptions")
  
  if (!tolower(area) %in% opts$areaList)
    stop(paste(area, "is not a valid area"), call. = FALSE)
  
  inputPath <- opts$inputPath
  assertthat::assert_that(!is.null(inputPath) && file.exists(inputPath))
  
  area_folder <- file.path(inputPath, type, "prepro", tolower(area))
  
  coefficients_file <- file.path(area_folder, "data.txt")
  daily_profile_file <- file.path(area_folder, "k.txt")
  translation_file <- file.path(area_folder, "translation.txt")
  conversion_file <- file.path(area_folder, "conversion.txt")
  
  if ((isTRUE(file.size(coefficients_file)) > 0 ||
       isTRUE(file.size(daily_profile_file)) > 0 ||
       isTRUE(file.size(translation_file)) > 0 ||
       isTRUE(file.size(conversion_file)) > 0) && !overwrite)
    stop("Coefficients already exist for this area. Use overwrite=TRUE if you want to overwrite them.",
         .call = FALSE)
  
  # Coefficients
  if (!is.null(coefficients)) {
    if (!identical(dim(coefficients), c(12L, 6L)))
      stop("'coefficients' must be a 12*6 matrix.", call. = FALSE)
    fwrite(
      x = as.data.table(coefficients), row.names = FALSE, col.names = FALSE, sep = "\t",
      file = coefficients_file
    )
  }
  
  # Daily profile
  if (!is.null(daily_profile)) {
    if (!identical(dim(daily_profile), c(24L, 12L)))
      stop("'daily_profile' must be a 24*12 matrix.", call. = FALSE)
    fwrite(
      x = as.data.table(daily_profile), row.names = FALSE, col.names = FALSE, sep = "\t",
      file = daily_profile_file
    )
  }
  
  # Translation
  if (!is.null(translation)) {
    if (!(is.atomic(translation) && length(translation) == 8760) &&
        !identical(dim(daily_profile), c(8760L, 1L)))
      stop("'translation' must be either a vector of length 8760 or a 8760*1 matrix.", call. = FALSE)
    fwrite(
      x = as.data.table(translation), row.names = FALSE, col.names = FALSE, sep = "\t",
      file = translation_file
    )
  }
  
  # Conversion
  if (!is.null(conversion)) {
    if (NROW(conversion) != 2)
      stop("'conversion' must be a 2*N matrix.", call. = FALSE)
    fwrite(
      x = as.data.table(conversion), row.names = FALSE, col.names = FALSE, sep = "\t",
      file = conversion_file
    )
  }
}
