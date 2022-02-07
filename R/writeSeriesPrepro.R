#' @title Write prepro data
#' 
#' @description 
#' `r antaresEditObject::badge_api_no()`
#'
#' This function allows to write load, wind and solar prepro data. Using
#' `character(0)` allows to erase data (cf Examples).
#'
#' @param area The area where to write prepro data.
#' @param type Type of data to write : `"load"`, `"wind"` or `"solar"`.
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
#'   [antaresRead::setSimulationPath()].
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
#' # Erase daily profile data:
#' writeSeriesPrepro("fictive_area", type = "solar", daily_profile = character(0))
#'
#' }
writeSeriesPrepro <- function(area,
                              type = c("load", "wind", "solar"),
                              coefficients = NULL,
                              daily_profile = NULL,
                              translation = NULL,
                              conversion = NULL,
                              overwrite = TRUE,
                              opts = antaresRead::simOptions()) {
  
  type <- match.arg(type)
  
  assertthat::assert_that(inherits(opts, "simOptions"))
  api_not_implemented(opts)
  
  check_area_name(area, opts)
  
  inputPath <- opts$inputPath
  assertthat::assert_that(!is.null(inputPath) && file.exists(inputPath))
  
  area_folder <- file.path(inputPath, type, "prepro", tolower(area))
  
  coefficients_file <- file.path(area_folder, "data.txt")
  daily_profile_file <- file.path(area_folder, "k.txt")
  translation_file <- file.path(area_folder, "translation.txt")
  conversion_file <- file.path(area_folder, "conversion.txt")
  
  if ((isTRUE(file.size(coefficients_file) > 0) ||
       isTRUE(file.size(daily_profile_file) > 0) ||
       isTRUE(file.size(translation_file) > 0) ||
       isTRUE(file.size(conversion_file) > 0)) && !overwrite)
    stop("Coefficients already exist for this area. Use overwrite=TRUE if you want to overwrite them.",
         .call = FALSE)
  
  # Coefficients
  if (!is.null(coefficients)) {
    if (!identical(dim(coefficients), c(12L, 6L)) && !identical(coefficients, character(0)))
      stop("'coefficients' must be either a 12*6 matrix or character(0).", call. = FALSE)
    fwrite(
      x = as.data.table(coefficients), 
      row.names = FALSE, 
      col.names = FALSE,
      sep = "\t",
      file = coefficients_file
    )
  }
  
  # Daily profile
  if (!is.null(daily_profile)) {
    if (!identical(dim(daily_profile), c(24L, 12L)) && !identical(daily_profile, character(0)))
      stop("'daily_profile' must be either a 24*12 matrix or character(0).", call. = FALSE)
    fwrite(
      x = as.data.table(daily_profile), 
      row.names = FALSE,
      col.names = FALSE,
      sep = "\t",
      file = daily_profile_file
    )
  }
  
  # Translation
  if (!is.null(translation)) {
    if (!(is.atomic(translation) && length(translation) == 8760) &&
        !identical(dim(translation), c(8760L, 1L)) &&
        !identical(translation, character(0)))
      stop("'translation' must be either a vector of length 8760, a 8760*1 matrix, or character(0).",
           call. = FALSE)
    fwrite(
      x = as.data.table(translation), 
      row.names = FALSE, 
      col.names = FALSE, 
      sep = "\t",
      file = translation_file
    )
  }
  
  # Conversion
  if (!is.null(conversion)) {
    if (NROW(conversion) != 2 && !identical(translation, character(0)))
      stop("'conversion' must be either a 2*N matrix or character(0).", call. = FALSE)
    fwrite(
      x = as.data.table(conversion),
      row.names = FALSE,
      col.names = FALSE, 
      sep = "\t",
      file = conversion_file
    )
  }
}
