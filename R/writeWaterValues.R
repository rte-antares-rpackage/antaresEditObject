#' Write water values
#'
#' @param area The area where to add the water values.
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
#' writeWaterValues("fictive_area", data = matrix(rep(0, 365*101), nrow = 365))
#'
#' }
writeWaterValues <- function(area,
                             data = NULL, 
                             overwrite = TRUE,
                             opts = antaresRead::simOptions()) {

  assertthat::assert_that(inherits(opts, "simOptions"))

  # Input path
  inputPath <- opts$inputPath
  assertthat::assert_that(!is.null(inputPath) && file.exists(inputPath))

  check_area_name(area, opts)
  
  values_file <- file.path(inputPath, "hydro", "common", "capacity", paste0("waterValues_", tolower(area), ".txt"))
  
  if (isTRUE(file.size(values_file) > 0) && !overwrite)
    stop("Water values already exist for this area. Use overwrite=TRUE if you want to overwrite them.",
         call. = FALSE)

  if (!(identical(dim(data), c(365L, 101L)) || identical(dim(data), c(36865L, 3L))))
    stop("'data' must be either a 365*101 or (365*101)*3 matrix.", call. = FALSE)

  if (ncol(data) == 3) {
    data <- data.table::data.table(data)
    names(data) <- c("date", "level", "value")
    data <- data.table::dcast(data, formula = date ~ level, value.var = "value")
    data$date <- NULL
  }

  utils::write.table(x = data, row.names = FALSE, col.names = FALSE, sep = "\t", file = values_file)
}
