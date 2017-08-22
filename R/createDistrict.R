#' Create a district
#' 
#' Allows selecting a set of areas so as to bundle them together in a "district".
#'
#' @param name District's name.
#' @param caption Caption for the district.
#' @param comments Comments for the district.
#' @param apply_filter Possible values are \code{add-all} to add all areas to the district,
#' \code{remove-all} to clear the district, or \code{none} (default) to don't apply a filter.
#' @param add_area Character vector of area(s) to add to the district.
#' @param remove_area Character vector of area(s) to remove from the district.
#' @param output Logical, compute the results for the district or not?
#' @param overwrite Logical, should the district be overwritten if already exist?
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#'
#' @export
#' @return An updated list containing various information about the simulation.
#'
#' @examples
#' \dontrun{
#' createDistrict(name = "mydistrict", 
#'                 apply_filter = "add-all", 
#'                 remove_area = c("fr", "be"))
#' }
createDistrict <- function(name, caption = NULL, comments = NULL, apply_filter = "none",
                           add_area = NULL, remove_area = NULL, output = FALSE, 
                           overwrite = FALSE, opts = antaresRead::simOptions()) {
  apply_filter <- match.arg(arg = apply_filter, choices = c("none", "add-all", "remove-all"))
  assertthat::assert_that(class(opts) == "simOptions")
  # Input path
  inputPath <- opts$inputPath
  assertthat::assert_that(!is.null(inputPath) && file.exists(inputPath))
  
  if (name %in% antaresRead::getDistricts() & !overwrite)
    stop(paste("District", name, "already exist!"))
  
  if (length(setdiff(add_area, antaresRead::getAreas())) != 0)
    stop("Invalid area in 'add_area'")
  
  if (length(setdiff(remove_area, antaresRead::getAreas())) != 0)
    stop("Invalid area in 'remove_area'")
  
  
  dropNulls <- function (x) {
    x[!vapply(x, is.null, FUN.VALUE = logical(1))]
  }
  
  new_district <- list(
    caption = caption,
    comments = comments,
    `apply-filter` = if (apply_filter != "none") apply_filter else NULL,
    # `+` = if (!is.null(add_area)) paste(add_area, collapse = ", ") else NULL,
    # `-` = if (!is.null(remove_area)) paste(remove_area, collapse = ", ") else NULL,
    output = output
  )
  
  new_district <- c(
    new_district,
    setNames(as.list(add_area), rep_len("+", length(add_area))),
    setNames(as.list(remove_area), rep_len("-", length(remove_area)))
  )
  
  # Read previous sets
  sets_path <- file.path(inputPath, "areas", "sets.ini")
  if (file.exists(sets_path)) {
    sets_params <- readIniFile(file = sets_path)
    sets_names <- names(sets_params)
    sets_params[[name]] <- dropNulls(new_district)
  } else {
    sets_params <- list()
    sets_params[[name]] <- dropNulls(new_district)
  }
  writeIni(listData = sets_params, pathIni = sets_path, overwrite = TRUE)
  
  # Maj simulation
  suppressWarnings({
    res <- antaresRead::setSimulationPath(path = opts$studyPath, simulation = "input")
  })
  
  invisible(res)
}
