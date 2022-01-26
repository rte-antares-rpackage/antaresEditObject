
#' Is study an Antares v7 study ?
#'
#' @param opts
#'   List of simulation parameters returned by the function
#'   [antaresRead::setSimulationPath()]
#'
#' @return a logical, `TRUE` if study is v7 or above, `FALSE` otherwise
#' @export
#'
#' @examples
#' 
#' \dontrun{
#' # setSimulationPath
#' 
#' is_antares_v7()
#' 
#' }
#' 
is_antares_v7 <- function(opts = antaresRead::simOptions()) {
  assertthat::assert_that(inherits(opts, "simOptions"))
  new_version <- getOption("antares.version.new", default = 700)
  new_version <- as.numeric(new_version)
  isTRUE(opts$antaresVersion >= new_version)
}
