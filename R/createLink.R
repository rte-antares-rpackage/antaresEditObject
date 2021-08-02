#' Create a link between two areas
#'
#' @param from The first area from which to create a link
#' @param to The second one
#' @param propertiesLink a named list containing the link properties, e.g. hurdles-cost
#' or transmission-capacities for example. See \code{\link{propertiesLinkOptions}}.
#' @param dataLink For Antares v7, a matrix with eight column corresponding to : trans. capacity (direct)
#' trans. capacity (indirect), hurdles cost (direct), hurdles cost (indirect), impedances, loop flow,
#' PST min, PST max.
#' If \code{NULL} (default), a matrix whose rows are equal to \code{1, 1, 0, 0, 0, 0, 0, 0} is set. See Details
#' @param overwrite Logical, overwrite the previous between the two areas if exist
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#' 
#' @note In Antares, areas are sorted in alphabetical order to establish links between.
#' For example, link between "fr" and "be" will appear under "be". 
#' So the areas are sorted before creating the link between them, and \code{dataLink} is
#' rearranged to match the new order.
#' 
#' @details The eight times-series are:
#' 
#' * **NTC direct** : the upstream-to-downstream capacity, in MW
#' * **NTC indirect** : the downstream-to-upstream capacity, in MW
#' * **Hurdle cost direct** : an upstream-to-downstream transmission fee, in euro/MWh
#' * **Hurdle cost indirect** : a downstream-to-upstream transmission fee, in euro/MWh
#' * **Impedances** : virtual impedances that are used in economy simulations to give a physical meaning to raw outputs, when no binding constraints have been defined to enforce Kirchhoff's laws.
#' * **Loop flow** : amount of power flowing circularly though the grid when all "nodes" are perfectly balanced (no import and no export).
#' * **PST min** : lower bound of phase-shifting that can be reached by a PST installed on the link, if any.
#' * **PST max** : upper bound of phase-shifting that can be reached by a PST installed on the link, if any.
#' 
#' NB: For Antares v7 the eight columns must conform to above order. For Antares v6, only five columns are 
#' expected, and they must follow this other order: NTC direct, NTC indirect, Impedances, Hurdle cost direct,
#' Hurdle cost indirect.
#'
#' @return An updated list containing various information about the simulation.
#' @export
#' 
#' @importFrom assertthat assert_that
#' @importFrom stats setNames
#' @importFrom utils read.table write.table
#'
#' @examples
#' \dontrun{
#' 
#' library(antaresRead)
#' 
#' # Set simulation path
#' setSimulationPath(path = "PATH/TO/SIMULATION", simulation = "input")
#' 
#' # Create a link between two areas
#' createLink(from = "first_area", to  = "second_area")
#' 
#' }
createLink <- function(from,
                       to, 
                       propertiesLink = propertiesLinkOptions(), 
                       dataLink = NULL, 
                       overwrite = FALSE,
                       opts = antaresRead::simOptions()) {
  
  assertthat::assert_that(class(opts) == "simOptions")
  
  v7 <- is_antares_v7(opts)
  
  if (!is.null(dataLink)) {
    if (v7) {
      assertthat::assert_that(ncol(dataLink) == 8)
    } else {
      assertthat::assert_that(ncol(dataLink) == 5)
    }
  }
  
  # control areas name
  # can be with some upper case (list.txt)
  from <- tolower(from)
  to <- tolower(to)
  
  # areas' order
  areas <- c(from, to)
  if (!identical(areas, sort(areas))) {
    from <- areas[2]
    to <- areas[1]
  }
  
  # Input path
  inputPath <- opts$inputPath
  assertthat::assert_that(!is.null(inputPath) && file.exists(inputPath))
  
  if (!from %in% opts$areaList)
    stop(paste(from, "is not a valid area"))
  if (!to %in% opts$areaList)
    stop(paste(to, "is not a valid area"))
  
  
  # Previous links
  prev_links <- readIniFile(
    file = file.path(inputPath, "links", from, "properties.ini")
  )
  
  if (to %in% names(prev_links) & !overwrite)
    stop(paste("Link to", to, "already exist"))
  
  if (to %in% names(prev_links) & overwrite) {
    opts <- removeLink(from = from, to = to, opts = opts)
    prev_links <- readIniFile(
      file = file.path(inputPath, "links", from, "properties.ini")
    )
  }
  
  # propLink <- list(propertiesLink)
  
  prev_links[[to]] <- propertiesLink
  
  # Write INI file
  writeIni(
    listData = prev_links, # c(prev_links, stats::setNames(propLink, to)),
    pathIni = file.path(inputPath, "links", from, "properties.ini"),
    overwrite = TRUE
  )
  
  # initialization data
  if (is.null(dataLink)) {
    if (v7) {
      dataLink <- matrix(data = c(rep(1, 8760*2), rep(0, 8760*6)), ncol = 8)
    } else {
      dataLink <- matrix(data = c(rep(1, 8760*2), rep(0, 8760*3)), ncol = 5)
    }
  }
  
  if (!identical(areas, sort(areas))) {
    dataLink[, 1:2] <- dataLink[, 2:1]
    
    if (v7) {
      dataLink[, 3:4] <- dataLink[, 4:3]
    } else {
      dataLink[, 4:5] <- dataLink[, 5:4]
    }
  }
  
  utils::write.table(
    x = dataLink, row.names = FALSE, col.names = FALSE, sep = "\t",
    file = file.path(inputPath, "links", from, paste0(to, ".txt"))
  )
  
  # Maj simulation
  suppressWarnings({
    res <- antaresRead::setSimulationPath(path = opts$studyPath, simulation = "input")
  })
  
  invisible(res)
}



#' Properties for creating a link
#'
#' @param hurdles_cost Logical, which is used to state whether (linear)
#'  transmission fees should be taken into account or not in economy and adequacy simulations
#' @param transmission_capacities Character, one of \code{enabled}, \code{ignore} or \code{infinite}, which is used to state whether 
#' the capacities to consider are those indicated in 8760-hour arrays or 
#' if zero or infinite values should be used instead (actual values / set to zero / set to infinite)
#' @param asset_type Character, one of \code{ac}, \code{dc}, \code{gas}, \code{virt} or \code{other}. Used to
#'   state whether the link is either an AC component (subject to Kirchhoff’s laws), a DC component, 
#'   or another type of asset.
#' @param display_comments Logical
#' @param filter_synthesis Output synthesis
#' @param filter_year_by_year Output year-by-year
#'
#' @return A named list
#' @export
#'
#' @examples
#' \dontrun{
#' propertiesLinkOptions()
#' }
propertiesLinkOptions <- function(hurdles_cost = FALSE, 
                           transmission_capacities = "enabled",
                           asset_type = "ac",
                           display_comments = TRUE,
                           filter_synthesis = c("hourly", "daily", "weekly", "monthly", "annual"),
                           filter_year_by_year = c("hourly", "daily", "weekly", "monthly", "annual")) {
  list(
    `hurdles-cost` = hurdles_cost,
    `transmission-capacities` = transmission_capacities,
    `asset-type` = asset_type,
    `display-comments` = display_comments,
    `filter-synthesis` = filter_synthesis,
    `filter-year-by-year` = filter_year_by_year
  )
}

