#' Edit a link between two areas
#'
#' @param from The first area from which to create a link
#' @param to The second one
#' @param hurdles_cost Logical, which is used to state whether (linear)
#'  transmission fees should be taken into account or not in economy and adequacy simulations
#' @param transmission_capacities Character, one of \code{enabled}, \code{ignore} or \code{infinite}, which is used to state whether 
#' the capacities to consider are those indicated in 8760-hour arrays or 
#' if zero or infinite values should be used instead (actual values / set to zero / set to infinite)
#' @param display_comments Logical
#' @param filter_synthesis Output synthesis
#' @param filter_year_by_year Output year-by-year
#' @param dataLink a matrix with five column corresponding to : trans. capacity (direct)
#' trans. capacity (indirect), impedances, hurdles cost (direct), hurdles cost (indirect).
#' If \code{NULL} (default), a matrix whose rows are equal to \code{1, 1, 0, 0, 0} is set. See Details
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#' 
#' @note In Antares, areas are sorted in alphabetical order to establish links between.
#' For example, link between "fr" and "be" will appear under "be". 
#' So the areas are sorted before creating the link between them, and \code{dataLink} is
#' rearranged to match the new order.
#' 
#' @details The five times-series are:
#' \itemize{
#'  \item{"NTC direct"}{the upstream-to-downstream capacity, in MW}
#'  \item{"NTC indirect"}{the downstream-to-upstream capacity, in MW}
#'  \item{"Impedances"}{virtual impedances that are used in economy simulations to give a physical meaning to raw outputs, when no binding constraints have been defined to enforce Kirchhoff's laws.}
#'  \item{"Hurdle cost direct"}{an upstream-to-downstream transmission fee, in euro/MWh}
#'  \item{"Hurdle cost indirect"}{a downstream-to-upstream transmission fee, in euro/MWh}
#' }
#'
#' @return An updated list containing various information about the simulation.
#' @export
#' 
#' @importFrom assertthat assert_that
#' @importFrom stats setNames 
#' @importFrom utils read.table write.table modifyList
#'
#' @examples
#' \dontrun{
#' editLink(
#'   from = "area1",
#'   to = "area2",
#'   transmission_capacities = "infinite"
#' )
#' }
editLink <- function(from, to, hurdles_cost = NULL, 
                     transmission_capacities = NULL, 
                     display_comments = NULL,
                     filter_synthesis = NULL,
                     filter_year_by_year = NULL, 
                     dataLink = NULL, opts = antaresRead::simOptions()) {
  
  assertthat::assert_that(class(opts) == "simOptions")
  if (!is.null(dataLink)) 
    assertthat::assert_that(ncol(dataLink) == 5)
  
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
  

  propertiesLink <- dropNulls(list(
    `hurdles-cost` = hurdles_cost,
    `transmission-capacities` = transmission_capacities,
    `display-comments` = display_comments,
    `filter-synthesis` = filter_synthesis,
    `filter-year-by-year` = filter_year_by_year
  ))
  
  prev_links[[to]] <- modifyList(x = prev_links[[to]], val = propertiesLink)
  
  # Write INI file
  writeIni(
    listData = prev_links, 
    pathIni = file.path(inputPath, "links", from, "properties.ini"),
    overwrite = TRUE
  )
  
  # edit data
  if (!is.null(dataLink)) {
    if (!identical(areas, sort(areas))) {
      dataLink <- dataLink[, c(2, 1, 3, 5, 4)]
    }
    
    utils::write.table(
      x = dataLink, row.names = FALSE, col.names = FALSE, sep = "\t",
      file = file.path(inputPath, "links", from, paste0(to, ".txt"))
    )
  }
  
  # Maj simulation
  suppressWarnings({
    res <- antaresRead::setSimulationPath(path = opts$studyPath, simulation = "input")
  })
  
  invisible(res)
}



