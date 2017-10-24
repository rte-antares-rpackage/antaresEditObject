#' Remove a link between two ares
#'
#' @param from The first area from which to create a link
#' @param to The second one 
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#'
#' @return An updated list containing various information about the simulation.
#' @export
#'
#' @examples
#' \dontrun{
#' createLink(from = "myarea", to  = "myarea2")
#' removeLink(from = "myarea", to  = "myarea2")
#' }
removeLink <- function(from, to, opts = antaresRead::simOptions()) {
  
  assertthat::assert_that(class(opts) == "simOptions")
  
  # Input path
  inputPath <- opts$inputPath
  assertthat::assert_that(!is.null(inputPath) && file.exists(inputPath))
  
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
  
  link <- paste(from, to, sep = " - ")
  if (!link %in% as.character(antaresRead::getLinks())) {
    message("Link doesn't exist")
    return()
  }
  
  
  # Previous links
  prev_links <- readIniFile(
    file = file.path(inputPath, "links", from, "properties.ini")
  )
  prev_links[[to]] <- NULL
  writeIni(
    listData = prev_links,
    pathIni = file.path(inputPath, "links", from, "properties.ini"),
    overwrite = TRUE
  )
  
  # remove initialization data
  unlink(x = file.path(inputPath, "links", from, paste0(to, ".txt")), recursive = TRUE)
  
  # Maj simulation
  suppressWarnings({
    res <- antaresRead::setSimulationPath(path = opts$studyPath, simulation = "input")
  })
  
  invisible(res)
}