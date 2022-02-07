#' @title Remove a link between two areas
#' 
#' @description 
#' `r antaresEditObject::badge_api_ok()`
#' 
#' Remove a link between two areas in an Antares study.
#' 
#'
#' @inheritParams createLink
#' 
#' @template opts
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' createLink(from = "myarea", to  = "myarea2")
#' removeLink(from = "myarea", to  = "myarea2")
#' }
removeLink <- function(from, to, opts = antaresRead::simOptions()) {
  
  assertthat::assert_that(inherits(opts, "simOptions"))
  
  # API block
  if (is_api_study(opts)) {
    cmd <- api_command_generate(
      action = "remove_link",
      area1 = from,
      area2 = to
    )
    api_command_register(cmd, opts = opts)
    `if`(
      should_command_be_executed(opts), 
      api_command_execute(cmd, opts = opts, text_alert = "{.emph remove_link}: {msg_api}"),
      cli_command_registered("remove_link")
    )
    
    return(invisible(opts))
  }
  
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
