#' @title Remove a link between two areas
#' 
#' @description 
#' `r antaresEditObject:::badge_api_ok()`
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
  
  from <- tolower(from)
  to <- tolower(to)
  
  # Area existence
  check_area_name(from, opts)
  check_area_name(to, opts)
  
  # areas' order
  areas <- c(from, to)
  if (!identical(areas, sort(areas))) {
    from <- areas[2]
    to <- areas[1]
  }
  
  # Link existence
  link <- paste(from, to, sep = " - ")
  if (!link %in% as.character(antaresRead::getLinks())) {
    message("Link doesn't exist")
    return()
  }
  
  # check if the link can be removed safely, i.e. the link is not referenced in a binding constraint
  bc_not_remove <- detect_pattern_in_binding_constraint(pattern = c(paste0(from, "%", to), paste0(to, "%", from)), opts = opts)
  if (!identical(bc_not_remove, character(0))) {
    warning("The following binding constraints have the link to remove as a coefficient : ", paste0(bc_not_remove, collapse = ", "))
  }
  
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
  
  inputlinksfromPath <- file.path(inputPath, "links", from)
  
  # Previous links
  propertiesPath <- file.path(inputlinksfromPath, "properties.ini")
  prev_links <- readIniFile(
    file = propertiesPath
  )
  prev_links[[to]] <- NULL
  writeIni(
    listData = prev_links,
    pathIni = propertiesPath,
    overwrite = TRUE
  )
  
  # Remove files
  if (is_antares_v820(opts)) {
    both_direction <- c("_direct.txt", "_indirect.txt")
    files_to_remove <- c(file.path(inputlinksfromPath, "capacities", paste0(to, both_direction)),
                         file.path(inputlinksfromPath, paste0(to, "_parameters.txt"))
                        )
  } else {
    files_to_remove <- c(file.path(inputlinksfromPath, paste0(to, ".txt")))
  }
  lapply(files_to_remove, unlink)
  
  # Maj simulation
  suppressWarnings({
    res <- antaresRead::setSimulationPath(path = opts$studyPath, simulation = "input")
  })
  
  invisible(res)
}
