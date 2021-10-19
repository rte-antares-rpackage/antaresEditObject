
#' Create an empty Antares study
#'
#' @param path Path where to create study, it should be an empty directory,
#'  if it doesn't exist, it'll be created.
#' @param study_name Name of the study.
#' @param antares_version Antares number version.
#'
#' @return Result of [antaresRead::setSimulationPath()].
#' @export
#' 
#' @importFrom whisker whisker.render
#' @importFrom utils unzip
#' @importFrom antaresRead setSimulationPath
#'
#' @examples
#' \dontrun{
#' 
#' createStudy("path/to/simulation")
#' 
#' }
createStudy <- function(path, study_name = "my_study", antares_version = "8.1.0") {
  antares_version <- as.numeric_version(antares_version)
  if (!dir.exists(path)) {
    dir.create(path = path, recursive = TRUE)
  } else {
    if (length(list.files(path = path)) > 0) {
      path <- file.path(path, study_name)
      if (!dir.exists(path)) {
        dir.create(path = path, recursive = TRUE)
      }
    }
  }
  if (antares_version < as.numeric_version("6.5.0")) {
    file.copy(
      from = list.files(path = system.file("newStudy", package = "antaresEditObject"), full.names = TRUE),
      to = path, recursive = TRUE
    )
    to_delete <- list.files(path = path, pattern = "ANTARESEDITOBJECT_TODELETE", full.names = TRUE, recursive = TRUE)
    unlink(to_delete)
  } else if (antares_version < as.numeric_version("7.1.0")) {
    unzip(zipfile = system.file("template-antares/antares-study-v700.zip", package = "antaresEditObject"), exdir = path)
  } else if (antares_version < as.numeric_version("7.2.0")){
    unzip(zipfile = system.file("template-antares/antares-study-v710.zip", package = "antaresEditObject"), exdir = path)
  } else if (antares_version < as.numeric_version("8.0.0")){
    unzip(zipfile = system.file("template-antares/antares-study-v720.zip", package = "antaresEditObject"), exdir = path)
  } else {
    unzip(zipfile = system.file("template-antares/antares-study-v800.zip", package = "antaresEditObject"), exdir = path)
  }
  antares <- paste(readLines(con = file.path(path, "study.antares")), collapse = "\n")
  antares <- whisker::whisker.render(
    template = antares,
    data = list(
      version = gsub(pattern = ".", replacement = "", x = antares_version, fixed = TRUE),
      study_name = study_name,
      date_created = floor(as.numeric(Sys.time()))
    )
  )
  writeLines(text = antares, con = file.path(path, "study.antares"))
  desktop <- paste(readLines(con = file.path(path, "Desktop.ini")), collapse = "\n")
  desktop <- whisker::whisker.render(
    template = desktop,
    data = list(
      study_name = study_name
    )
  )
  writeLines(text = desktop, con = file.path(path, "Desktop.ini"))
  opts <- setSimulationPath(path = path)
  if (antares_version >= as.numeric_version("8.1.0")) {
    activateRES(opts = opts)
  }
  return(invisible(opts))
}










