#' @title Update input parameters of an Antares study
#' 
#' @description 
#' `r antaresEditObject:::badge_api_ok()`
#' 
#' Update input parameters of an Antares study
#' 
#'
#' @param import Series to import.
#' 
#' @template opts
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' updateInputSettings(import = c("thermal"))
#' updateInputSettings(import = c("hydro", "thermal"))
#' 
#' }
updateInputSettings <- function(import, opts = antaresRead::simOptions()) {
  assertthat::assert_that(inherits(opts, "simOptions"))
  
  if ("renewables" %in% import) {
    warning("import parameter cannot be 'renewables', it will be discarded.")
    import <- setdiff(import, "renewables")
  }
  
  if ("links" %in% import) {
    warning("import parameter cannot be 'renewables', it will be discarded.")
    import <- setdiff(import, "links")
  }
  
  # API block
  if (is_api_study(opts)) {
    
    writeIni(
      listData = list(import = paste(import, collapse = ", ")),
      pathIni = "settings/generaldata/input",
      opts = opts
    )
    
    return(update_api_opts(opts))
  }
  
  # read
  generaldatapath <- file.path(opts$studyPath, "settings", "generaldata.ini")
  generaldata <- readIniFile(file = generaldatapath)
  
  l_input <- generaldata$input
  new_params <- list(
    import = paste(import, collapse = ", ")
  )
  
  l_input <- utils::modifyList(x = l_input, val = new_params)
  generaldata$input <- l_input
  
  # write
  writeIni(listData = generaldata, pathIni = generaldatapath, overwrite = TRUE)
  
  # Maj simulation
  suppressWarnings({
    res <- antaresRead::setSimulationPath(path = opts$studyPath, simulation = "input")
  })
  
  invisible(res)
}
