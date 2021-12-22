#' @title Update input parameters of an Antares study
#' 
#' @description 
#' `r antaresEditObject::badge_api_ok()`
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
  
  # API block
  if (is_api_study(opts)) {
    
    cmd <- api_command_generate(
      action = "update_config",
      target = "settings/generaldata/input",
      data = list(import = paste(import, collapse = ", "))
    )
    api_command_register(cmd, opts = opts)
    `if`(
      should_command_be_executed(opts), 
      api_command_execute(cmd, opts = opts, text_alert = "Updating input settings: {msg_api}"),
      cli_command_registered("update_config")
    )
    
    return(invisible(opts))
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
