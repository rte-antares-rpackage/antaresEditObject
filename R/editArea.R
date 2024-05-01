#' @title Edit an area in an Antares study
#' 
#' @description 
#' `r antaresEditObject:::badge_api_ok()`
#' 
#' Edit an existing area in an Antares study.
#'
#' @inheritParams createArea
#' 
#' @template opts
#' 
#' @seealso [createArea()], [removeArea()]
#' 
#' @export
#' 
#' @importFrom antaresRead simOptions setSimulationPath readIniFile
#' @importFrom utils modifyList
#' @importFrom assertthat assert_that
#' @importFrom grDevices col2rgb
#'
#' @examples
#' \dontrun{
#' 
#' library(antaresRead)
#' 
#' # Set simulation path
#' setSimulationPath(path = "PATH/TO/SIMULATION", simulation = "input")
#' 
#' # Edit an existing area
#' editArea("area", color = grDevices::rgb(230, 108, 44, max = 255),
#'   localization = c(1, 1),
#'   opts = antaresRead::simOptions()) 
#' 
#' editArea("de",  nodalOptimization = list("spilledenergycost" = list(fr = 30)),
#' opts = antaresRead::simOptions())
#' 
#' editArea("de",  nodalOptimization = nodalOptimizationOptions(),
#' opts = antaresRead::simOptions())
#' 
#' editArea(
#'   "de",
#'   filtering = list("filter_synthesis"=paste(c("hourly","daily"),collapse = ", "))
#' )
#' 
#' }
editArea <- function(name, 
                     color = NULL,
                     localization = NULL,
                     nodalOptimization = NULL,
                     filtering = NULL,
                     adequacy = NULL,
                     opts = antaresRead::simOptions()) {
  
  assert_that(inherits(opts, "simOptions"))
  validate_area_name(name)
  # name of the area can contain upper case in areas/list.txt (and use in graphics)
  # (and use in graphics) but not in the folder name (and use in all other case)
  name <- tolower(name)
  
  check_area_name(name, opts)
  
  nodalOptimization_ori <- nodalOptimization
  is_830 <- opts$antaresVersion >= 830
  nodal_by_targets <- .split_nodalOptimization_by_target(nodalOptimization)
  nodalOptimization <- nodal_by_targets[["toIniOptimization"]]
  nodalThermal <- nodal_by_targets[["toIniAreas"]]
  
  not_null_filtering <- !is.null(filtering)
  not_null_adequacy <- !is.null(adequacy)
   
  # API block
  if (is_api_study(opts)) {
    
    if (!is.null(nodalOptimization_ori)) {
      cmd <- api_command_generate(
        action = "update_config", 
        target = sprintf("input/areas/%s/optimization/nodal optimization", name),
        data = nodalOptimization_ori
      )
      api_command_register(cmd, opts = opts)
      `if`(
        should_command_be_executed(opts), 
        api_command_execute(cmd, opts = opts, text_alert = "Update area's nodal optimization option: {msg_api}"),
        cli_command_registered("update_config")
      )
    }
    
    if (not_null_filtering) {
      cmd <- api_command_generate(
        action = "update_config", 
        target = sprintf("input/areas/%s/optimization/filtering", name),
        data = filtering
      )
      api_command_register(cmd, opts = opts)
      `if`(
        should_command_be_executed(opts), 
        api_command_execute(cmd, opts = opts, text_alert = "Update area's filtering option: {msg_api}"),
        cli_command_registered("update_config")
      )
    }
    
    if (is_830){
      if (not_null_adequacy) {
        cmd <- api_command_generate(
          action = "update_config", 
          target = sprintf("input/areas/%s/adequacy_patch/adequacy-patch", name),
          data = adequacy
        )
        api_command_register(cmd, opts = opts)
        `if`(
          should_command_be_executed(opts), 
          api_command_execute(cmd, opts = opts, text_alert = "Update area's adequacy patch mode: {msg_api}"),
          cli_command_registered("update_config")
        )
      }
    }
    
    return(invisible(opts))
  }
  
  if (opts$mode != "Input") 
    stop("You can initialize an area only in 'Input' mode")
  
  # Input path
  inputPath <- opts$inputPath
  assert_that(!is.null(inputPath) && file.exists(inputPath))
  
  # input/areas/<area>/optimization.ini
  optimization_area_path <- file.path(inputPath, "areas", name, "optimization.ini")
  infoIni <- readIniFile(file = optimization_area_path)
  
  if (!is.null(nodalOptimization)) {
    for (i in names(nodalOptimization)) {
      infoIni$`nodal optimization`[[i]] <- nodalOptimization[[i]]
    }
  }
  
  if (not_null_filtering) {
    for (i in names(filtering)) {
      infoIni$filtering[[i]] <- filtering[[i]]
    }
  }
  
  writeIni(listData = infoIni, pathIni = optimization_area_path, overwrite = TRUE)
  
  # input/areas/<area>/ui.ini
  ui_area_path <- file.path(inputPath, "areas", name, "ui.ini")
  color_loc_ini <- readIniFile(file = ui_area_path)
  
  if (!is.null(localization)) {
    localization <- as.character(localization)
    color_loc_ini$ui$x <- localization[1]
    color_loc_ini$ui$y <- localization[2]
    color_loc_ini$layerX$`0` <- localization[1]
    color_loc_ini$layerY$`0` <- localization[2]
  }
  
  if (!is.null(color)) {
    color_loc_ini$ui$color_r <- unname(grDevices::col2rgb(color)["red", 1])
    color_loc_ini$ui$color_g <- unname(grDevices::col2rgb(color)["green", 1])
    color_loc_ini$ui$color_b <- unname(grDevices::col2rgb(color)["blue", 1])
    color_loc_ini$layerColor = list(`0` = as.vector(grDevices::col2rgb(color)))
  }
  
  writeIni(listData = color_loc_ini, pathIni = ui_area_path, overwrite = TRUE)
  
  # input/thermal/areas.ini
  if (!is.null(nodalThermal)) {
    
    thermal_areas_path <- file.path(inputPath, "thermal", "areas.ini")
    assert_that(file.exists(thermal_areas_path), msg = "File input/thermal/areas.ini does not exist.")
    thermal_areas <- readIniFile(file = thermal_areas_path)
    
    LnodalThermal <- list()
    for (economic_option in names(nodalThermal)) {
      LnodalThermal[[economic_option]][[name]] <- nodalThermal[[economic_option]]
    }
    
    writeIni(listData = modifyList(x = thermal_areas, val = LnodalThermal), pathIni = thermal_areas_path, overwrite = TRUE)
  }
  
  # input/areas/<area>/adequacy_patch.ini
  if (is_830) {
    adequacy_area_path <- file.path(inputPath, "areas", name, "adequacy_patch.ini")
    adequacyIni <- readIniFile(file = adequacy_area_path)
    
    if (not_null_adequacy) {
      for (i in names(adequacy)) {
        adequacyIni$`adequacy-patch`[[i]] <- adequacy[[i]]
      }
    }
    
    writeIni(listData = adequacyIni, pathIni = adequacy_area_path, overwrite = TRUE)
  }
  
  # Maj simulation
  suppressWarnings({
    res <- antaresRead::setSimulationPath(path = opts$studyPath, simulation = "input")
  })
  
  invisible(res)
}
