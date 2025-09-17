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
  
  is_830 <- opts$antaresVersion >= 830
  nodal_by_targets <- .split_nodalOptimization_by_target(nodalOptimization)
  nodalOptimization <- nodal_by_targets[["toIniOptimization"]]
  nodalThermal <- nodal_by_targets[["toIniAreas"]]
   
  # API block
  if (is_api_study(opts)) {
    
    .api_command_execute_edit_area(name = name, new_values = nodalOptimization, type = "nodalOptimization", opts = opts)
    .api_command_execute_edit_area(name = name, new_values = nodalThermal, type = "nodalThermal", opts = opts)
    .api_command_execute_edit_area(name = name, new_values = filtering, type = "filtering", opts = opts)
    if (is_830) {
      .api_command_execute_edit_area(name = name, new_values = adequacy, type = "adequacy", opts = opts)
    } 
    
    .api_command_execute_edit_area_ui(name = name, color = color, localization = localization, opts = opts)
    
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
    for (property in names(nodalOptimization)) {
      infoIni$`nodal optimization`[[property]] <- nodalOptimization[[property]]
    }
  }
  
  if (!is.null(filtering)) {
    for (property in names(filtering)) {
      infoIni$filtering[[property]] <- filtering[[property]]
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
    for (property in names(nodalThermal)) {
      LnodalThermal[[property]][[name]] <- nodalThermal[[property]]
    }
    
    writeIni(listData = modifyList(x = thermal_areas, val = LnodalThermal), pathIni = thermal_areas_path, overwrite = TRUE)
  }
  
  # input/areas/<area>/adequacy_patch.ini
  if (is_830) {
    adequacy_area_path <- file.path(inputPath, "areas", name, "adequacy_patch.ini")
    adequacyIni <- readIniFile(file = adequacy_area_path)
    
    if (!is.null(adequacy)) {
      for (property in names(adequacy)) {
        adequacyIni$`adequacy-patch`[[property]] <- adequacy[[property]]
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


.generate_params_editArea <- function() {
  
  param_editArea <- list("nodalOptimization" = list("target" = "input/areas/%s/optimization/nodal optimization/%s",
                                                    "message" = "Update area's nodal optimization option: {msg_api}"
                                                    ),
                         "nodalThermal" = list("target" = "input/thermal/areas/%s/%s",
                                               "message" = "Update area's energy cost option: {msg_api}"
                                               ),
                         "filtering" = list("target" = "input/areas/%s/optimization/filtering/%s",
                                            "message" = "Update area's filtering option: {msg_api}"
                                            ),
                         "adequacy" = list("target" = "input/areas/%s/adequacy_patch/adequacy-patch/%s",
                                           "message" = "Update area's adequacy patch mode: {msg_api}"
                                           )
                         )

  return(param_editArea)
}



#' Edit area's parameters in API mode.
#'
#' @param name Name of the area to edit.
#' @param new_values Values of the parameters to edit.
#' @param type Type of edition.
#'
#' @template opts
#'
#' @importFrom assertthat assert_that
#'
.api_command_execute_edit_area <- function(name, new_values, type, opts) {
  
  assert_that(type %in% c("nodalOptimization", "nodalThermal", "filtering", "adequacy"))
  
  if (!is.null(new_values)) {
    params <- .generate_params_editArea()
    params_type <- params[[type]]
    
    actions <- lapply(
      X = seq_along(new_values),
      FUN = function(i) {
        property <- names(new_values)[i]
        if (type == "nodalThermal") {
          url_elements <- c(property, name)
        } else {
          url_elements <- c(name, property)
        }
        list(
          target = sprintf(params_type[["target"]], url_elements[1], url_elements[2]),
          data = new_values[[i]]
        )
      }
    )
    actions <- setNames(actions, rep("update_config", length(actions)))
    cmd <- do.call(api_commands_generate, actions)
    api_command_register(cmd, opts = opts)
    `if`(
      should_command_be_executed(opts),
      api_command_execute(cmd, opts = opts, text_alert = params_type[["message"]]),
      cli_command_registered("update_config")
    )
  }
}


#' Edit area's ui in API mode.
#'
#' @inheritParams editArea
#'
.api_command_execute_edit_area_ui <- function(name, color, localization, opts) {
  
  wo_localization <- is.null(localization)
  wo_color <- is.null(color)
  
  if (wo_localization || wo_color) {
    prev_ui <- readIni(pathIni = paste0("input/areas/",name,"/ui.ini"), opts = opts)
    if (wo_localization) {
      localization <- c(prev_ui[["ui"]][["x"]], prev_ui[["ui"]][["y"]])
    }
    if (wo_color) {
      color <- grDevices::rgb(prev_ui[["ui"]][["color_r"]], prev_ui[["ui"]][["color_g"]], prev_ui[["ui"]][["color_b"]], max = 255)
    }
  }
  
  ui <- .format_ui_data_by_mode(name = name, localization = localization, color = color, api_mode = TRUE)
  cmd <- api_command_generate(action = "update_area_ui", ui)
  api_command_register(command = cmd, opts = opts)
  `if`(
    should_command_be_executed(opts = opts), 
    api_command_execute(command = cmd, opts = opts, text_alert = "Update area's ui: "),
    cli_command_registered("update_area_ui")
  )
}
