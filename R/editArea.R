#' @title Edit an area in an Antares study
#' 
#' @description 
#' `r antaresEditObject::badge_api_ok()`
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
#' @importFrom antaresRead simOptions setSimulationPath
#' @importFrom utils read.table write.table
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
#' }
editArea <- function(name, 
                     color = NULL,
                     localization = NULL,
                     nodalOptimization = NULL,
                     filtering = NULL,
                     opts = antaresRead::simOptions()) {
  
  assertthat::assert_that(inherits(opts, "simOptions"))
  validate_area_name(name)
  # name of the area can contain upper case in areas/list.txt (and use in graphics)
  # (and use in graphics) but not in the folder name (and use in all other case)
  list_name <- name
  name <- tolower(name)
  
  # API block
  if (is_api_study(opts)) {
    
    if (!is.null(nodalOptimization)) {
      cmd <- api_command_generate(
        action = "update_config", 
        target = sprintf("input/areas/%s/optimization/nodal optimization", name),
        data = nodalOptimization
      )
      api_command_register(cmd, opts = opts)
      `if`(
        should_command_be_executed(opts), 
        api_command_execute(cmd, opts = opts, text_alert = "Update area's nodal optimization option: {msg_api}"),
        cli_command_registered("update_config")
      )
    }
    
    if (!is.null(filtering)) {
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
    
    return(invisible(opts))
  }
  
  v7 <- is_antares_v7(opts)
  
  check_area_name(name, opts)
  
  if (opts$mode != "Input") 
    stop("You can initialize an area only in 'Input' mode")
  
  # Input path
  inputPath <- opts$inputPath
  assertthat::assert_that(!is.null(inputPath) && file.exists(inputPath))
  infoIni <- readIniFile(file.path(inputPath, "areas", name, "optimization.ini"))
  
  
  nodalOptimizationThermal <- nodalOptimization[names(nodalOptimization) %in% c("unserverdenergycost", "spilledenergycost")]
  nodalOptimization <- nodalOptimization[!names(nodalOptimization) %in% c("unserverdenergycost", "spilledenergycost")]
  if (!is.null(nodalOptimization)) {
    for (i in names(nodalOptimization)) {
      infoIni$`nodal optimization`[[i]] <- nodalOptimization[[i]]
    }
  }
  
  if (!is.null(filtering)) {
    for (i in names(filtering)) {
      infoIni$filtering[[i]] <- filtering[[i]]
    }
  }
  
  # optimization ini file
  writeIni(
    listData = infoIni ,
    pathIni = file.path(inputPath, "areas", name, "optimization.ini"),
    overwrite = TRUE
  )
  
  color_loc_ini <- readIniFile(file.path(inputPath, "areas", name, "ui.ini"))
  
  names(color_loc_ini)
  
  if (!is.null(localization)) {
    localization <- as.character(localization)
    color_loc_ini$ui$x <- localization[1]
    color_loc_ini$ui$y <- localization[2]
  }
  
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
  
  writeIni(
    listData = color_loc_ini,
    pathIni = file.path(inputPath, "areas", name, "ui.ini"),
    overwrite = TRUE
  )
  
  if (!is.null(nodalOptimizationThermal)) {
    
    
    thermal_areas_path <- file.path(inputPath, "thermal", "areas.ini")
    if (file.exists(thermal_areas_path)) {
      thermal_areas <- readIniFile(file = thermal_areas_path)
    } else {
      thermal_areas <- list()
    }
    thermal_areas$unserverdenergycost[[name]] <- nodalOptimizationThermal[["unserverdenergycost"]]
    thermal_areas$spilledenergycost[[name]] <- nodalOptimizationThermal[["spilledenergycost"]]
    writeIni(thermal_areas, thermal_areas_path, overwrite = TRUE)
    
  }
  
  # Maj simulation
  suppressWarnings({
    res <- antaresRead::setSimulationPath(path = opts$studyPath, simulation = "input")
  })
  
  invisible(res)
}

# ## Hydro ----
# 
# # capacity
# con <- file(description = file.path(inputPath, "hydro", "common", "capacity", paste0("maxpower_", name, ".txt")), open = "wt")
# writeLines(text = character(0), con = con)
# close(con)
# 
# reservoir <- matrix(data = rep(c(0, 0.5, 1), each = 12), ncol = 3)
# utils::write.table(
#   x = reservoir, row.names = FALSE, col.names = FALSE, sep = "\t",
#   file = file.path(inputPath, "hydro", "common", "capacity", paste0("reservoir_", name, ".txt"))
# )
# 
# if (v7) {
#   creditmodulations <- matrix(data = rep(1, 202), nrow = 2)
#   utils::write.table(
#     x = creditmodulations, row.names = FALSE, col.names = FALSE, sep = "\t",
#     file = file.path(inputPath, "hydro", "common", "capacity", paste0("creditmodulations_", name, ".txt"))
#   )
#   
#   inflowPattern <- matrix(data = rep(1, 365), ncol = 1)
#   utils::write.table(
#     x = inflowPattern, row.names = FALSE, col.names = FALSE, sep = "\t",
#     file = file.path(inputPath, "hydro", "common", "capacity", paste0("inflowPattern_", name, ".txt"))
#   )
#   
#   maxpower <- matrix(data = rep(c(0, 24, 0, 24), each = 365), ncol = 4)
#   utils::write.table(
#     x = maxpower, row.names = FALSE, col.names = FALSE, sep = "\t",
#     file = file.path(inputPath, "hydro", "common", "capacity", paste0("maxpower_", name, ".txt"))
#   )
#   
#   reservoir <- matrix(data = rep(c("0", "0.500", "1"), each = 365), ncol = 3)
#   utils::write.table(
#     x = reservoir, row.names = FALSE, col.names = FALSE, sep = "\t", quote = FALSE,
#     file = file.path(inputPath, "hydro", "common", "capacity", paste0("reservoir_", name, ".txt"))
#   )
#   
#   con <- file(description = file.path(inputPath, "hydro", "common", "capacity", paste0("waterValues_", name, ".txt")), open = "wt")
#   writeLines(text = character(0), con = con)
#   close(con)
# }
# 
# # prepro
# # dir
# dir.create(path = file.path(inputPath, "hydro", "prepro", name), showWarnings = FALSE)
# 
# con <- file(description = file.path(inputPath, "hydro", "prepro", name, "energy.txt"), open = "wt")
# writeLines(text = character(0), con = con)
# close(con)
# 
# writeIni(
#   listData = list(`prepro` = list(`intermonthly-correlation` = 0.5)),
#   pathIni = file.path(inputPath, "hydro", "prepro", name, "prepro.ini"),
#   overwrite = overwrite
# )
# 
# # series
# # dir
# dir.create(path = file.path(inputPath, "hydro", "series", name), showWarnings = FALSE)
# 
# con <- file(description = file.path(inputPath, "hydro", "series", name, "mod.txt"), open = "wt")
# writeLines(text = character(0), con = con)
# close(con)
# 
# con <- file(description = file.path(inputPath, "hydro", "series", name, "ror.txt"), open = "wt")
# writeLines(text = character(0), con = con)
# close(con)
# 
# 
# 
# ## Links ----
# # dir
# dir.create(path = file.path(inputPath, "links", name), showWarnings = FALSE)
# writeIni(
#   listData = list(),
#   pathIni = file.path(inputPath, "links", name, "properties.ini"),
#   overwrite = overwrite
# )
# 
# 
# 
# ## Load ----
# 
# # prepro
# # dir
# dir.create(path = file.path(inputPath, "load", "prepro", name), showWarnings = FALSE)
# 
# conversion <- matrix(data = c(-9999999980506447872,	0,	9999999980506447872, 0, 0, 0), nrow = 2, byrow = TRUE)
# utils::write.table(
#   x = conversion, row.names = FALSE, col.names = FALSE, sep = "\t",
#   file = file.path(inputPath, "load", "prepro", name, "conversion.txt")
# )
# 
# data <- matrix(data = c(rep(1, 2*12), rep(0, 12), rep(1, 3*12)), nrow = 12)
# utils::write.table(
#   x = data, row.names = FALSE, col.names = FALSE, sep = "\t",
#   file = file.path(inputPath, "load", "prepro", name, "data.txt")
# )
# 
# utils::write.table(
#   x = character(0), row.names = FALSE, col.names = FALSE, sep = "\t",
#   file = file.path(inputPath, "load", "prepro", name, "k.txt")
# )
# 
# writeIni(
#   listData = list(),
#   pathIni = file.path(inputPath, "load", "prepro", name, "settings.ini"),
#   overwrite = overwrite
# )
# 
# utils::write.table(
#   x = character(0), row.names = FALSE, col.names = FALSE, sep = "\t",
#   file = file.path(inputPath, "load", "prepro", name, "translation.txt")
# )
# 
# # series
# write.table(
#   x = character(0), row.names = FALSE, col.names = FALSE, sep = "\t",
#   file = file.path(inputPath, "load", "series", paste0("load_", name, ".txt"))
# )
# 
# 
# 
# ## Misc-gen ----
# utils::write.table(
#   x = character(0), row.names = FALSE, col.names = FALSE, sep = "\t",
#   file = file.path(inputPath, "misc-gen", paste0("miscgen-", name, ".txt"))
# )
# 
# 
# ## Reserves ----
# utils::write.table(
#   x = character(0), row.names = FALSE, col.names = FALSE, sep = "\t",
#   file = file.path(inputPath, "reserves", paste0(name, ".txt"))
# )
# 
# 
# ## Solar ----
# 
# # prepro
# # dir
# dir.create(path = file.path(inputPath, "solar", "prepro", name), showWarnings = FALSE)
# 
# conversion <- matrix(data = c(-9999999980506447872,	0,	9999999980506447872, 0, 0, 0), nrow = 2, byrow = TRUE)
# utils::write.table(
#   x = conversion, row.names = FALSE, col.names = FALSE, sep = "\t",
#   file = file.path(inputPath, "solar", "prepro", name, "conversion.txt")
# )
# 
# data <- matrix(data = c(rep(1, 2*12), rep(0, 12), rep(1, 3*12)), nrow = 12)
# utils::write.table(
#   x = data, row.names = FALSE, col.names = FALSE, sep = "\t",
#   file = file.path(inputPath, "solar", "prepro", name, "data.txt")
# )
# 
# write.table(
#   x = character(0), row.names = FALSE, col.names = FALSE, sep = "\t",
#   file = file.path(inputPath, "solar", "prepro", name, "k.txt")
# )
# 
# writeIni(
#   listData = list(),
#   pathIni = file.path(inputPath, "solar", "prepro", name, "settings.ini"),
#   overwrite = overwrite
# )
# 
# utils::write.table(
#   x = character(0), row.names = FALSE, col.names = FALSE, sep = "\t",
#   file = file.path(inputPath, "solar", "prepro", name, "translation.txt")
# )
# 
# # series
# utils::write.table(
#   x = character(0), row.names = FALSE, col.names = FALSE, sep = "\t",
#   file = file.path(inputPath, "solar", "series", paste0("solar_", name, ".txt"))
# )
# 
# 
# ## Thermal ----
# 
# # dir
# dir.create(path = file.path(inputPath, "thermal", "clusters", name), showWarnings = FALSE)
# 
# writeIni(
#   listData = list(),
#   pathIni = file.path(inputPath, "thermal", "clusters", name, "list.ini"),
#   overwrite = overwrite
# )
# 
# # thermal/areas ini file
# thermal_areas_path <- file.path(inputPath, "thermal", "areas.ini")
# if (file.exists(thermal_areas_path)) {
#   thermal_areas <- readIniFile(file = thermal_areas_path)
# } else {
#   thermal_areas <- list()
# }
# thermal_areas$unserverdenergycost[[name]] <- nodalOptimization[["unserverdenergycost"]]
# thermal_areas$spilledenergycost[[name]] <- nodalOptimization[["spilledenergycost"]]
# writeIni(thermal_areas, thermal_areas_path, overwrite = TRUE)
# 
# 
# 
# ## Wind ----
# 
# # prepro
# # dir
# dir.create(path = file.path(inputPath, "wind", "prepro", name), showWarnings = FALSE)
# 
# conversion <- matrix(data = c(-9999999980506447872,	0,	9999999980506447872, 0, 0, 0), nrow = 2, byrow = TRUE)
# utils::write.table(
#   x = conversion, row.names = FALSE, col.names = FALSE, sep = "\t",
#   file = file.path(inputPath, "wind", "prepro", name, "conversion.txt")
# )
# 
# data <- matrix(data = c(rep(1, 2*12), rep(0, 12), rep(1, 3*12)), nrow = 12)
# write.table(
#   x = data, row.names = FALSE, col.names = FALSE, sep = "\t",
#   file = file.path(inputPath, "wind", "prepro", name, "data.txt")
# )
# 
# utils::write.table(
#   x = character(0), row.names = FALSE, col.names = FALSE, sep = "\t",
#   file = file.path(inputPath, "wind", "prepro", name, "k.txt")
# )
# 
# writeIni(
#   listData = list(),
#   pathIni = file.path(inputPath, "wind", "prepro", name, "settings.ini"),
#   overwrite = overwrite
# )
# 
# write.table(
#   x = character(0), row.names = FALSE, col.names = FALSE, sep = "\t",
#   file = file.path(inputPath, "wind", "prepro", name, "translation.txt")
# )
# 
# # series
# utils::write.table(
#   x = character(0), row.names = FALSE, col.names = FALSE, sep = "\t",
#   file = file.path(inputPath, "wind", "series", paste0("wind_", name, ".txt"))
# )


