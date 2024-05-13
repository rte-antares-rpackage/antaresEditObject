#' @title Create an area in an Antares study
#' 
#' @description 
#' `r antaresEditObject:::badge_api_ok()`
#' 
#' Create a new area in an Antares study.
#' 
#'
#' @param name Name of the area as a character, without punctuation except - and _.
#' @param color Color of the node
#' @param localization Localization on the map
#' @param nodalOptimization Nodal optimization parameters, see [nodalOptimizationOptions()].
#' @param filtering Filtering parameters, see [filteringOptions()].
#' @param adequacy Adequacy parameters, see [adequacyOptions()].
#' @param overwrite Overwrite the area if already exist.
#' 
#' @template opts
#' 
#' @seealso [editArea()], [removeArea()]
#' 
#' @export
#' 
#' @importFrom antaresRead simOptions setSimulationPath readIniFile
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
#' # Create a new area
#' createArea("fictive_area")
#' 
#' }
createArea <- function(name,
                       color = grDevices::rgb(230, 108, 44, max = 255),
                       localization = c(0, 0),
                       nodalOptimization = nodalOptimizationOptions(),
                       filtering = filteringOptions(),
                       adequacy = adequacyOptions(),
                       overwrite = FALSE,
                       opts = antaresRead::simOptions()) {
  
  assertthat::assert_that(inherits(opts, "simOptions"))
  validate_area_name(name)
  # name of the area can contain upper case in areas/list.txt (and use in graphics)
  # (and use in graphics) but not in the folder name (and use in all other case)
  list_name <- name
  name <- tolower(name)
  
  is_830 <- opts$antaresVersion >= 830
  nodal_by_targets <- .split_nodalOptimization_by_target(nodalOptimization)
  nodalOptimization <- nodal_by_targets[["toIniOptimization"]]
  nodalThermal <- nodal_by_targets[["toIniAreas"]]
  
  # API block
  if (is_api_study(opts)) {
    cmd <- api_command_generate("create_area", area_name = name)
    api_command_register(cmd, opts = opts)
    `if`(
      should_command_be_executed(opts), 
      api_command_execute(cmd, opts = opts, text_alert = "{.emph create_area}: "),
      cli_command_registered("create_area")
    )
    
    default_nodal_by_targets <- .split_nodalOptimization_by_target(nodalOptimizationOptions())
    # input/areas/<name>/optimization/nodal optimization
    if (is_different(nodalOptimization,
                     default_nodal_by_targets[["toIniOptimization"]]
                    )
        ) {
      cmd <- api_command_generate(
        action = "update_config", 
        target = sprintf("input/areas/%s/optimization/nodal optimization", name),
        data = nodalOptimization
      )
      api_command_register(cmd, opts = opts)
      `if`(
        should_command_be_executed(opts), 
        api_command_execute(cmd, opts = opts, text_alert = "Create area's nodal optimization option: "),
        cli_command_registered("update_config")
      )
    }
    
    # input/thermal/areas
    unserverdenergycost <- nodalThermal[["unserverdenergycost"]]
    if (is_different(unserverdenergycost,
                     default_nodal_by_targets[["toIniAreas"]][["unserverdenergycost"]]
                     )
       ) {
      cmd <- api_command_generate(
        action = "update_config", 
        target = sprintf("input/thermal/areas/unserverdenergycost/%s", name),
        data = unserverdenergycost
      )
      api_command_register(cmd, opts = opts)
      `if`(
        should_command_be_executed(opts), 
        api_command_execute(cmd, opts = opts, text_alert = "Create area's unsupplied energy cost option: {msg_api}"),
        cli_command_registered("update_config")
      )
    }
    
    spilledenergycost <- nodalThermal[["spilledenergycost"]]
    if (is_different(spilledenergycost,
                     default_nodal_by_targets[["toIniAreas"]][["spilledenergycost"]]
                    )
       ) {
      cmd <- api_command_generate(
        action = "update_config", 
        target = sprintf("input/thermal/areas/spilledenergycost/%s", name),
        data = spilledenergycost
      )
      api_command_register(cmd, opts = opts)
      `if`(
        should_command_be_executed(opts), 
        api_command_execute(cmd, opts = opts, text_alert = "Create area's spilled energy cost option: {msg_api}"),
        cli_command_registered("update_config")
      )
    }
    
    # input/areas/<name>/optimization/filtering
    if (is_different(filtering, filteringOptions())){
      cmd <- api_command_generate(
        action = "update_config", 
        target = sprintf("input/areas/%s/optimization/filtering", name),
        data = filtering
      )
      api_command_register(cmd, opts = opts)
      `if`(
        should_command_be_executed(opts), 
        api_command_execute(cmd, opts = opts, text_alert = "Create area's filtering: "),
        cli_command_registered("update_config")
      )
    }
    if (is_830){
      if (is_different(adequacy, adequacyOptions())){
        cmd <- api_command_generate(
          action = "update_config", 
          target = sprintf("input/areas/%s/adequacy_patch/adequacy-patch", name),
          data = adequacy
        )
        api_command_register(cmd, opts = opts)
        `if`(
          should_command_be_executed(opts), 
          api_command_execute(cmd, opts = opts, text_alert = "Create area's adequacy patch mode: "),
          cli_command_registered("update_config")
        )
      }
    }
    
    return(update_api_opts(opts))
  }
  
  if (opts$mode != "Input") 
    stop("You can initialize an area only in 'Input' mode")
  
  if (name %in% opts$areaList & !overwrite)
    stop(sprintf("Area %s already exist", name), call. = FALSE)
  
  if (name %in% opts$areaList & overwrite)
    opts <- removeArea(name = name, opts = opts)
  
  # Input path
  inputPath <- opts$inputPath
  assertthat::assert_that(!is.null(inputPath) && file.exists(inputPath))
  
  # Update area list
  areas <- readLines(file.path(inputPath, "areas/list.txt"))
  areas <- c(areas, list_name)
  areas <- areas[!duplicated(areas)]
  areas <- paste(sort(areas), collapse = "\n")
  writeLines(text = areas, con = file.path(inputPath, "areas/list.txt"))
  
  ## Create area ----
  # dir
  dir.create(path = file.path(inputPath, "areas", name), showWarnings = FALSE)
  # optimization ini file
  writeIni(
    listData = c(
      list(`nodal optimization` = nodalOptimization),
      list(filtering = filtering)
    ),
    pathIni = file.path(inputPath, "areas", name, "optimization.ini"),
    overwrite = overwrite
  )
  # ui ini file
  localization <- as.character(localization)
  ui <- list(
    ui = list(
      x = localization[1], y = localization[2],
      color_r = unname(grDevices::col2rgb(color)["red", 1]),
      color_g = unname(grDevices::col2rgb(color)["green", 1]),
      color_b = unname(grDevices::col2rgb(color)["blue", 1]),
      layers = "0"
    ),
    layerX = list(`0` = localization[1]),
    layerY = list(`0` = localization[2]),
    layerColor = list(`0` = as.vector(grDevices::col2rgb(color)))
  )
  writeIni(
    listData = ui,
    pathIni = file.path(inputPath, "areas", name, "ui.ini"),
    overwrite = overwrite
  )
  # adequacy patch ini file
  if (is_830){
    writeIni(
      listData = c(
        list(`adequacy-patch` = adequacy[c("adequacy-patch-mode")])
      ),
      pathIni = file.path(inputPath, "areas", name, "adequacy_patch.ini"),
      overwrite = overwrite
    )
  }
  
  
  ## Hydro ----
  
  # ini
  if (file.exists(file.path(inputPath, "hydro", "hydro.ini"))) {
    default_params <- get_default_hydro_ini_values()
    # Check area is not possible at this step
    writeIniHydro(area = name, params = default_params, mode = "createArea", opts = opts)
  }
  
  # allocation
  allocation <- list(as.character(1))
  names(allocation) <- name
  writeIni(
    listData = list(`[allocation]` = allocation),
    pathIni = file.path(inputPath, "hydro", "allocation", paste0(name, ".ini")),
    overwrite = overwrite
  )
  
  # capacity
  con <- file(description = file.path(inputPath, "hydro", "common", "capacity", paste0("maxpower_", name, ".txt")), open = "wt")
  writeLines(text = character(0), con = con)
  close(con)
  
  reservoir <- matrix(data = rep(c(0, 0.5, 1), each = 12), ncol = 3)
  utils::write.table(
    x = reservoir, row.names = FALSE, col.names = FALSE, sep = "\t",
    file = file.path(inputPath, "hydro", "common", "capacity", paste0("reservoir_", name, ".txt"))
  )
  
  if (is_antares_v7(opts)) {
    creditmodulations <- matrix(data = rep(1, 202), nrow = 2)
    utils::write.table(
      x = creditmodulations, row.names = FALSE, col.names = FALSE, sep = "\t",
      file = file.path(inputPath, "hydro", "common", "capacity", paste0("creditmodulations_", name, ".txt"))
    )
    
    inflowPattern <- matrix(data = rep(1, 365), ncol = 1)
    utils::write.table(
      x = inflowPattern, row.names = FALSE, col.names = FALSE, sep = "\t",
      file = file.path(inputPath, "hydro", "common", "capacity", paste0("inflowPattern_", name, ".txt"))
    )
    
    maxpower <- matrix(data = rep(c(0, 24, 0, 24), each = 365), ncol = 4)
    utils::write.table(
      x = maxpower, row.names = FALSE, col.names = FALSE, sep = "\t",
      file = file.path(inputPath, "hydro", "common", "capacity", paste0("maxpower_", name, ".txt"))
    )
    
    reservoir <- matrix(data = rep(c("0", "0.500", "1"), each = 365), ncol = 3)
    utils::write.table(
      x = reservoir, row.names = FALSE, col.names = FALSE, sep = "\t", quote = FALSE,
      file = file.path(inputPath, "hydro", "common", "capacity", paste0("reservoir_", name, ".txt"))
    )
    
    con <- file(description = file.path(inputPath, "hydro", "common", "capacity", paste0("waterValues_", name, ".txt")), open = "wt")
    writeLines(text = character(0), con = con)
    close(con)
  }
  
  # prepro
  # dir
  dir.create(path = file.path(inputPath, "hydro", "prepro", name), showWarnings = FALSE)
  
  con <- file(description = file.path(inputPath, "hydro", "prepro", name, "energy.txt"), open = "wt")
  writeLines(text = character(0), con = con)
  close(con)
  
  writeIni(
    listData = list(`prepro` = list(`intermonthly-correlation` = 0.5)),
    pathIni = file.path(inputPath, "hydro", "prepro", name, "prepro.ini"),
    overwrite = overwrite
  )
  
  # series
  # dir
  dir.create(path = file.path(inputPath, "hydro", "series", name), showWarnings = FALSE)
  
  con <- file(description = file.path(inputPath, "hydro", "series", name, "mod.txt"), open = "wt")
  writeLines(text = character(0), con = con)
  close(con)
  
  con <- file(description = file.path(inputPath, "hydro", "series", name, "ror.txt"), open = "wt")
  writeLines(text = character(0), con = con)
  close(con)
  
  ## Load ----
  
  # prepro
  # dir
  dir.create(path = file.path(inputPath, "load", "prepro", name), showWarnings = FALSE)
  
  conversion <- matrix(data = c(-9999999980506447872, 0, 9999999980506447872, 0, 0, 0), nrow = 2, byrow = TRUE)
  utils::write.table(
    x = conversion, row.names = FALSE, col.names = FALSE, sep = "\t",
    file = file.path(inputPath, "load", "prepro", name, "conversion.txt")
  )
  
  data <- matrix(data = c(rep(1, 2*12), rep(0, 12), rep(1, 3*12)), nrow = 12)
  utils::write.table(
    x = data, row.names = FALSE, col.names = FALSE, sep = "\t",
    file = file.path(inputPath, "load", "prepro", name, "data.txt")
  )
  
  utils::write.table(
    x = character(0), row.names = FALSE, col.names = FALSE, sep = "\t",
    file = file.path(inputPath, "load", "prepro", name, "k.txt")
  )
  
  writeIni(
    listData = list(),
    pathIni = file.path(inputPath, "load", "prepro", name, "settings.ini"),
    overwrite = overwrite
  )
  
  utils::write.table(
    x = character(0), row.names = FALSE, col.names = FALSE, sep = "\t",
    file = file.path(inputPath, "load", "prepro", name, "translation.txt")
  )
  
  # series
  write.table(
    x = character(0), row.names = FALSE, col.names = FALSE, sep = "\t",
    file = file.path(inputPath, "load", "series", paste0("load_", name, ".txt"))
  )
  
  
  
  ## Misc-gen ----
  utils::write.table(
    x = character(0), row.names = FALSE, col.names = FALSE, sep = "\t",
    file = file.path(inputPath, "misc-gen", paste0("miscgen-", name, ".txt"))
  )
  
  
  ## Reserves ----
  utils::write.table(
    x = character(0), row.names = FALSE, col.names = FALSE, sep = "\t",
    file = file.path(inputPath, "reserves", paste0(name, ".txt"))
  )
  
  
  ## Solar ----
  
  # prepro
  # dir
  dir.create(path = file.path(inputPath, "solar", "prepro", name), showWarnings = FALSE)
  
  conversion <- matrix(data = c(-9999999980506447872, 0, 9999999980506447872, 0, 0, 0), nrow = 2, byrow = TRUE)
  utils::write.table(
    x = conversion, row.names = FALSE, col.names = FALSE, sep = "\t",
    file = file.path(inputPath, "solar", "prepro", name, "conversion.txt")
  )
  
  data <- matrix(data = c(rep(1, 2*12), rep(0, 12), rep(1, 3*12)), nrow = 12)
  utils::write.table(
    x = data, row.names = FALSE, col.names = FALSE, sep = "\t",
    file = file.path(inputPath, "solar", "prepro", name, "data.txt")
  )
  
  write.table(
    x = character(0), row.names = FALSE, col.names = FALSE, sep = "\t",
    file = file.path(inputPath, "solar", "prepro", name, "k.txt")
  )
  
  writeIni(
    listData = list(),
    pathIni = file.path(inputPath, "solar", "prepro", name, "settings.ini"),
    overwrite = overwrite
  )
  
  utils::write.table(
    x = character(0), row.names = FALSE, col.names = FALSE, sep = "\t",
    file = file.path(inputPath, "solar", "prepro", name, "translation.txt")
  )
  
  # series
  utils::write.table(
    x = character(0), row.names = FALSE, col.names = FALSE, sep = "\t",
    file = file.path(inputPath, "solar", "series", paste0("solar_", name, ".txt"))
  )
  
  ## Links ----
  .initializeLinksArea(name = name, overwrite = overwrite, opts = opts)
  
  ## Thermal ----
  .initializeThermalArea(name = name,
                         overwrite = overwrite,
                         economic_options = nodalThermal,
                         opts = opts
                         )
  
  ## Renewables ----
  .initializeRenewablesArea(name = name, overwrite = overwrite, opts = opts)
  
  ## st-storage ----
  
  # INIT dir
  if (opts$antaresVersion >= 860) {
    dir.create(path = file.path(inputPath, "st-storage", "clusters", name), showWarnings = FALSE)
    
  # INIT list.ini file  
    writeIni(
      listData = list(),
      pathIni = file.path(inputPath, "st-storage", "clusters", name, "list.ini"),
      overwrite = overwrite
    )
    
    # INIT mingen.txt
      # /series
    con <- file(description = file.path(inputPath, "hydro", "series", name, "mingen.txt"), open = "wt")
    writeLines(text = character(0), con = con)
    close(con)
  }
  
  ## Wind ----
  
  # prepro
  # dir
  dir.create(path = file.path(inputPath, "wind", "prepro", name), showWarnings = FALSE)
  
  conversion <- matrix(data = c(-9999999980506447872, 0, 9999999980506447872, 0, 0, 0), nrow = 2, byrow = TRUE)
  utils::write.table(
    x = conversion, row.names = FALSE, col.names = FALSE, sep = "\t",
    file = file.path(inputPath, "wind", "prepro", name, "conversion.txt")
  )
  
  data <- matrix(data = c(rep(1, 2*12), rep(0, 12), rep(1, 3*12)), nrow = 12)
  write.table(
    x = data, row.names = FALSE, col.names = FALSE, sep = "\t",
    file = file.path(inputPath, "wind", "prepro", name, "data.txt")
  )
  
  utils::write.table(
    x = character(0), row.names = FALSE, col.names = FALSE, sep = "\t",
    file = file.path(inputPath, "wind", "prepro", name, "k.txt")
  )
  
  writeIni(
    listData = list(),
    pathIni = file.path(inputPath, "wind", "prepro", name, "settings.ini"),
    overwrite = overwrite
  )
  
  write.table(
    x = character(0), row.names = FALSE, col.names = FALSE, sep = "\t",
    file = file.path(inputPath, "wind", "prepro", name, "translation.txt")
  )
  
  # series
  utils::write.table(
    x = character(0), row.names = FALSE, col.names = FALSE, sep = "\t",
    file = file.path(inputPath, "wind", "series", paste0("wind_", name, ".txt"))
  )
  
  # Maj simulation
  suppressWarnings({
    res <- antaresRead::setSimulationPath(path = opts$studyPath, simulation = "input")
  })
  
  invisible(res)
}


#' Output profile options for creating an area
#'
#' @param filter_synthesis Character, vector of time steps used in the output synthesis, among `hourly`, `daily`, `weekly`, `monthly`, and `annual`
#' @param filter_year_by_year Character, vector of time steps used in the output year-by-year, among `hourly`, `daily`, `weekly`, `monthly`, and `annual`
#'
#' @return a named list
#' @export
#'
#' @examples
#' filteringOptions(
#'   filter_synthesis=c("hourly","daily"),
#'   filter_year_by_year=c("weekly","monthly")
#' )
filteringOptions <- function(filter_synthesis = c("hourly", "daily", "weekly", "monthly", "annual"),
                             filter_year_by_year = c("hourly", "daily", "weekly", "monthly", "annual")) {
  list(
    `filter-synthesis` = paste(filter_synthesis, collapse = ", "),
    `filter-year-by-year` = paste(filter_year_by_year, collapse = ", ")
  )
}


#' Nodal optimization parameters for creating an area
#'
#' @param non_dispatchable_power logical, default to FALSE
#' @param dispatchable_hydro_power logical, default to FALSE
#' @param other_dispatchable_power logical, default to FALSE
#' @param spread_unsupplied_energy_cost numeric, default to 0
#' @param spread_spilled_energy_cost numeric, default to 0
#' @param average_unsupplied_energy_cost numeric, default to 0
#' @param average_spilled_energy_cost numeric, default to 0
#'
#' @return a named list
#' @export
#'
#' @examples
#' nodalOptimizationOptions()
nodalOptimizationOptions <- function(non_dispatchable_power = TRUE,
                                     dispatchable_hydro_power = TRUE,
                                     other_dispatchable_power = TRUE,
                                     spread_unsupplied_energy_cost = 0,
                                     spread_spilled_energy_cost = 0,
                                     average_unsupplied_energy_cost = 0,
                                     average_spilled_energy_cost = 0) {
  list(
    `non-dispatchable-power` = non_dispatchable_power,
    `dispatchable-hydro-power` = dispatchable_hydro_power,
    `other-dispatchable-power` = other_dispatchable_power,
    `spread-unsupplied-energy-cost` = spread_unsupplied_energy_cost,
    `spread-spilled-energy-cost` = spread_spilled_energy_cost,
    `unserverdenergycost` = average_unsupplied_energy_cost,
    `spilledenergycost` = average_spilled_energy_cost
  )
}


#' Adequacy patch parameters for creating an area
#'
#' @param adequacy_patch_mode character, default to "outside"
#'
#' @return a named list
#' @export
#'
#' @examples
#' adequacyOptions()
adequacyOptions <- function(adequacy_patch_mode = "outside"){
  list(
    `adequacy-patch-mode` = adequacy_patch_mode
  )
}


#' Split list nodalOptimization by target file.
#'
#' @param nodalOptimization Nodal optimization parameters, see [nodalOptimizationOptions()]
.split_nodalOptimization_by_target <- function(nodalOptimization) {
  
  nodal_optimization <- NULL
  nodal_thermal <- NULL
  
  properties_to_edit <- names(nodalOptimization)
  
  # input/thermal/areas.ini
  target_IniAreas <- c("unserverdenergycost", "spilledenergycost")
  # input/areas/<area>/optimization.ini
  target_IniOptimization <- setdiff(names(nodalOptimizationOptions()), target_IniAreas)
  
  ini_optimization <- intersect(properties_to_edit, target_IniOptimization)
  if (!identical(ini_optimization, character(0))) {
    nodal_optimization <- nodalOptimization[ini_optimization]
  } 
  
  ini_areas <- intersect(properties_to_edit, target_IniAreas)
  if (!identical(ini_areas, character(0))) {
    nodal_thermal <- nodalOptimization[ini_areas]
  }
  
  return(list("toIniOptimization" = nodal_optimization,
              "toIniAreas" = nodal_thermal
              )
        )
}


#' Initialize thermal data for a new area. For disk mode only.
#'
#' @param name Name of the area as a character, without punctuation except - and _.
#' @param overwrite Overwrite the area if already exists.
#' @param economic_options Economic options.
#'
#' @template opts
#'
.initializeThermalArea <- function(name, overwrite, economic_options, opts) {
  
  inputPath <- opts$inputPath
  # dir
  dir.create(path = file.path(inputPath, "thermal", "clusters", name), showWarnings = FALSE)
  
  writeIni(
    listData = list(),
    pathIni = file.path(inputPath, "thermal", "clusters", name, "list.ini"),
    overwrite = overwrite
  )
  
  # thermal/areas ini file
  thermal_areas_path <- file.path(inputPath, "thermal", "areas.ini")
  if (file.exists(thermal_areas_path)) {
    thermal_areas <- readIniFile(file = thermal_areas_path)
  } else {
    thermal_areas <- list()
  }
  thermal_areas[["unserverdenergycost"]][[name]] <- economic_options[["unserverdenergycost"]]
  thermal_areas[["spilledenergycost"]][[name]] <- economic_options[["spilledenergycost"]]
  
  writeIni(thermal_areas, thermal_areas_path, overwrite = TRUE)
}


#' Initialize links data for a new area. For disk mode only.
#'
#' @param name Name of the area as a character, without punctuation except - and _.
#' @param overwrite Overwrite the area if already exists.
#'
#' @template opts
#'
.initializeLinksArea <- function(name, overwrite, opts) {
  
  linksPath <- file.path(opts$inputPath, "links", name)
  # dir
  dir.create(path = linksPath, showWarnings = FALSE)
  writeIni(
    listData = list(),
    pathIni = file.path(linksPath, "properties.ini"),
    overwrite = overwrite
  )
}


#' Initialize renewables data for a new area. For disk mode only.
#'
#' @param name Name of the area as a character, without punctuation except - and _.
#' @param overwrite Overwrite the area if already exists.
#'
#' @template opts
#'
.initializeRenewablesArea <- function(name, overwrite, opts) {
  
  if (is_active_RES(opts)) {
    renewablesPath <- file.path(opts$inputPath, "renewables", "clusters", name)
    # dir
    dir.create(path = renewablesPath, showWarnings = FALSE)
    
    # list.ini file
    writeIni(
      listData = list(),
      pathIni = file.path(renewablesPath, "list.ini"),
      overwrite = overwrite
    )
  }
}
