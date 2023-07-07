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
  
  # API block
  if (is_api_study(opts)) {
    cmd <- api_command_generate("create_area", area_name = name)
    api_command_register(cmd, opts = opts)
    `if`(
      should_command_be_executed(opts), 
      api_command_execute(cmd, opts = opts, text_alert = "{.emph create_area}: {msg_api}"),
      cli_command_registered("create_area")
    )
    
    if (is_different(nodalOptimization, nodalOptimizationOptions())){
      cmd <- api_command_generate(
        action = "update_config", 
        target = sprintf("input/areas/%s/optimization/nodal optimization", name),
        data = nodalOptimization
      )
      api_command_register(cmd, opts = opts)
      `if`(
        should_command_be_executed(opts), 
        api_command_execute(cmd, opts = opts, text_alert = "Create area's nodal optimization option: {msg_api}"),
        cli_command_registered("update_config")
      )
    }
    if (is_different(filtering, filteringOptions())){
      cmd <- api_command_generate(
        action = "update_config", 
        target = sprintf("input/areas/%s/optimization/filtering", name),
        data = filtering
      )
      api_command_register(cmd, opts = opts)
      `if`(
        should_command_be_executed(opts), 
        api_command_execute(cmd, opts = opts, text_alert = "Create area's filtering: {msg_api}"),
        cli_command_registered("update_config")
      )
    }
    if (opts$antaresVersion >= 830){
      if (is_different(adequacy, adequacyOptions())){
        cmd <- api_command_generate(
          action = "update_config", 
          target = sprintf("input/areas/%s/adequacy_patch/adequacy-patch", name),
          data = adequacy
        )
        api_command_register(cmd, opts = opts)
        `if`(
          should_command_be_executed(opts), 
          api_command_execute(cmd, opts = opts, text_alert = "Create area's adequacy patch mode: {msg_api}"),
          cli_command_registered("update_config")
        )
      }
    }
    
    return(update_api_opts(opts))
  }
  
  v7 <- is_antares_v7(opts)
  
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
      list(`nodal optimization` = nodalOptimization[c(
        "non-dispatchable-power",
        "dispatchable-hydro-power",
        "other-dispatchable-power",
        "spread-unsupplied-energy-cost",
        "spread-spilled-energy-cost"
      )]),
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
  if (opts$antaresVersion >= 830){
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
    hydro <- readIniFile(file = file.path(inputPath, "hydro", "hydro.ini"))
    if (!is.null(hydro$`inter-daily-breakdown`))
      hydro$`inter-daily-breakdown`[[name]] <- 1
    if (!is.null(hydro$`intra-daily-modulation`))
      hydro$`intra-daily-modulation`[[name]] <- 24
    if (!is.null(hydro$`inter-monthly-breakdown`))
      hydro$`inter-monthly-breakdown`[[name]] <- 1
    
    if (v7) {
      if (!is.null(hydro$`initialize reservoir date`))
        hydro$`initialize reservoir date`[[name]] <- 0
      if (!is.null(hydro$`leeway low`))
        hydro$`leeway low`[[name]] <- 1
      if (!is.null(hydro$`leeway up`))
        hydro$`leeway up`[[name]] <- 1
      if (!is.null(hydro$`pumping efficiency`))
        hydro$`pumping efficiency`[[name]] <- 1
    }
    
    writeIni(
      listData = hydro,
      pathIni = file.path(inputPath, "hydro", "hydro.ini"),
      overwrite = TRUE
    )
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
  
  if (v7) {
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
  
  
  
  ## Links ----
  # dir
  dir.create(path = file.path(inputPath, "links", name), showWarnings = FALSE)
  writeIni(
    listData = list(),
    pathIni = file.path(inputPath, "links", name, "properties.ini"),
    overwrite = overwrite
  )
  
  
  
  ## Load ----
  
  # prepro
  # dir
  dir.create(path = file.path(inputPath, "load", "prepro", name), showWarnings = FALSE)
  
  conversion <- matrix(data = c(-9999999980506447872,	0,	9999999980506447872, 0, 0, 0), nrow = 2, byrow = TRUE)
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
  
  conversion <- matrix(data = c(-9999999980506447872,	0,	9999999980506447872, 0, 0, 0), nrow = 2, byrow = TRUE)
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
  
  
  ## Thermal ----
  
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
  thermal_areas$unserverdenergycost[[name]] <- nodalOptimization[["unserverdenergycost"]]
  thermal_areas$spilledenergycost[[name]] <- nodalOptimization[["spilledenergycost"]]
  writeIni(thermal_areas, thermal_areas_path, overwrite = TRUE)
  
  
  ## Renewables ----
  
  if (is_active_RES(opts)) {
    # INIT dir
    dir.create(path = file.path(inputPath, "renewables", "clusters", name), showWarnings = FALSE)
    
    # INIT list.ini file
    writeIni(
      listData = list(),
      pathIni = file.path(inputPath, "renewables", "clusters", name, "list.ini"),
      overwrite = overwrite
    )
    
    
  }
  
  
  ## st-storage ----
  
  # INIT dir
  if (opts$antaresVersion >= 860 ){
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
  
  conversion <- matrix(data = c(-9999999980506447872,	0,	9999999980506447872, 0, 0, 0), nrow = 2, byrow = TRUE)
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
#' @param filter_synthesis Output synthesis
#' @param filter_year_by_year Output Year-by-year
#'
#' @return a named list
#' @export
#'
#' @examples
#' filteringOptions()
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