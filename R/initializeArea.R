#' Create An Area In An Antares Study
#'
#' @param name Name of the area
#' @param color Color of the node
#' @param localization Localization on the map
#' @param nodalOptimization Nodal optimization parameters
#' @param filtering Filtering parameters
#' @param overwrite Overwrite the area if already exist
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#'
#' @return An upddated list containing various information about the simulation.
#' @export
#' 
#' @importFrom antaresRead simOptions setSimulationPath
#' @importFrom utils read.table write.table
#' @importFrom assertthat assert_that
#'
#' @examples
#' \dontrun{
#' 
#' initializeArea("fictive_area")
#' 
#' }
initializeArea <- function(name, color = grDevices::rgb(230, 108, 44, max = 255),
                       localization = c(0, 0),
                       nodalOptimization = nodalOptimizationOptions(),
                       filtering = filteringOptions(),
                       overwrite = FALSE,
                       opts = antaresRead::simOptions()) {

  assertthat::assert_that(class(opts) == "simOption")
  
  if (name %in% opts$areaList & !overwrite)
    stop("Area already exist")

  # Input path
  inputPath <- opts$inputPath

  # Update area list
  areas <- c(opts$areaList, name)
  areas <- paste(sort(areas), collapse = "\n")
  writeLines(text = areas, con = file.path(inputPath, "areas/list.txt"))



  ## Create area ----
  # dir
  dir.create(path = file.path(inputPath, "areas", name), showWarnings = FALSE)
  # optimization ini file
  writeIni(
    listData = c(list(`nodal optimization` = nodalOptimization), list(filtering = filtering)),
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
  res <- antaresRead::setSimulationPath(path = opts$studyPath, simulation = opts$name)

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
    `filter-synthesis` = filter_synthesis,
    `filter-year-by-year` = filter_year_by_year
  )
}


#' Nodal optimization paramameters for creating an area
#'
#' @param non_dispatchable_power logical, default to FALSE
#' @param dispatchable_hydro_power logical, default to FALSE
#' @param other_dispatchable_power logical, default to FALSE
#' @param spread_unsupplied_energy_cost numeric, default to 0
#' @param spread_spilled_energy_cost numeric, default to 0
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
                                     spread_spilled_energy_cost = 0) {
  list(
    `non-dispatchable-power` = non_dispatchable_power,
    `dispatchable-hydro-power` = dispatchable_hydro_power,
    `other-dispatchable-power` = other_dispatchable_power,
    `spread-unsupplied-energy-cost` = spread_unsupplied_energy_cost,
    `spread-spilled-energy-cost` = spread_spilled_energy_cost
  )
}

