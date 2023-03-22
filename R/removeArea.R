#' @title Remove an area from an Antares study
#' 
#' @description 
#' `r antaresEditObject:::badge_api_ok()`
#' 
#' Remove an area in an Antares study.
#'
#' @param name An area name.
#' 
#' @template opts
#' 
#' @seealso [createArea()], [editArea()]
#' 
#' @export
#' 
#' @importFrom antaresRead simOptions setSimulationPath readBindingConstraints
#'
#' @examples
#' \dontrun{
#' removeArea("fictive_area")
#' }
removeArea <- function(name, opts = antaresRead::simOptions()) {
  
  # name of the area can contain upper case in areas/list.txt (and use in graphics)
  # (and use in graphics) but not in the folder name (and use in all other case)
  list_name <- name
  name <- tolower(name)
  
  # API block
  if (is_api_study(opts)) {
    cmd <- api_command_generate("remove_area", id = name)
    api_command_register(cmd, opts = opts)
    `if`(
      should_command_be_executed(opts), 
      api_command_execute(cmd, opts = opts, text_alert = "{.emph remove_area}: {msg_api}"),
      cli_command_registered("remove_area")
    )
    
    return(update_api_opts(opts))
  }
  
  check_area_name(name, opts)

  # Input path
  inputPath <- opts$inputPath
  
  # Links
  links_area <- as.character(getLinks(areas = name))
  if (length(links_area) > 0) {
    links_area <- strsplit(x = links_area, split = " - ")
    for (i in seq_along(links_area)) {
      area1 <- links_area[[i]][1]
      area2 <- links_area[[i]][2]
      opts <- removeLink(from = area1, to = area2, opts = opts)
    }
  }
  unlink(x = file.path(inputPath, "links", name), recursive = TRUE)
  alllinks <- list.files(path = file.path(inputPath, "links"), pattern = name, full.names = TRUE, recursive = TRUE)
  lapply(alllinks, unlink, recursive = TRUE)

  # Update area list
  areas <- readLines(file.path(inputPath, "areas/list.txt"))
  areas <- areas[!duplicated(areas)]
  index_to_remove <- match(name, tolower(areas))
  areas <- areas[-index_to_remove]
  areas <- paste(sort(areas), collapse = "\n")
  writeLines(text = areas, con = file.path(inputPath, "areas/list.txt"))


  # Area folder
  unlink(x =  file.path(inputPath, "areas", name), recursive = TRUE)


  # Hydro
  # ini
  if (file.exists(file.path(inputPath, "hydro", "hydro.ini"))) {
    hydro <- readIniFile(file = file.path(inputPath, "hydro", "hydro.ini"))
    if (!is.null(hydro$`inter-daily-breakdown`))
      hydro$`inter-daily-breakdown`[[name]] <- NULL
    if (!is.null(hydro$`intra-daily-modulation`))
      hydro$`intra-daily-modulation`[[name]] <- NULL
    if (!is.null(hydro$`inter-monthly-breakdown`))
      hydro$`inter-monthly-breakdown`[[name]] <- NULL
    if (!is.null(hydro$`initialize reservoir date`))
      hydro$`initialize reservoir date`[[name]] <- NULL
    if (!is.null(hydro$`leeway low`))
      hydro$`leeway low`[[name]] <- NULL
    if (!is.null(hydro$`leeway up`))
      hydro$`leeway up`[[name]] <- NULL
    if (!is.null(hydro$`pumping efficiency`))
      hydro$`pumping efficiency`[[name]] <- NULL
    writeIni(
      listData = hydro,
      pathIni = file.path(inputPath, "hydro", "hydro.ini"),
      overwrite = TRUE
    )
  }
  # allocation
  unlink(x = file.path(inputPath, "hydro", "allocation", paste0(name, ".ini")), recursive = TRUE)
  # capacity
  unlink(x = file.path(inputPath, "hydro", "common", "capacity", paste0("maxpower_", name, ".txt")), recursive = TRUE)
  unlink(x = file.path(inputPath, "hydro", "common", "capacity", paste0("reservoir_", name, ".txt")), recursive = TRUE)
  unlink(x = file.path(inputPath, "hydro", "common", "capacity", paste0("creditmodulations_", name, ".txt")), recursive = TRUE)
  unlink(x = file.path(inputPath, "hydro", "common", "capacity", paste0("inflowPattern_", name, ".txt")), recursive = TRUE)
  unlink(x = file.path(inputPath, "hydro", "common", "capacity", paste0("waterValues_", name, ".txt")), recursive = TRUE)
  # prepro
  unlink(x = file.path(inputPath, "hydro", "prepro", name), recursive = TRUE)
  # series
  unlink(x = file.path(inputPath, "hydro", "series", name), recursive = TRUE)




  # Load
  unlink(x = file.path(inputPath, "load", "prepro", name), recursive = TRUE)
  unlink(x = file.path(inputPath, "load", "series", paste0("load_", name, ".txt")), recursive = TRUE)


  # Misc-gen
  unlink(x = file.path(inputPath, "misc-gen", paste0("miscgen-", name, ".txt")), recursive = TRUE)


  # Reserves
  unlink(x = file.path(inputPath, "reserves", paste0(name, ".txt")), recursive = TRUE)


  # Solar
  unlink(x = file.path(inputPath, "solar", "prepro", name), recursive = TRUE)
  unlink(x = file.path(inputPath, "solar", "series", paste0("solar_", name, ".txt")), recursive = TRUE)


  # Thermal
  unlink(x = file.path(inputPath, "thermal", "clusters", name), recursive = TRUE)
  unlink(x = file.path(inputPath, "thermal", "prepro", name), recursive = TRUE)
  unlink(x = file.path(inputPath, "thermal", "series", name), recursive = TRUE)
  
  thermal_areas_path <- file.path(inputPath, "thermal", "areas.ini")
  if (file.exists(thermal_areas_path)) {
    thermal_areas <- readIniFile(thermal_areas_path)
    if (!is.null(thermal_areas$unserverdenergycost))
      thermal_areas$unserverdenergycost[[name]] <- NULL
    if (!is.null(thermal_areas$spilledenergycost))
      thermal_areas$spilledenergycost[[name]] <- NULL
    writeIni(thermal_areas, thermal_areas_path, overwrite = TRUE)
  }


  # Wind
  unlink(x = file.path(inputPath, "wind", "prepro", name), recursive = TRUE)
  unlink(x = file.path(inputPath, "wind", "series", paste0("wind_", name, ".txt")), recursive = TRUE)

  
  
  # Remove binding constraints
  bc <- readBindingConstraints(opts = opts)
  bc_area <- lapply(
    X = bc,
    FUN = function(x) {
      all(grepl(pattern = name, x = names(x$coefs)))
    }
  )
  bc_area <- unlist(bc_area)
  bc_remove <- names(bc_area[bc_area])
  if (length(bc_remove) > 0) {
    for (bci in bc_remove) {
      opts <- removeBindingConstraint(name = bci, opts = opts)
    }
  }
  
  
  bindingconstraints <- readLines(
    con = file.path(inputPath, "bindingconstraints", "bindingconstraints.ini")
  )
  # bindingconstraints <- grep(pattern = name, x = bindingconstraints, value = TRUE, invert = TRUE)
  ind1 <- !grepl(pattern = paste0("^", name, "%"), x = bindingconstraints)
  ind2 <- !grepl(pattern = paste0("%", name, "\\s"), x = bindingconstraints)
  
  writeLines(
    text = paste(bindingconstraints[ind1 | ind2], collapse = "\n"), 
    con = file.path(inputPath, "bindingconstraints", "bindingconstraints.ini")
  )


  # Maj simulation
  suppressWarnings({
    res <- antaresRead::setSimulationPath(path = opts$studyPath, simulation = "input")
  })

  invisible(res)
}






#' @title Seek for a removed area
#'
#' @description Check if it remains trace of a deleted area in the input folder
#'
#' @param area An area
#' @param all_files Check files in study directory.
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#'
#' @return a named list with two elements
#' @export
#'
#' @examples
#' \dontrun{
#' checkRemovedArea("myarea")
#' }
checkRemovedArea <- function(area, all_files = TRUE, opts = antaresRead::simOptions()) {
  
  # Input path
  inputPath <- opts$inputPath
  
  
  # Search for files or directories named after the area searched
  inputFiles <- list.files(
    path = inputPath,
    pattern = if (all_files) "" else area,
    recursive = TRUE, include.dirs = TRUE, full.names = TRUE
  )
  
  areaResiduFiles <- grep(
    pattern = sprintf("[[:punct:]]+%s[[:punct:]]+|%s$", area, area), 
    x = inputFiles, 
    value = TRUE
  )
  
  
  # Check files content
  areaResidus <- vector(mode = "character")
  for (i in inputFiles) {
    if (!file.info(i)$isdir) {
      suppressWarnings({tmp <- readLines(con = i)})
      tmp <- paste(tmp, collapse = "\n")
      if (grepl(pattern = area, x = tmp)) {
        areaResidus <- append(areaResidus, i)
      }
    }
  }
  
  list(
    areaResiduFiles = areaResiduFiles,
    areaResidus = areaResidus
  )
  
}






