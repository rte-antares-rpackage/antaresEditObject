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
  
  check_area_name(name, opts)
  api_study <- is_api_study(opts)
  if (!api_study | (api_study && !is_api_mocked(opts))) {
    # check if the area can be removed safely, i.e. the area is not referenced in a binding constraint
    .check_area_in_binding_constraint(name, opts)
  }
  
  # API block
  if (api_study) {
    cmd <- api_command_generate("remove_area", id = name)
    api_command_register(cmd, opts = opts)
    `if`(
      should_command_be_executed(opts), 
      api_command_execute(cmd, opts = opts, text_alert = "{.emph remove_area}: {msg_api}"),
      cli_command_registered("remove_area")
    )
    
    return(update_api_opts(opts))
  }
  
  # Input path
  inputPath <- opts$inputPath
  
  ## Links
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

  # Update area list
  areas <- readLines(file.path(inputPath, "areas/list.txt"))
  areas <- areas[!duplicated(areas)]
  index_to_remove <- match(name, tolower(areas))
  areas <- areas[-index_to_remove]
  areas <- paste(sort(areas), collapse = "\n")
  writeLines(text = areas, con = file.path(inputPath, "areas/list.txt"))


  # Area folder
  unlink(x =  file.path(inputPath, "areas", name), recursive = TRUE)
  
  ## Hydro
  # ini
  if (file.exists(file.path(inputPath, "hydro", "hydro.ini"))) {
    default_params <- get_default_hydro_ini_values()
    empty_params <- sapply(names(default_params), FUN = function(n) default_params[[n]] <- NULL)
    writeIniHydro(area = name, params = empty_params, mode = "removeArea", opts = opts)
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

  ## Load
  unlink(x = file.path(inputPath, "load", "prepro", name), recursive = TRUE)
  unlink(x = file.path(inputPath, "load", "series", paste0("load_", name, ".txt")), recursive = TRUE)

  ## Misc-gen
  unlink(x = file.path(inputPath, "misc-gen", paste0("miscgen-", name, ".txt")), recursive = TRUE)

  ## Reserves
  unlink(x = file.path(inputPath, "reserves", paste0(name, ".txt")), recursive = TRUE)

  ## Solar
  unlink(x = file.path(inputPath, "solar", "prepro", name), recursive = TRUE)
  unlink(x = file.path(inputPath, "solar", "series", paste0("solar_", name, ".txt")), recursive = TRUE)

  ## Thermal
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

  ## Wind
  unlink(x = file.path(inputPath, "wind", "prepro", name), recursive = TRUE)
  unlink(x = file.path(inputPath, "wind", "series", paste0("wind_", name, ".txt")), recursive = TRUE)
  
  ## st-storage
  unlink(x = file.path(inputPath, "st-storage", "clusters", name), recursive = TRUE)
  unlink(x = file.path(inputPath, "st-storage", "series", name), recursive = TRUE)
  
  ## renewables
  unlink(x = file.path(inputPath, "renewables", "clusters", name), recursive = TRUE)
  unlink(x = file.path(inputPath, "renewables", "series", name), recursive = TRUE)

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


.check_area_in_binding_constraint <- function(name, opts) {

  # Link
  bc_not_remove_link <- character(0)
  links_area <- as.character(getLinks(areas = name, opts = opts))
  links_area <- gsub(pattern = " - ", replacement = "%", x = links_area)
  # Legacy code allows reversed (i.e. not sorted) coefficient in a binding constraint 
  links_area_reversed <- gsub(pattern = "(^.*)%(.*$)", replacement = "\\2%\\1", x = links_area)
  if (length(links_area) > 0) {
    bc_not_remove_link <- detect_pattern_in_binding_constraint(pattern = c(links_area, links_area_reversed), opts = opts)
  }
  
  # Cluster
  bc_not_remove_cluster <- character(0)
  clusters <- readClusterDesc(opts = opts)
  clusters_area <- clusters[clusters$area == name, c("area", "cluster")]
  if (nrow(clusters_area) > 0) {  
    bc_not_remove_cluster <- detect_pattern_in_binding_constraint(pattern = paste0(clusters_area$area, ".", clusters_area$cluster), opts = opts)
  }
  
  bc_not_remove <- union(bc_not_remove_cluster, bc_not_remove_link)
  if (!identical(bc_not_remove, character(0))) {
    message("The following binding constraints have the area to remove in a coefficient : ", paste0(bc_not_remove, collapse = ", "))
    stop("Can not remove the area ", name)
  }
}
