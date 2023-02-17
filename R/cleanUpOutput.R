#' @title Clean up output based on geographic trimming
#'
#' @param areas Character. vector of areas (folders). Links will also be cleaned based on getLinks() results
#' @param opts List. simulation options
#'
#' @export
cleanUpOutput <- function(areas = NULL, opts = simOptions()){
  if (!is.null(areas) && length(areas) == 1 && areas == "all") areas <- opts$areaList
  filteringData <- getGeographicTrimming(areas)
  areasFiltering <- rbindlist(filteringData$areas, idcol = T)
  linksFiltering <- rbindlist(filteringData$links, idcol = T)

  lapply(areasFiltering$area, .cleanUpOutputSingle, data = areasFiltering, type = "areas", opts = opts)
  lapply(linksFiltering$link, .cleanUpOutputSingle, data = linksFiltering, type = "links", opts = opts)
  
  print("Output clean up success.")
}


.cleanUpOutputSingle <- function(x, data, type, opts){
  
  synthesis <- unlist(strsplit(data[data[[1]] == x]$synthesis, ", "))
  year_by_year <- unlist(strsplit(data[data[[1]] == x]$year_by_year, ", "))
  
  #mc-all
  curr_path <- file.path(opts$simDataPath, "mc-all", type, x)
  filesCurrent <- list.files(curr_path, full.names = T)
  filesToKeep <- unlist(lapply(as.list(synthesis), function(y){
    filesCurrent[grep(y, gsub(opts$simDataPath, "", filesCurrent))]
  }))
  unlink(setdiff(filesCurrent, filesToKeep))
  
  #mc-ind
  curr_path <- file.path(opts$simDataPath, "mc-ind", sprintf("%05.f", opts$mcYears), type, x)
  filesCurrent <- list.files(curr_path, full.names = T)
  filesToKeep <- unlist(lapply(as.list(year_by_year), function(y){
    filesCurrent[grep(y, gsub(opts$simDataPath, "", filesCurrent))]
  }))
  unlink(setdiff(filesCurrent, filesToKeep))
}