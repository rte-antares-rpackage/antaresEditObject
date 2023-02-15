#' @title Clean up output based on geographic trimming
#'
#' @param areas Character. vector of areas (folders). Links will also be cleaned based on getLinks() results
#' @param opts List. simulation options
#'
#' @export
cleanUpOutput <- function(areas = NULL, opts = simOptions()){
  if (!is.null(areas) && areas == "all") areas <- opts$areaList
  filteringData <- getGeographicTrimming(areas)
  areasFiltering <- as.data.table(ldply(names(filteringData$areas), function(x){
    data.table(area = x, 
               synthesis = filteringData$areas[[x]][[1]], 
               year_by_year = filteringData$areas[[x]][[2]])}))
  linksFiltering <- as.data.table(ldply(names(filteringData$links), function(x){
    data.table(link = x, 
               synthesis = filteringData$links[[x]][[1]], 
               year_by_year = filteringData$links[[x]][[2]])}))

  lapply(areasFiltering$area, .cleanUpOutputSingle, data = areasFiltering, type = "areas", opts = opts)
  lapply(linksFiltering$link, .cleanUpOutputSingle, data = linksFiltering, type = "links", opts = opts)

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
  
  filesToKeep
}