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
  params <- readIni(file.path("settings", "generaldata"))
  mc_all <- params$output$synthesis
  mc_ind <- params$general$`year-by-year`

  if (!mc_all) unlink(file.path(opts$simDataPath, "mc-all"))
  if (!mc_ind) unlink(file.path(opts$simDataPath, "mc-ind"))

  if (mc_all | mc_ind){
    lapply(areasFiltering$.id, .cleanUpOutputSingle, data = areasFiltering, 
           type = "areas", mc_all = mc_all, mc_ind = mc_ind, opts = opts)
    lapply(linksFiltering$.id, .cleanUpOutputSingle, data = linksFiltering, 
           type = "links", mc_all = mc_all, mc_ind = mc_ind, opts = opts)
  }

  print("Output clean up success.")
}


.cleanUpOutputSingle <- function(x, data, type, mc_all = T, mc_ind = T, opts){
  
  synthesis <- unlist(strsplit(data[data[[1]] == x]$`filter-synthesis`, ", "))
  year_by_year <- unlist(strsplit(data[data[[1]] == x]$`filter-year-by-year`, ", "))
  
  #mc-all
  if (mc_all){
    curr_path <- file.path(opts$simDataPath, "mc-all", type, x)
    filesCurrent <- list.files(curr_path, full.names = T)
    filesToKeep <- unlist(lapply(as.list(synthesis), function(y){
      filesCurrent[grep(y, gsub(opts$simDataPath, "", filesCurrent))]
    }))
    unlink(setdiff(filesCurrent, filesToKeep))
  }

  #mc-ind
  if (mc_ind){
    curr_path <- file.path(opts$simDataPath, "mc-ind", sprintf("%05.f", opts$mcYears), type, x)
    filesCurrent <- list.files(curr_path, full.names = T)
    filesToKeep <- unlist(lapply(as.list(year_by_year), function(y){
      filesCurrent[grep(y, gsub(opts$simDataPath, "", filesCurrent))]
    }))
    unlink(setdiff(filesCurrent, filesToKeep))
  }
}