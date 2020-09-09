#' Write links data
#'
#' This function write links data for antares output
#' 
#' @param opts opts simulation path
#' @param data obtain with readAntares
#' 
#' @examples
#' \dontrun{
#' library(antaresRead)
#' library(data.table)
#' opts <- setSimulationPath("C:/Users/TitouanRobert/Desktop/Projet/RTE/antares/etude/MAF2025_S3_FBCOREMOD")
#' data <- readAntares(mcYears = 179, links = "all")
#' write_link_output_values(opts, data)
#' 
#' }
#' 
#' @export
write_link_output_values <- function(opts,
                                     data){
  ##Load output and keep area
  sapply(unique(data$link), function(link_sel){
    if(is.null(data$mcYear))data$mcYear <- 0
    sapply(unique(data$mcYear), function(mcY){
      
      datatp <- data[link == link_sel & mcYear == mcY]
      file_name <- paste0("values-", attributes(datatp)$timeStep, ".txt")
      file_name_new <- paste0("values-", attributes(datatp)$timeStep, "_new.txt")
      
      if(attributes(datatp)$synthesis){
        yd <- "mc-all"
        file_path <- file.path(opts$simDataPath, yd, "links", link_sel)
        
      }else{
        yd <- "mc-ind"
        year_for_write <- formatC(mcY, width = 5, format = "d", flag = "0")
        file_path <- file.path(opts$simDataPath, yd, year_for_write, "links", link_sel)
      }
      
      file_name_new <- file.path(file_path, file_name_new)
      file_name <- file.path(file_path, file_name)
      
      .writeAntaresOutput(file_name, file_name_new, datatp)
      
    })
  })
}
