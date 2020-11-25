#' Write output value for antares
#'
#' This function write all ouput values for antares
#' 
#' @param data obtain with readAntares
#' @param opts opts simulation path
#' 
#' @examples
#' \dontrun{
#' 
#' library(antaresRead)
#' library(data.table)
#' opts <- setSimulationPath("C:/Users/TitouanRobert/Desktop/Projet/RTE/antares/etude/new/BP19_FB18_2021_60mcAcc/output/20201015-0957eco-test3a")
#' data <- readAntares(links = "all", areas = "all", clusters = "all")
#' writeOutputValues(data)
#' 
#' 
#' }
#' 
#' @export
writeOutputValues <- function(data, opts = NULL){
  
  if(!"antaresDataList" %in% class(data)){
    data <- list(data)
  }
  
  if(is.null(opts)){
    opts <- attributes(data[[1]])$opts
  }
  
  lapply(data, function(data){
    
    typeS <- attributes(data)$type
    
    if(typeS %in% c("areas", "links")){
      file = "values-"
    }
    if(typeS == c("clusters")){
      
      file = "details-"
    } 
    
    type <- substr(typeS, 1, nchar(typeS) - 1)
    
    if(type == "cluster"){
      type2 = "area"
    }else{
      type2 = type
    }
    
    
    if(type == "cluster"){
      typeS2 = "areas"
    }else{
      typeS2 = typeS
    }
    
    ##Load output
    sapply(unique(data[[type2]]), function(sel){
      if(is.null(data$mcYear))data$mcYear <- 0
      sapply(unique(data$mcYear), function(mcY){
        
        
        
        datatp <- data[eval(parse(text = paste0(type2, "=='", sel, "' & mcYear == mcY")))]
        
        sinthesys <- attributes(datatp)$synthesis
        
        if(type == "cluster"){
          
          ts <- attributes(datatp)$timeStep
          
          id <- getIdCols(datatp)
          id <- id[id != "cluster"]
          tmp_formula <- as.formula(paste0(paste0(id, collapse = "+"), "~cluster"))
          datatp <- dcast(datatp, tmp_formula, value.var = c("production", "NP Cost", "NODU"))
          
          class(datatp) <- c(class(datatp), "antaresDataTable")
          attributes(datatp)$synthesis <- sinthesys
          attributes(datatp)$timeStep <- ts
        }
        
        
        
        
        file_name <- paste0(file, attributes(datatp)$timeStep, ".txt")
        file_name_new <- paste0(file, attributes(datatp)$timeStep, "_new.txt")
        
        if(sinthesys){
          yd <- "mc-all"
          file_path <- file.path(opts$simDataPath, yd, typeS2, sel)
          
        }else{
          yd <- "mc-ind"
          year_for_write <- formatC(mcY, width = 5, format = "d", flag = "0")
          file_path <- file.path(opts$simDataPath, yd, year_for_write, typeS2, sel)
        }
        
        file_name_new <- file.path(file_path, file_name_new)
        file_name <- file.path(file_path, file_name)
        
        .writeAntaresOutput(file_name, file_name_new, datatp)
        
      })
    })
    
  })
}
