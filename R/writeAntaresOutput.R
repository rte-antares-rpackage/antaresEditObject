#' @title Write output value for Antares
#'
#' @description
#' `r antaresEditObject:::badge_api_no()`
#'
#' This function write all output values for an Antares study.
#'
#' @param data obtain with readAntares
#' @param opts
#'   List of simulation parameters returned by the function
#'   [antaresRead::setSimulationPath()]
#'
#' @examples
#' \dontrun{
#'
#' library(antaresRead)
#' library(data.table)
#' opts <- setSimulationPath("my_study")
#' data <- readAntares(links = "all", areas = "all", clusters = "all")
#' writeOutputValues(data)
#'
#' }
#'
#' @export
writeOutputValues <- function(data, opts = NULL) {
  if (!"antaresDataList" %in% class(data)) {
    data <- list(data)
  }
  
  if (is.null(opts)) {
    opts <- attributes(data[[1]])$opts
  }
  assertthat::assert_that(inherits(opts, "simOptions"))
  api_not_implemented(opts)
  
  lapply(data, function(data) {
    typeS <- attributes(data)$type
    
    if (typeS %in% c("areas", "links")) {
      file = "values-"
    }
    if (typeS == c("clusters")) {
      file = "details-"
    }
    if (typeS == c("clustersRes")) {
      file = "details-res-"
    }
    
    type <- substr(typeS, 1, nchar(typeS) - 1)
    
    if (type %in% c("cluster", "clustersRe")) {
      type2 = "area"
    } else{
      type2 = type
    }
    
    
    if (type %in% c("cluster", "clustersRe")) {
      typeS2 = "areas"
    } else{
      typeS2 = typeS
    }
    
    ##Load output
    sapply(unique(data[[type2]]), function(sel) {
      if (is.null(data$mcYear))
        data$mcYear <- 0
      sapply(unique(data$mcYear), function(mcY) {
        datatp <-
          data[eval(parse(text = paste0(
            type2, "=='", sel, "' & mcYear == mcY"
          )))]
        
        sinthesys <- attributes(datatp)$synthesis
        ori_type <- attributes(datatp)$type
        
        if (type %in% c("cluster", "clustersRe")) {
          id <- antaresRead::getIdCols(datatp)
          id <- id[id != "cluster"]
          colValues <- setdiff(colnames(datatp), c("cluster",id))
          ts <- attributes(datatp)$timeStep
          if (ts == "weekly"){
            weekOrder <- unique(datatp[, id, with = F]$week)
            datatp[, year := as.integer(factor(cumsum(week == unique(week)[1]))), by = c("area", "cluster", "mcYear")] #handle double week (first and last)
            id <- c(id, "year")
          } 
          tmp_formula <-
            as.formula(paste0(paste0(id, collapse = "+"), "~cluster"))
          datatp <-
            dcast(datatp,
                  tmp_formula,
                  value.var = colValues)
          if (ts == "weekly") datatp <- datatp[order(year, factor(week, levels = weekOrder))][, year := NULL]
          class(datatp) <- c(class(datatp), "antaresDataTable")
          attributes(datatp)$synthesis <- sinthesys
          attributes(datatp)$timeStep <- ts
          attributes(datatp)$type <- ori_type
        }
        
        
        
        
        file_name <-
          paste0(file, attributes(datatp)$timeStep, ".txt")
        file_name_new <-
          paste0(file, attributes(datatp)$timeStep, "_new.txt")
        
        if (sinthesys) {
          yd <- "mc-all"
          file_path <- file.path(opts$simDataPath, yd, typeS2, sel)
          
        } else{
          yd <- "mc-ind"
          year_for_write <-
            formatC(mcY,
                    width = 5,
                    format = "d",
                    flag = "0")
          file_path <-
            file.path(opts$simDataPath, yd, year_for_write, typeS2, sel)
        }
        
        file_name_new <- file.path(file_path, file_name_new)
        file_name <- file.path(file_path, file_name)
        
        if (ori_type %in% c("areas", "links") || file.exists(file_name))
          .writeAntaresOutput(file_name, file_name_new, datatp, opts = opts)
        
      })
    })
    
  })
}
