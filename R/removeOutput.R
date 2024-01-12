
#' @title Delete an Antares Simulation
#' 
#' @description 
#' `r antaresEditObject:::badge_api_ok()`
#' 
#' Delete an Antares Simulation
#' 
#' @param name
#'   Name of the simulation
#' @param opts
#'   List of simulation parameters returned by the function
#'   [antaresRead::setSimulationPath()]
#' 
#' @importFrom antaresRead api_delete
#' @importFrom assertthat assert_that
#' @export
#' 
removeOutput <- function(name,opts){
  assert_that(inherits(opts, "simOptions"))
  
  if(is_api_study(opts)){
    study_path <- gsub(pattern = "\\/raw\\?path=",replacement = "",x = opts$studyPath)
    url <- file.path(study_path,"outputs",name)
    api_delete(opts = opts, endpoint = I(url))
  } else {
    path <- file.path(opts$studyPath,"output",name)
    unlink(path,recursive = T)
  }
  
  res <- update_opts(opts)

  return(invisible(res))
}