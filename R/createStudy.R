
#' @title Create an empty Antares study
#' 
#' @description Create study on disk or with AntaREST server through the API.
#'
#' @param path Path where to create study, it should be an empty directory,
#'  if it doesn't exist, it'll be created.
#' @param study_name Name of the study.
#' @param antares_version Antares number version.
#'
#' @return Result of [antaresRead::setSimulationPath()] or [setSimulationPathAPI()] accordingly.
#' @export
#' 
#' @name create-study
#' 
#' @importFrom whisker whisker.render
#' @importFrom utils unzip
#' @importFrom antaresRead setSimulationPath
#'
#' @examples
#' \dontrun{
#' 
#' createStudy("path/to/simulation")
#' 
#' }
createStudy <- function(path, study_name = "my_study", antares_version = "8.2.0") {
  antares_version <- as.numeric_version(antares_version)
  if (!dir.exists(path)) {
    dir.create(path = path, recursive = TRUE)
  } else {
    if (length(list.files(path = path)) > 0) {
      path <- file.path(path, study_name)
      if (!dir.exists(path)) {
        dir.create(path = path, recursive = TRUE)
      }
    }
  }
  if (antares_version < as.numeric_version("6.5.0")) {
    file.copy(
      from = list.files(path = system.file("newStudy", package = "antaresEditObject"), full.names = TRUE),
      to = path, recursive = TRUE
    )
    to_delete <- list.files(path = path, pattern = "ANTARESEDITOBJECT_TODELETE", full.names = TRUE, recursive = TRUE)
    unlink(to_delete)
  } else if (antares_version < as.numeric_version("7.1.0")) {
    unzip(zipfile = system.file("template-antares/antares-study-v700.zip", package = "antaresEditObject"), exdir = path)
  } else if (antares_version < as.numeric_version("7.2.0")){
    unzip(zipfile = system.file("template-antares/antares-study-v710.zip", package = "antaresEditObject"), exdir = path)
  } else if (antares_version < as.numeric_version("8.0.0")){
    unzip(zipfile = system.file("template-antares/antares-study-v720.zip", package = "antaresEditObject"), exdir = path)
  } else {
    unzip(zipfile = system.file("template-antares/antares-study-v800.zip", package = "antaresEditObject"), exdir = path)
  }
  antares <- paste(readLines(con = file.path(path, "study.antares")), collapse = "\n")
  antares <- whisker::whisker.render(
    template = antares,
    data = list(
      version = gsub(pattern = ".", replacement = "", x = antares_version, fixed = TRUE),
      study_name = study_name,
      date_created = floor(as.numeric(Sys.time()))
    )
  )
  writeLines(text = antares, con = file.path(path, "study.antares"))
  desktop <- paste(readLines(con = file.path(path, "Desktop.ini")), collapse = "\n")
  desktop <- whisker::whisker.render(
    template = desktop,
    data = list(
      study_name = study_name
    )
  )
  writeLines(text = desktop, con = file.path(path, "Desktop.ini"))
  opts <- setSimulationPath(path = path)
  if (antares_version >= as.numeric_version("8.1.0")) {
    activateRES(opts = opts)
  }
  
  if (antares_version >= as.numeric_version("8.6.0")) {
    activateST(opts = opts)
  }
  return(invisible(opts))
}




#' @param host Host of AntaREST server API.
#' @param token API personnal access token.
#' @param ... Other query parameters passed to POST request.
#' 
#' @importFrom antaresRead setSimulationPathAPI api_post
#' 
#' @export
#' 
#' @rdname create-study
createStudyAPI <- function(host, token = NULL, study_name = "my_study", antares_version = "8.2.0", ...) {
  studyId <- api_post(
    opts = list(host = host, token = token),
    endpoint = NULL,
    query = dropNulls(list(
      name = study_name,
      version = paste(unlist(as.numeric_version(antares_version)), collapse = ""),
      ...
    ))
  )
  opts <- setSimulationPathAPI(
    host = host,
    study_id = studyId, 
    token = token, 
    simulation = "input"
  )
  setAPImode("sync")
  invisible(opts)
}

#' @title Delete a study or a simulation
#' 
#' @param opts List. study options
#' @param prompt_validation `logical` to put validation message to delete study (default `FALSE`)
#' @param simulation simulation to be deleted (default `NULL`)
#' @importFrom antaresRead api_delete
#' @export
deleteStudy <- function(opts = simOptions(), prompt_validation = FALSE, simulation = NULL){

  delete_simulation <- !is.null(simulation)
  is_api_study <- is_api_study(opts)
  
  if(!file.exists(opts$studyPath)){ stop("Study not found.") }
  
  if(prompt_validation){
    if(delete_simulation){
      prompt_question <- sprintf("Are you sure you want to delete the simulation : %s ?", 
                                 simulation)
    } else {
      prompt_question <- sprintf("Are you sure you want to delete the study : %s (%s)?", 
                                 ifelse(opts$typeLoad == "api", 
                                        opts$study_id, 
                                        opts$studyPath), 
                                 opts$studyName)
    }
    
    prompt_answer <- menu(c("Yes", "No"),title=prompt_question)
    if (prompt_answer == 2){ return() }
  }
  
  if(is_api_study & delete_simulation){
    study_path <- gsub(pattern = "\\/raw\\?path=",replacement = "",x = opts$studyPath)
    url <- I(file.path(study_path,"outputs",simulation))
    api_delete(opts = opts, endpoint = url)
    
  } else if(is_api_study & !delete_simulation){
    url <- opts$study_id
    api_delete(opts = opts, endpoint = url)
    
  } else if(!is_api_study & delete_simulation){ 
    path <- file.path(opts$studyPath,"output",simulation)
    unlink(path, recursive = TRUE) 
    
  } else if(!is_api_study & !delete_simulation){ 
    path <- opts$studyPath
    unlink(path, recursive = TRUE) 
  } 
  
  res <- NULL
  if(is_api_study(opts)){ res <- update_opts(opts) }
  
  cat(sprintf("\n%s successfully deleted", 
              ifelse(delete_simulation, 
                     "Simulation",
                     "Study")))
  
  return(invisible(res))
}

