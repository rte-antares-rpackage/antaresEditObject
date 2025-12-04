
#' @title Create an empty Antares study
#' 
#' @description Create study on disk or with AntaREST server through the API.
#'
#' @param path Path where to create study, it should be an empty directory,
#'  if it doesn't exist, it'll be created.
#' @param study_name Name of the study.
#' @param antares_version Antares number version.
#' 
#' @section Warning: 
#' From **Antares version 9.2** onwards, versioning is only done with one number 
#' for the major version number and a two-digit number for the minor 
#' version number (e.g. 9.2, 9.35, 10.58, ...).
#'
#' @return Result of [antaresRead::setSimulationPath()] or [antaresRead::setSimulationPathAPI()] accordingly.
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
#' # with default values 
#' createStudy("path/to/simulation", 
#'   study_name = "my_study", 
#'   antares_version = "8.2.0")
#'   
#' # with Antares study version >= 9.2 (max 2 digits, ex : "9.25")  
#' createStudy("path/to/simulation", 
#'   study_name = "my_study", 
#'   antares_version = "9.25")
#' 
#' }
createStudy <- function(path, study_name = "my_study", antares_version = "8.2.0") {
  antares_version <- as.numeric_version(antares_version)
  
  # check format version >= 9
  is_new_version <- .is_version_9(version = antares_version)
  
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
  
  # choose template
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
  
  # read ".antares" file to update
  antares <- paste(readLines(con = file.path(path, "study.antares")), 
                   collapse = "\n")
  
  # specific format from version 9
  if(!is_new_version)
    version_to_write <- gsub(pattern = ".", 
                             replacement = "", 
                             x = antares_version, 
                             fixed = TRUE)
  else
    version_to_write <- antares_version

  # template for file "study.antares"
  antares <- whisker::whisker.render(
    template = antares,
    data = list(
      version = version_to_write,
      study_name = study_name,
      date_created = floor(as.numeric(Sys.time())),
      author = Sys.getenv("USERNAME")
    )
  )
  
  # write meta data
  writeLines(text = antares, con = file.path(path, "study.antares"))
  desktop <- paste(readLines(con = file.path(path, "Desktop.ini")), collapse = "\n")
  desktop <- whisker::whisker.render(
    template = desktop,
    data = list(
      study_name = study_name
    )
  )
  writeLines(text = desktop, con = file.path(path, "Desktop.ini"))
  
  # read study to create meta data object to return then
  opts <- setSimulationPath(path = path)
  
  # add specific directory and files according to version of study to create
  if (antares_version >= as.numeric_version("8.1.0")) 
    activateRES(opts = opts)
  
  if (antares_version >= as.numeric_version("8.6.0")) 
    activateST(opts = opts)
  
  # update actual template for 'generaldata.ini' (>=9.2)
    # as.numeric_version() is not efficient for 2 digits ... 
  if(is_new_version){
    antares_version <- as.numeric(
      as.character(antares_version))
    
    if (antares_version >= 9.2){
      updateOptimizationSettings(shedding.policy = "accurate shave peaks")
      
      # add new section and initiate with value "daily" to keep legacy behavior
      gen_data_file <- readIni("settings/generaldata")
      # >= 9.3.0 : do not write these parameters in generaldata.ini
      if ( antares_version >=9.3) {
        gen_data_file$general[["refreshtimeseries"]] <- NULL
        gen_data_file$general[["refreshintervalload"]] <- NULL
        gen_data_file$general[["refreshintervalhydro"]] <- NULL
        gen_data_file$general[["refreshintervalwind"]] <- NULL
        gen_data_file$general[["refreshintervalthermal"]] <- NULL
        gen_data_file$general[["refreshintervalsolar"]] <- NULL
      }
      new_section <- list(
        "compatibility" = list("hydro-pmax" = "daily"))
      gen_data_file <- append(gen_data_file, new_section)
      
      writeIni(listData = gen_data_file, 
               pathIni = "settings/generaldata", 
               overwrite = TRUE)
    }
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
  
  if(!is_api_study && !file.exists(opts$studyPath))
    stop("Study not found.") 
  
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
    if(prompt_answer == 2)
      return(NULL)
  }
  
  if(is_api_study){
    if(delete_simulation){
      study_path <- gsub(pattern = "\\/raw\\?path=",
                         replacement = "",
                         x = opts$studyPath)
      url <- I(file.path(study_path,"outputs",simulation))
    } else {
      url <- opts$study_id
    }
    api_delete(opts = opts, endpoint = url)
    
  } else {
    path <- opts$studyPath
    if(delete_simulation)
      path <- file.path(path,"output",simulation)
    unlink(path, recursive = TRUE) 
  }
  
  cat(sprintf("\n%s successfully deleted", 
              ifelse(delete_simulation, 
                     "Simulation",
                     "Study")))
}


#' function that ensures the transition to 9
#' @description basic tests on the version's writing format
#' 
#' @param version `character` (eg, "8.2.0" or "9.0")
#' @return `logical` if version >=9
#' @keywords internal
.is_version_9 <- function(version){
  # Split major and minor parts
  antares_version_splitted <- unlist(
    strsplit(
      as.character(version), 
      split = "\\."))
  major <- antares_version_splitted[1]
  
  # check from version 9, minor max two digits 
  is_new_version <- as.numeric(major)>=9
  if(is_new_version){
    minor <- antares_version_splitted[2]
    if(length(antares_version_splitted)>2)
      stop("From Antares version 9, put version like this : '9.0' or '9.12' or '10.25'", 
           call. = FALSE)
    if (nchar(minor) > 2) 
      stop("Invalid antares_version format, good format is like '9.99' (two digits on minor)", 
           call. = FALSE)
  }
  
  return(is_new_version)
}

