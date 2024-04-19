#' @title Import physical study to Antares Web (managed study)
#' 
#' @description Copy study from an existing workspace into a managed study.
#' NOTE : The study must be present in a workspace (DRD, PPSE..) not just locally. 
#' 
#' @param opts List of simulation parameters returned by the function \code{antaresRead::setSimulationPath}. 
#' If id is not available, \code{antaresRead::searchStudy} will be used to find study.
#' @param host Host of AntaREST server API.
#' @param token API personnal access token.
#' @param outputs Logical. Determine if outputs are copied too.
#' @param groups Character. Add study to groups of Antares Web.
#' @param suffix Character. default is "managedCopy"
#' By default the new study will be : studyname_managedCopy
#' 
#' @return New managed study ID
#' 
#' @export
copyStudyWeb <- function(opts = antaresRead::simOptions(), host, token, 
                         outputs = T, groups = NULL, suffix = "managedCopy") {
  
  if (missing(host)) stop("Please specify an url to antares API host.")
  if (missing(token)) stop("Please specify your access token.")
  if (is.null(opts$study_id)){
    #Extract web path from study path
    folder <- strsplit(opts$studyPath, ":/")[[1]]
    if (is.na(folder[2])) folder <- folder[1] else folder <- folder[2]
    if (startsWith(folder, "//") | startsWith(folder, "\\"))
      folder <- do.call(file.path, as.list(strsplit(folder, "/")[[1]][-c(1,2)]))
    studies <- searchStudy(folder = folder, 
                           host = host, 
                           token = token)[, c("workspace", "folder", "name", "id", "version")]
    print(studies)
    prompt_answer <- menu(studies$folder, title="Select the study to import :")
    study_id <- studies$id[prompt_answer]
  } else study_id <- opts$study_id

  new_study_id <- api_post(
    opts = list(host = host, token = token),
    endpoint = file.path(study_id, "copy"),
    query = list(
      dest = paste(opts$studyName, suffix, sep = "_"),
      with_outputs = outputs,
      groups = groups
    )
  )

  new_study_id
}



#' @title Import a local study to Antares Web
#'
#' @param host Host of AntaREST server API.
#' @param token API personnal access token.
#' @param zipfile_name Name of the zipfile of the study.
#'
#' @template opts
#'
#' @importFrom antaresRead setSimulationPathAPI api_post simOptions
#' @importFrom httr upload_file
#' 
#' @export
#'
importZipStudyWeb <- function(host, token, zipfile_name, opts = antaresRead::simOptions()) {
  
  # Build the destination folder
  dir_study <- unlist(strsplit(opts$studyPath, split = .Platform$file.sep))
  dir_study <- dir_study[seq(length(dir_study) - 1)]
  dir_study <- do.call("file.path", as.list(dir_study))
  
  # Zip the study
  zipfile <- backupStudy(zipfile_name, what = "study", opts = opts, extension = ".zip") 
  
  # Import the study
  studyId <- api_post(
    opts = list(host = host, token = token),
    endpoint = "_import",
    default_endpoint = "v1/studies",
    body = list(study = upload_file(file.path(dir_study, zipfile))),
    encode = "multipart"
  )
  
  opts <- setSimulationPathAPI(host = host, token = token, study_id = studyId, simulation = "input")
  
  return(invisible(opts))
}