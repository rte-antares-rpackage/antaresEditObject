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



# importZipStudyWeb <- function(studyPath, opts = antaresRead::simOptions(), host, token) {
#   
#   if (missing(host)) stop("Please specify an url to antares API host.")
#   if (missing(token)) stop("Please specify your access token.")
#   if (missing(studyPath)) studyPath <- opts$studyPath
#   
#   if (!file.exists(studyPath)) stop("Study not found.")
#   #Check if study is already zipped
#   if (length(grep("*\\.zip$", studyPath)) == 0){
#     backupStudy(opts)
#     studyPath <- paste0(studyPath, ".zip")
#   } 
# 
# toto = base64enc::base64encode(studyPath)
# con_toto <- file(studyPath, "rb")
# toto = readBin(studyPath, "raw", file.info(studyPath)$size)
#   studyId <- api_post(
#     opts = list(host = host, token = token),
#     endpoint = "_import",
#     encode = "raw",
#     query = list(
#       study = toto
#     )
#   )
#   
# }
