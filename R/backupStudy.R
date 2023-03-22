#' @title Create a backup with an Antares Study
#' 
#' @description 
#' `r antaresEditObject:::badge_api_no()`
#' 
#' Save an Antares Study or only inputs in a \code{.tar.gz} or \code{.zip} file
#'
#' @param backupfile Name of the backup, without extension. If missing, 
#' either the name of the study or 'input' according argument \code{what}.
#' @param what Which folder to save, \code{input} for the input folder
#'  or \code{study} for the whole study.
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#' @param extension Defaut is {.zip}.
#'
#' @return The path of the backup
#' @export
#' 
#' @importFrom utils tar
#' @importFrom assertthat assert_that
#'
#' @examples
#' \dontrun{
#' backupStudy()
#' }
backupStudy <- function(backupfile, what = "study", 
                        opts = antaresRead::simOptions(), extension = ".zip") {
  
  what <- match.arg(arg = what)
  
  assertthat::assert_that(inherits(opts, "simOptions"))
  assertthat::assert_that(extension %in% c(".tar.gz", ".zip"))
  api_not_implemented(opts)
  assertthat::assert_that(!is.null(opts$studyPath) && dir.exists(opts$studyPath))
  
  if (missing(backupfile))
    backupfile <- ifelse(what == "study", opts$studyPath, what)
  
  # backupfile <- file.path(dirname(opts$studyPath), paste0(backupfile, ".tar.gz"))
  
  curr_wd <- getwd()
  if (what == "study") {
    zip_dir <- dirname(opts$studyPath)
    zip_files <-  basename(opts$studyPath)
  } else {
    zip_dir <- dirname(opts$inputPath)
    zip_files <-  basename(opts$inputPath)
  }
  setwd(zip_dir)
  
  tryCatch(
    if (extension == ".tar.gz")
      utils::tar(tarfile = paste0(backupfile, ".tar.gz"), files = zip_files, compression = "gzip")
    else utils::zip(zipfile = paste0(backupfile, ".zip"), files = zip_files)
    , error = function(e) {
      stop("Could not write ", shQuote(paste0(backupfile, extension)), " [", e$message, "]")
    }
    , finally = {
      setwd(curr_wd)
  })
  
  return (paste0(backupfile), extension)
  
}
# backupSimulation <- function(backupfile, opts = antaresRead::simOptions()) {
#   
#   if (missing(backupfile)) 
#     backupfile <- paste0(opts$studyName, "_backup")
#   
#   backupfile <- file.path(dirname(opts$studyPath), paste0(backupfile, ".tar.gz"))
#   
#   res <- tryCatch({
#     tar(
#       tarfile = backupfile,
#       files = opts$studyPath,
#       compression = "gzip"
#     )
#     TRUE
#   }, error = function(e) FALSE)
#   
#   if (res) {
#     cat(paste0("Backup successfully created ! (", backupfile, ")"), "\n")
#   } else {
#     cat("Failed to create backup", "\n")
#   }
#   return(res)
# }
