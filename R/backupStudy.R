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
#' @param compression_level "int" A number between 1 and 9 (quality of compression only used for `.zip` archive). 
#' See details below for more information (default to 5, fast and good compression).   
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#' @param extension Default is \code{.zip}.
#'
#' @details
#' Parameter `compression_level` is used with function [zip::zip()] 
#'
#' @return The path of the backup
#' @export
#' 
#' @importFrom utils tar
#' @importFrom assertthat assert_that
#' @importFrom zip zip
#'
#' @examples
#' \dontrun{
#' 
#' backupStudy()
#' }
backupStudy <- function(
    backupfile,
    what = c("study", "input"),
    compression_level = 5,
    opts = antaresRead::simOptions(),
    extension = c(".zip", ".tar.gz")
) {
  what <- match.arg(what)
  extension <- match.arg(extension)
  
  stopifnot(inherits(opts, "simOptions"))
  api_not_implemented(opts)
  
  target <- switch(
    what,
    study = opts$studyPath,
    input = opts$inputPath
  )
  
  if (missing(backupfile))
    backupfile <- basename(target)
  
  archive_name <- paste0(backupfile, extension)
  
  existing_archive <- file.path(dirname(target), paste0(backupfile, extension))
  if (file.exists(existing_archive))
    stop("Archive already exists: ", archive_name, call. = FALSE)
  
  if (extension == ".zip") {
    zip::zip(
      zipfile = archive_name,
      files = basename(target),
      root = dirname(target),
      compression_level = compression_level
    )
    
  } else {
    tarfile_path <- file.path(dirname(target), archive_name)
    
    old_wd <- setwd(dirname(target))
    on.exit(setwd(old_wd), add = TRUE)
    
    utils::tar(
      tarfile = archive_name,
      files = basename(target),
      compression = "gzip"
    )
    
  }
  
  existing_archive
}


# backupStudy_old <- function(backupfile, what = "study", 
#                             opts = antaresRead::simOptions(), extension = ".zip") {
#   
#   assertthat::assert_that(what %in% c("study", "input"))  
#   assertthat::assert_that(inherits(opts, "simOptions"))
#   assertthat::assert_that(extension %in% c(".tar.gz", ".zip"))
#   api_not_implemented(opts)
#   assertthat::assert_that(!is.null(opts$studyPath) && dir.exists(opts$studyPath))
#   
#   if (missing(backupfile))
#     backupfile <- ifelse(what == "study", opts$studyPath, what)
#   
#   # backupfile <- file.path(dirname(opts$studyPath), paste0(backupfile, ".tar.gz"))
#   
#   curr_wd <- getwd()
#   if (what == "study") {
#     zip_dir <- dirname(opts$studyPath)
#     zip_files <-  basename(opts$studyPath)
#   } else {
#     zip_dir <- dirname(opts$inputPath)
#     zip_files <-  basename(opts$inputPath)
#   }
#   setwd(zip_dir)
#   
#   if (file.exists(paste0(backupfile, extension))) stop("Backup already exists in this folder.")
#   tryCatch(
#     if (extension == ".tar.gz")
#       utils::tar(tarfile = paste0(backupfile, ".tar.gz"), files = zip_files, compression = "gzip")
#     else utils::zip(zipfile = paste0(backupfile, ".zip"), files = zip_files)
#     , error = function(e) {
#       stop("Could not write ", shQuote(paste0(backupfile, extension)), " [", e$message, "]")
#     }
#     , finally = {
#       setwd(curr_wd)
#     })
#   
#   return (paste0(backupfile, extension))
#   
# }

