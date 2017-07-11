#' @title Create a backup with an Antares Study
#' 
#' @description Save an Antares Study or only inputs in a .tar.gz file
#'
#' @param backupfile Name of the backup, without extension.
#' @param what Which folder to save, \code{input} for the input folder
#'  or \code{study} for the whole study.
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
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
backupStudy <- function(backupfile, what = c("input", "study"), opts = antaresRead::simOptions()) {
  
  what <- match.arg(arg = what)
  
  assertthat::assert_that(class(opts) == "simOptions")
  assertthat::assert_that(!is.null(opts$studyPath) && dir.exists(opts$studyPath))
  
  if (missing(backupfile))
    backupfile <- ifelse(what == "study", opts$studyName, what)
  
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
    utils::tar(
      tarfile = paste0(backupfile, ".tar.gz"),
      files = zip_files,
      compression = "gzip"
    )
    , error = function(e) {
      stop("Could not write ", shQuote(paste0(backupfile, ".tar.gz")), " [", e$message, "]")
    }
    , finally = {
      setwd(curr_wd)
  })
  
  res <- file.path(zip_dir, paste0(backupfile, ".tar.gz"))
  
  if( !file.exists(res) ){
    stop("Failed to create backup", call. = FALSE)
  }
  
  res
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
