#' Create a .tar.gz file with an Antares Study
#'
#' @param backupfile Name of the backup, without extension.
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#'
#' @return TRUE or FALSE accordingly to the success or not of the backup
#' @export
#' 
#' @importFrom utils tar
#' @importFrom assertthat assert_that
#'
#' @examples
#' \dontrun{
#' backupStudy()
#' }
backupSimulation <- function(backupfile, opts = antaresRead::simOptions()) {
  
  assertthat::assert_that(class(opts) == "simOption")
  assertthat::assert_that(!is.null(opts$studyPath) && dir.exists(opts$studyPath))
  
  if (missing(backupfile))
    backupfile <- paste0(opts$studyName, "_backup")
  
  # backupfile <- file.path(dirname(opts$studyPath), paste0(backupfile, ".tar.gz"))
  
  curr_wd <- getwd()
  zip_dir <- dirname(opts$studyPath)
  setwd(zip_dir)
  
  tryCatch(
    utils::tar(
      tarfile = paste0(backupfile, ".tar.gz"),
      files = basename(opts$studyPath),
      compression = "gzip"
    )
    , error = function(e) {
      stop("Could not write ", shQuote(paste0(backupfile, ".tar.gz")), " [", e$message, "]")
    }
    , finally = {
      setwd(curr_wd)
  })
  
  res <- file.path(dirname(opts$studyPath), paste0(backupfile, ".tar.gz"))
  
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
