#Copyright © 2016 RTE Réseau de transport d’électricité

# Copy the test studies in a temporary folder

pathstd <- tempdir()

sourcedir <- system.file("testdata", package = "antaresRead")

studies <- list.files(sourcedir, pattern = "\\.tar\\.gz$", full.names = TRUE)

# Hack: For some unknown reason, this script is executed at some point of
# the R CMD CHECK before package is correctly installed and tests actually run. 
# The following "if" prevents errors at this step

setup_study <- function(study, sourcedir) {
  if (sourcedir != "") {
    # if (Sys.info()['sysname'] == "Windows") {
    #   untar(file.path(sourcedir, "antares-test-study.tar.gz"), exdir = path, 
    #         extras = "--force-local")
    # } else {
      untar(study, exdir = pathstd)
    # }

    assign("studyPath", file.path(pathstd, "test_case"), envir = globalenv())
    assign("nweeks", 2, envir = globalenv())
  }
}

# <<<<<<< HEAD
# 
# # study v850 ----
# sourcedir850 <- system.file("test_v8", package = "antaresRead")
# 
# setup_study_850 <- function(dir_path){
#   studies850 <- list.files(dir_path, pattern = "\\.tar\\.gz$", full.names = TRUE)
#   studies850 <- studies850[grep(x = studies850, pattern = "v85")]
#   # untar etude
#   path_850 <- file.path(tempdir(), "studyv850")
#   untar(studies850[1], exdir = path_850) # v85
#   study_temp_path <- file.path(path_850, "test_case")  
#   
#   assign("study_temp_path", 
#          file.path(path_850, 
#                    "test_case"), 
#          envir = globalenv())
# }
# 
# 
# 
# =======
# study v860 ----
sourcedir860 <- system.file("test_v8", package = "antaresRead")

setup_study_860 <- function(dir_path){
  studies860 <- list.files(dir_path, pattern = "\\.tar\\.gz$", full.names = TRUE)
  studies860 <- studies860[grep(x = studies860, pattern = "v86")]
  # untar etude
  path_860 <- file.path(tempdir(), "studyv860")
  untar(studies860[1], exdir = path_860) # v86
  study_temp_path <- file.path(path_860, "test_case")
  
  assign("study_temp_path",
         file.path(path_860,
                   "test_case"),
         envir = globalenv())
}
# >>>>>>> 28f01006492e9467bbd5b4697c810c26e889324e
