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

# study version > v800 ----
  # new template study for each new release
sourcedir_last_study <- system.file("test_v8", package = "antaresRead")

setup_study_last <- function(dir_path){
  studies <- list.files(dir_path, pattern = "\\.tar\\.gz$", full.names = TRUE)
  studies_last_version <- studies[grep(x = studies, pattern = "v86")]
  # untar etude
  path_last_version <- file.path(tempdir(), "studyv860")
  untar(studies_last_version[1], exdir = path_last_version) # v86
  study_temp_path <- file.path(path_last_version, "test_case")
  
  assign("study_temp_path",
         file.path(path_last_version,
                   "test_case"),
         envir = globalenv())
}

