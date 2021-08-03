

# RES utilities functions -------------------------------------------------


initialize_RES <- function(opts) {
  inputPath <- opts$inputPath
  ren_dir <- file.path(inputPath, "renewables")
  if (dir.exists(ren_dir))
    return(invisible(TRUE))
  dir.create(ren_dir)
  dir.create(file.path(ren_dir, "clusters"))
  areas <- opts$areaList
  for (area in areas) {
    dir.create(file.path(inputPath, "renewables", "clusters", tolower(area)))
    writeLines(character(0), con = file.path(inputPath, "renewables", "clusters", tolower(area), "list.ini"))
  }
  dir.create(file.path(ren_dir, "series"))
  return(invisible(TRUE))
}

is_active_RES <- function(opts) {
  generaldatapath <- file.path(opts$studyPath, "settings", "generaldata.ini")
  generaldata <- readIniFile(file = generaldatapath)
  rgm <- generaldata$`other preferences`$`renewable-generation-modelling`
  !is.null(rgm) && identical(rgm, "clusters")
}

check_active_RES <- function(opts, check_dir = FALSE) {
  if (!is_active_RES(opts)) {
    stop(
      "Cannot create a renewable cluster: parameter renewable-generation-modelling value is not 'clusters'",
      ", please use updateOptimizationSettings() to update that parameter before creating renewable cluster.",
      call. = FALSE
    )
  }
  if (isTRUE(check_dir)) {
    inputPath <- opts$inputPath
    ren_dir <- file.path(inputPath, "renewables")
    if (!dir.exists(ren_dir)) {
      stop(
        "There is no 'renewables' directory in the study, are you sure you have renewable clusters?",
        call. = FALSE
      )
    }
  }
  return(invisible(TRUE))
}
