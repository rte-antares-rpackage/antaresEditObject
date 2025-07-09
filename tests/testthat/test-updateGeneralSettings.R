

context("Function updateGeneralSettings")


sapply(studies, function(study) {
  
  setup_study(study, sourcedir)
  opts <- antaresRead::setSimulationPath(studyPath, "input")
  
  
  
  test_that("Update a general parameter", {
    
    # year-by-year
    current_value <- getOption("antares")$parameters$general$`year-by-year`
    updateGeneralSettings(year.by.year = !current_value, opts = opts)
    new_value <- getOption("antares")$parameters$general$`year-by-year`
    if (current_value) {
      expect_false(new_value)
    } else {
      expect_true(new_value)
    }
    
    # geographic-trimming
    current_value <- getOption("antares")$parameters$general$`geographic-trimming`
    updateGeneralSettings(geographic.trimming = !current_value, opts = opts)
    new_value <- getOption("antares")$parameters$general$`geographic-trimming`
    if (current_value) {
      expect_false(new_value)
    } else {
      expect_true(new_value)
    }
    
    # thematic-trimming
    current_value <- getOption("antares")$parameters$general$`thematic-trimming`
    updateGeneralSettings(thematic.trimming = !current_value, opts = opts)
    new_value <- getOption("antares")$parameters$general$`thematic-trimming`
    if (current_value) {
      expect_false(new_value)
    } else {
      expect_true(new_value)
    }
    
  })
  
  # remove temporary study
  unlink(x = file.path(pathstd, "test_case"), recursive = TRUE)
  
})


# custom-scenario ----
test_that("updateGeneralSettings() : check appearance of property custom-scenario and check if it is written in lowercase", {
  
  ant_version <- "8.2.0"
  st_test <- paste0("my_study_820_", paste0(sample(letters,5),collapse = ""))
  suppressWarnings(opts <- createStudy(path = pathstd, study_name = st_test, antares_version = ant_version))
  
  # custom-scenario (logical)
  expect_false(getOption("antares")$parameters$general$`custom-scenario`)
  updateGeneralSettings(custom.scenario = TRUE, opts = opts)
  expect_true(getOption("antares")$parameters$general$`custom-scenario`)
  # check lower case for a logical value
  lines_generaldata <- readLines(file.path(opts$studyPath, "settings", "generaldata.ini"))
  expect_false(paste0(dicoGeneralSettings("custom.scenario"), " = TRUE") %in% lines_generaldata)
  expect_true(paste0(dicoGeneralSettings("custom.scenario"), " = true") %in% lines_generaldata)
  
  unlink(x = opts$studyPath, recursive = TRUE)
})
