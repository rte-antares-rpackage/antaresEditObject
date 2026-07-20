test_that("Check consistency of dictionary of the compatibility settings", {
  
  keys <- c("hydro.pmax", "hydro.rule.curves")
  
  values <- sapply(keys, FUN = dicoCompatibilitySettings, simplify = FALSE)
  properties <- sapply(values, "[[", "property")
  expect_false(any(sapply(properties, is.null)))
})


test_that("hydro.pmax and hydro.rule.curves parameters available only if version >= 9.2 or 10.1, update multiple properties and check log message if value is not authorized", {
  
  ant_version <- "8.8.0"
  st_test <- paste0("my_study_880_", paste0(sample(letters,5),collapse = ""))
  suppressWarnings(opts <- createStudy(path = pathstd, study_name = st_test, antares_version = ant_version))
  expect_error(updateCompatibilitySettings(hydro.pmax = "daily"),
               regexp = "updateCompatibilitySettings: hydro.pmax parameter is only available if using Antares >= 9.2"
               )
  unlink(x = opts$studyPath, recursive = TRUE)
  
  ant_version <- "9.2"
  st_test <- paste0("my_study_920_", paste0(sample(letters,5),collapse = ""))
  suppressWarnings(opts <- createStudy(path = pathstd, study_name = st_test, antares_version = ant_version))
  updateCompatibilitySettings(hydro.pmax = "hourly", opts = opts)
  expect_equal(getOption("antares")$parameters$compatibility$`hydro-pmax`, "hourly")
  
  expect_error(updateCompatibilitySettings(hydro.pmax = "unauthorized-value", opts = opts),
               regexp = " is not an authorized value"
              )
  
  unlink(x = opts$studyPath, recursive = TRUE)
  
  ## ajouter V10
  ant_version <- "9.3"
  st_test <- paste0("my_study_930_", paste0(sample(letters,5),collapse = ""))
  suppressWarnings(opts <- createStudy(path = pathstd, study_name = st_test, antares_version = ant_version))
  expect_error(updateCompatibilitySettings(hydro.rule.curves = "scenarized"),
               regexp = "updateCompatibilitySettings: hydro.pmax parameter is only available if using Antares >= 10.1"
               )
  unlink(x = opts$studyPath, recursive = TRUE)
  
  ant_version <- "10.1"
  st_test <- paste0("my_study_101_", paste0(sample(letters,5),collapse = ""))
  suppressWarnings(opts <- createStudy(path = pathstd, study_name = st_test, antares_version = ant_version))
  updateCompatibilitySettings(hydro.rule.curves = "scenarized", opts = opts)
  expect_equal(getOption("antares")$parameters$compatibility$`hydro-rule-curves`, "scenarized")
  
  expect_error(updateCompatibilitySettings(hydro.rule.curves = "unauthorized-value", opts = opts),
               regexp = " is not an authorized value"
              )
  
  unlink(x = opts$studyPath, recursive = TRUE)
})
