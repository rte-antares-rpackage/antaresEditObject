

context("Function updateGeneralSettings")


sapply(studies, function(study) {
  
  setup_study(study, sourcedir)
  opts <- antaresRead::setSimulationPath(studyPath, "input")
  
  
  
  test_that("Update a general parameter", {
    
    # year-by-year
    expect_true(getOption("antares")$parameters$general$`year-by-year`)
    updateGeneralSettings(year.by.year = FALSE)
    expect_false(getOption("antares")$parameters$general$`year-by-year`)
    
    # geographic-trimming
    expect_true(getOption("antares")$parameters$general$`geographic-trimming`)
    updateGeneralSettings(geographic.trimming = FALSE)
    expect_false(getOption("antares")$parameters$general$`geographic-trimming`)
  })
  
  # remove temporary study
  unlink(x = file.path(pathstd, "test_case"), recursive = TRUE)
  
})
