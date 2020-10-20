

context("Function updateGeneralSettings")


sapply(studies, function(study) {
  
  setup_study(study, sourcedir)
  opts <- antaresRead::setSimulationPath(studyPath, "input")
  
  
  
  test_that("Update a general parameter", {
    
    updateGeneralSettings(year.by.year = FALSE)
    
    expect_false(getOption("antares")$parameters$general$`year-by-year`)
    
  })
  
  # remove temporary study
  unlink(x = file.path(pathstd, "test_case"), recursive = TRUE)
  
})
