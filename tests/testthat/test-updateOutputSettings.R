
context("Function updateOuputSettings")


sapply(studies, function(study) {
  
  setup_study(study, sourcedir)
  opts <- antaresRead::setSimulationPath(studyPath, "input")
  
  
  
  test_that("Update an output parameter", {
    
    updateOutputSettings(synthesis = FALSE)
    
    expect_false(getOption("antares")$parameters$output$synthesis)
    
  })
  
  # remove temporary study
  unlink(x = file.path(pathstd, "test_case"), recursive = TRUE)
  
})