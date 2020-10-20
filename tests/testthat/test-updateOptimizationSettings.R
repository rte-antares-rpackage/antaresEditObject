
context("Function updateOptimizationSettings")


sapply(studies, function(study) {
  
  setup_study(study, sourcedir)
  opts <- antaresRead::setSimulationPath(studyPath, "input")
  
  
  
  
  test_that("Update an optimization parameter", {
    
    
    updateOptimizationSettings(include.hurdlecosts = "false")
    expect_false(getOption("antares")$parameters$optimization$`include-hurdlecosts`)
    
    updateOptimizationSettings(power.fluctuations = "minimize excursions")
    expect_equal(getOption("antares")$parameters$`other preferences`$`power-fluctuations`, "minimize excursions")
    
    expect_error( updateOptimizationSettings(unit.commitment.mode = "unknown"))
    
  })
  
  # remove temporary study
  unlink(x = file.path(pathstd, "test_case"), recursive = TRUE)
  
})
