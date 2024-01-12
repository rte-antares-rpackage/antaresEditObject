
context("Function removeOutput")

sapply(studies, function(study) {
  
  setup_study(study, sourcedir)
  opts <- antaresRead::setSimulationPath(studyPath, 1)
  
  test_that("Delete a simulation", {
    
    testthat::expect_true(file.exists(opts$simPath))
    split_simPath <- strsplit(opts$simPath,"/")[[1]]
    simName <- split_simPath[length(split_simPath)]
    opts <- removeOutput(simName,opts)
    testthat::expect_true(is.null(opts$simPath))
  })
})