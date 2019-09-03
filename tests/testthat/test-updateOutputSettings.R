
context("Function updateOutputSettings")



# Setup study -------------------------------------------------------------


path <- tempdir()
# Unzip the study
setup_study(path, sourcedir)
# set simulation path in mode input
opts <- antaresRead::setSimulationPath(studyPath, 'input')





# Tests -------------------------------------------------------------------


test_that("Update an output parameter", {
  
  updateOutputSettings(synthesis = FALSE)
  
  expect_false(getOption("antares")$parameters$output$synthesis)
  
})
