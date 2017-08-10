#Copyright © 2016 RTE Réseau de transport d’électricité


context("Function updateGeneralSettings")



# Setup study -------------------------------------------------------------


path <- tempdir()
# Unzip the study
setup_study(path, sourcedir)
# set simulation path in mode input
opts <- antaresRead::setSimulationPath(studyPath, 'input')





# Tests -------------------------------------------------------------------


test_that("Update a general parameter", {
  
  updateGeneralSettings(year.by.year = FALSE)

  expect_false(getOption("antares")$parameters$general$`year-by-year`)
  
})
