#Copyright © 2016 RTE Réseau de transport d’électricité


context("Function updateOptimizationSettings")



# Setup study -------------------------------------------------------------


path <- tempdir()
# Unzip the study
setup_study(path, sourcedir)
# set simulation path in mode input
opts <- antaresRead::setSimulationPath(studyPath, 'input')





# Tests -------------------------------------------------------------------


test_that("Update an optimization parameter", {
  
  
  updateOptimizationSettings(include.hurdlecosts = "false")
  expect_false(getOption("antares")$parameters$optimization$`include-hurdlecosts`)
  
  updateOptimizationSettings(power.fluctuations = "minimize excursions")
  expect_equal(getOption("antares")$parameters$`other preferences`$`power-fluctuations`, "minimize excursions")
  
  expect_error( updateOptimizationSettings(unit.commitment.mode = "unknown"))
  
})
