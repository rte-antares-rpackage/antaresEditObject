#Copyright © 2016 RTE Réseau de transport d’électricité


context("Function createArea")



# Setup study -------------------------------------------------------------


path <- tempdir()
# Unzip the study
setup_study(path, sourcedir)
opts <- antaresRead::setSimulationPath(studyPath)




# Tests -------------------------------------------------------------------



test_that("Cannot initialize a new area if not in 'Input' mode", {
  expect_error(createArea(name = "myarea"))
})


# set simulation path in mode input
opts <- antaresRead::setSimulationPath(studyPath, 'input')





test_that("Backup study/input", {
  expect_length(backupStudy(what = "study"), 1)
  expect_length(backupStudy(what = "input"), 1)
})



test_that("Initialize a new area", {
  n_before <- length(getOption("antares")$areaList)
  createArea(name = "myarea")
  n_after <- length(getOption("antares")$areaList)
  expect_equal(n_before + 1, n_after)
  expect_true("myarea" %in% getOption("antares")$areaList)
})





test_that("Remove an area", {
  area2remove <- "myareatoremove"
  createArea(name = area2remove)
  
  ra <- checkRemovedArea(area = area2remove)
  expect_true(length(ra$areaResiduFiles) > 0)
  expect_true(length(ra$areaResidus) > 0)
  
  removeArea(name = area2remove)
  ra <- checkRemovedArea(area = area2remove)
  expect_length(ra$areaResiduFiles, 0)
  expect_length(ra$areaResidus, 0)
})






# End ---------------------------------------------------------------------


# remove temporary study
unlink(x = file.path(path, "test_case"), recursive = TRUE)

