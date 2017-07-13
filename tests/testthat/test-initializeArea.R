#Copyright © 2016 RTE Réseau de transport d’électricité

context("Function initializeArea")


path <- tempdir()
# Unzip the study
setup_study(path, sourcedir)

opts <- antaresRead::setSimulationPath(studyPath)

test_that("Cannot initialize a new area if not in 'Input' mode", {
  expect_error(initializeArea(name = "myarea"))
})

opts <- antaresRead::setSimulationPath(studyPath, 'input')

test_that("Initialize a new area", {
  n_before <- length(getOption("antares")$areaList)
  initializeArea(name = "myarea")
  n_after <- length(getOption("antares")$areaList)
  expect_equal(n_before + 1, n_after)
})



test_that("Initialize a new area", {
  initializeArea(name = "myareatoremove")
  removeArea(name = "myareatoremove")
  ra <- checkRemoveArea(area = "secondarea")
  expect_length(ra$areaResiduFiles, 0)
  expect_length(ra$areaResidus, 0)
})


# remove temporary study
unlink(x = file.path(path, "test_case"), recursive = TRUE)

