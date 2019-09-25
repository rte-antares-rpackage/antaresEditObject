#Copyright © 2019 RTE Réseau de transport d’électricité


context("scenarioBuilder")



# Setup study -------------------------------------------------------------


path <- tempdir()
# Unzip the study
setup_study(path, sourcedir)
# Set simulation path in mode input
opts <- antaresRead::setSimulationPath(studyPath, "input")



# Tests -------------------------------------------------------------------

test_that("scenarioBuilder works", {
  
  sbuilder <- scenarioBuilder(
    n_scenario = 2,
    n_mc = 2,
    areas = c("fr", "it", "be"),
    areas_rand = c("it", "be")
  )
  
  sb <- structure(
    c("1", "rand", "rand", "2", "rand", "rand"),
    .Dim = 3:2,
    .Dimnames = list(c("fr", "it", "be"), NULL)
  )
  
  expect_identical(sbuilder, sb)
  
})

test_that("scenarioBuilder works when areas_rand has length 1", {
  
  sbuilder <- scenarioBuilder(
    n_scenario = 2,
    n_mc = 2,
    areas = c("fr", "it", "be"),
    areas_rand = "it"
  )
  
  sb <- structure(
    c("1", "rand", "1", "2", "rand", "2"),
    .Dim = 3:2,
    .Dimnames = list(c("fr", "it", "be"), NULL)
  )
  
  expect_identical(sbuilder, sb)
  
})

test_that("Warning is thrown when n_mc differs from nbyears", {
  
  expect_warning(scenarioBuilder(
    n_scenario = 2,
    n_mc = 3,
    areas = c("fr", "it", "be"),
    areas_rand = "it"
  ))
  
})



# End ---------------------------------------------------------------------


# remove temporary study
unlink(x = file.path(path, "test_case"), recursive = TRUE)


