#Copyright © 2016 RTE Réseau de transport d’électricité


context("Function createLink")



# Setup study -------------------------------------------------------------


path <- tempdir()
# Unzip the study
setup_study(path, sourcedir)
# set simulation path in mode input
opts <- antaresRead::setSimulationPath(studyPath, 'input')




# Tests -------------------------------------------------------------------

test_that("Create a new link", {
  
  areas <- sort(sample(x = getOption("antares")$areaList, size = 2))
  
  createLink(from = areas[1], to = areas[2])
  
  expect_true(paste(areas, collapse = " - ") %in% levels(antaresRead::getLinks()))
})



# End ---------------------------------------------------------------------


# remove temporary study
unlink(x = file.path(path, "test_case"), recursive = TRUE)
