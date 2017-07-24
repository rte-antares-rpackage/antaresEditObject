#Copyright © 2016 RTE Réseau de transport d’électricité


context("Function createCluster")



# Setup study -------------------------------------------------------------


path <- tempdir()
# Unzip the study
setup_study(path, sourcedir)
# set simulation path in mode input
opts <- antaresRead::setSimulationPath(studyPath, 'input')




# Tests -------------------------------------------------------------------

test_that("Create a new cluster", {
  
  area <- sample(x = getOption("antares")$areaList, size = 1)
  
  createCluster(area = area, cluster_name = "mycluster")
  
  expect_true("mycluster" %in% levels(antaresRead::readClusterDesc()$cluster))
})



# End ---------------------------------------------------------------------


# remove temporary study
unlink(x = file.path(path, "test_case"), recursive = TRUE)


