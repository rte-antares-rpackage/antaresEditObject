#Copyright © 2019 RTE Réseau de transport d’électricité


context("Function editCluster")



# Setup study -------------------------------------------------------------


path <- tempdir()
# Unzip the study
setup_study(path, sourcedir)
# Set simulation path in mode input
opts <- antaresRead::setSimulationPath(studyPath, "input")



# Tests -------------------------------------------------------------------

test_that("Edit nominal capacity", {
  editCluster(area = "a", cluster_name = "peak", nominalcapacity = 10600, add_prefix = FALSE)
  res <- antaresRead::readClusterDesc()[area == "a" & cluster == "peak", nominalcapacity]
  expect_equal(res, 10600)
})



# End ---------------------------------------------------------------------


# remove temporary study
unlink(x = file.path(path, "test_case"), recursive = TRUE)


