#Copyright © 2016 RTE Réseau de transport d’électricité


context("Functions get and set playlist")



# Setup study -------------------------------------------------------------


path <- tempdir()
# Unzip the study
setup_study(path, sourcedir)
# set simulation path in mode input
opts <- antaresRead::setSimulationPath(studyPath, 'input')





# Tests -------------------------------------------------------------------


test_that("Update playlist", {
  
  setPlaylist(playlist = 1, opts = opts)
  expect_length(getPlaylist(opts = opts), 1)
  expect_equal(getPlaylist(opts = opts), 1)
  
  setPlaylist(playlist = 2, opts = opts)
  expect_length(getPlaylist(opts = opts), 1)
  expect_equal(getPlaylist(opts = opts), 2)
  
  
  setPlaylist(playlist = c(1,2), opts = opts)
  expect_length(getPlaylist(opts = opts), 2)
  expect_equal(getPlaylist(opts = opts), c(1,2))
})
