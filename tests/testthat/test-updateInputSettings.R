#Copyright © 2019 RTE Réseau de transport d’électricité


context("Function updateInputSettings")



# Setup study -------------------------------------------------------------


path <- tempdir()
# Unzip the study
setup_study(path, sourcedir)
# Set simulation path in mode input
opts <- antaresRead::setSimulationPath(studyPath, "input")



# Tests -------------------------------------------------------------------

test_that("Update input settings", {
  
  updateInputSettings(import = c("hydro", "thermal"))
  
  expect_identical(getOption("antares")$parameters$input$import, c("hydro, thermal"))
  
})

# End ---------------------------------------------------------------------


# remove temporary study
unlink(x = file.path(path, "test_case"), recursive = TRUE)


