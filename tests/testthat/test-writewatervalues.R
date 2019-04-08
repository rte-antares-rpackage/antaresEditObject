#Copyright © 2016 RTE Réseau de transport d’électricité

context("Function writewatervalues")


# Setup study -------------------------------------------------------------


path <- tempdir()
# Unzip the study
setup_study(path, sourcedir)
# set simulation path in mode input
opts <- antaresRead::setSimulationPath(studyPath, 'input')



# Tests -------------------------------------------------------------------


test_that("Write new water values", {
  
  area <- sample(x = getOption("antares")$areaList, size = 1)
  
  writeWaterValues(area = area, data = matrix(rep(0, 365*101), nrow = 365), overwrite = FALSE)
  
  values_file <- file.path(path, "test_case", "input", "hydro", "common", "capacity", 
                           paste0("waterValues_", tolower(area), ".txt"))
  
  expect_true(file.size(values_file) > 0)
})



# End ---------------------------------------------------------------------


# remove temporary study
unlink(x = file.path(path, "test_case"), recursive = TRUE)

