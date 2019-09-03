
context("Function writeInputTS")



# Setup study -------------------------------------------------------------


path <- tempdir()
# Unzip the study
setup_study(path, sourcedir)
# set simulation path in mode input
opts <- antaresRead::setSimulationPath(studyPath, 'input')




# Tests -------------------------------------------------------------------


test_that("Write new input time series", {
  
  area <- sample(x = getOption("antares")$areaList, size = 1)
  
  M <- matrix(c(rep(8, 8760), rep(5.1, 8760)), nrow = 8760)
  
  writeInputTS(area = area, type = "thermal", data = M)
  
  values_file <- file.path(path, "test_case", "input", "thermal", "series",
                           paste0("thermal_", area, ".txt"))
  
  expect_equal(fread(values_file), as.data.table(M))
})


# End ---------------------------------------------------------------------


# remove temporary study
unlink(x = file.path(path, "test_case"), recursive = TRUE)


