
context("Function writeSeriesPrepro")



# Setup study -------------------------------------------------------------


path <- tempdir()
# Unzip the study
setup_study(path, sourcedir)
# set simulation path in mode input
opts <- antaresRead::setSimulationPath(studyPath, 'input')




# Tests -------------------------------------------------------------------


test_that("Write new prepro data", {
  
  area <- sample(x = getOption("antares")$areaList, size = 1)
  
  M <- matrix(rep(0.3, 12*6), nrow = 12)
  V1 <- rep(2, 8760)
  
  writeSeriesPrepro(area = area, type = "wind",
                    coefficients = M,
                    translation =  V1)
  
  coefficients_file <- file.path(path, "test_case", "input", "wind", "prepro",
                                 area, "data.txt")
  
  translation_file <- file.path(path, "test_case", "input", "wind", "prepro",
                                area, "translation.txt")
  
  expect_equal(
    list(fread(coefficients_file), fread(translation_file)),
    list(as.data.table(M), as.data.table(V1))
  )
})


# End ---------------------------------------------------------------------


# remove temporary study
unlink(x = file.path(path, "test_case"), recursive = TRUE)


