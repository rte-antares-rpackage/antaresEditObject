
context("Function writeSeriesPrepro")



# Setup study -------------------------------------------------------------


path <- tempdir()
# Unzip the study
setup_study(path, sourcedir)
# set simulation path in mode input
opts <- antaresRead::setSimulationPath(studyPath, 'input')



# Tests -------------------------------------------------------------------

area <- sample(x = getOption("antares")$areaList, size = 1)

coefficients_file <- file.path(path, "test_case", "input", "wind", "prepro",
                               area, "data.txt")

translation_file <- file.path(path, "test_case", "input", "wind", "prepro",
                              area, "translation.txt")

dp_file <- file.path(path, "test_case", "input", "wind", "prepro",
                     area, "k.txt")

conv_file <- file.path(path, "test_case", "input", "wind", "prepro",
                       area, "conversion.txt")


test_that("Write new prepro data", {
  
  M <- matrix(rep(0.3, 12*6), nrow = 12)
  V1 <- rep(2, 8760)
  dp <- matrix(rep(2, 24*12), nrow = 24)
  cv <- matrix(1:8, nrow = 2)
  
  writeSeriesPrepro(area = area, type = "wind",
                    coefficients = M,
                    daily_profile = dp,
                    translation =  V1,
                    conversion = cv)
  
  expect_equal(
    list(fread(coefficients_file), fread(translation_file), fread(dp_file), fread(conv_file)),
    list(as.data.table(M), as.data.table(V1), as.data.table(dp), as.data.table(cv))
  )
  
  expect_error(
    writeSeriesPrepro(area = area, type = "wind",
                      coefficients = M,
                      overwrite = FALSE),
    regexp = "already exist"
  )
})

test_that("Erase prepro data", {
  
  writeSeriesPrepro(area = area, type = "wind",
                    coefficients = character(0),
                    daily_profile = character(0),
                    translation =  character(0),
                    conversion = character(0))
  
  expect_equal(
    list(file.size(coefficients_file), file.size(translation_file), file.size(dp_file), file.size(conv_file)),
    list(0, 0, 0, 0)
  )
})


# End ---------------------------------------------------------------------


# remove temporary study
unlink(x = file.path(path, "test_case"), recursive = TRUE)


