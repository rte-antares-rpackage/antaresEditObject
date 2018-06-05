

context("Function createDistrict")



# Setup study -------------------------------------------------------------


path <- tempdir()
# Unzip the study
setup_study(path, sourcedir)
# set simulation path in mode input
opts <- antaresRead::setSimulationPath(studyPath, 'input')




# Tests -------------------------------------------------------------------

test_that("Create a new district", {
  
  areas <- sort(sample(x = getOption("antares")$areaList, size = 2))
  
  createDistrict(
    name = "mydistrict", 
    apply_filter = "add-all", 
    remove_area = areas
  )
  
  expect_true("mydistrict" %in% antaresRead::getDistricts())
})




# End ---------------------------------------------------------------------


# remove temporary study
unlink(x = file.path(path, "test_case"), recursive = TRUE)

