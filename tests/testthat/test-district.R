

context("Function createDistrict")


sapply(studies, function(study) {
  
  setup_study(study, sourcedir)
  opts <- antaresRead::setSimulationPath(studyPath, "input")
  
  
  test_that("Create a new district", {
    
    areas <- sort(sample(x = getOption("antares")$areaList, size = 2))
    
    createDistrict(
      name = "MyDistrict", 
      apply_filter = "add-all", 
      remove_area = areas
    )
    
    expect_true("mydistrict" %in% antaresRead::getDistricts())
  })
  
  test_that("Remove a district", {
    
    expect_warning(removeDistrict(name = "mydistrict", opts = simOptions()),
                   regexp = "No district was removed. Please provide the exact name of the district."
                   )
    
    removeDistrict(
      name = "MyDistrict", 
      opts = simOptions()
    )
    
    expect_false("mydistrict" %in% antaresRead::getDistricts())
  })
  
  # remove temporary study
  unlink(x = file.path(pathstd, "test_case"), recursive = TRUE)
  
})

