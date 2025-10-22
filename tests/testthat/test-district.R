

context("Function createDistrict")


sapply(studies, function(study) {
  
  setup_study(study, sourcedir)
  opts <- antaresRead::setSimulationPath(studyPath, "input")
  
  
  test_that("Create a new district", {
    
    areas <- sort(sample(x = getOption("antares")$areaList, size = 2))
    
    expect_error(  
      createDistrict(
        name = "MyDistrict", 
        apply_filter = "remove-all", 
        remove_area = areas,
        add_area = areas,
        opts = opts
      )
      , regexp = "You can not use 'add_area' and 'remove_area' at the same time"
    )
    
    expect_error(  
      createDistrict(
        name = "MyDistrict", 
        apply_filter = "remove-all", 
        remove_area = c(areas, "fake_area"),
        opts = opts
      )
      , regexp = "Invalid area in 'remove_area'"
    )

    expect_error(  
      createDistrict(
        name = "MyDistrict", 
        apply_filter = "remove-all", 
        add_area = c(areas, "fake_area"),
        opts = opts
      )
      , regexp = "Invalid area in 'add_area'"
    )
    
    expect_error(  
      createDistrict(
        name = "MyDistrict", 
        apply_filter = "remove-all", 
        remove_area = areas,
        opts = opts
      )
      , regexp = "You have to use 'remove_area' with 'apply_filter' set to add-all"
    )

    expect_error(  
      createDistrict(
        name = "MyDistrict", 
        apply_filter = "add-all", 
        add_area = areas,
        opts = opts
      )
      , regexp = "You have to use 'add_area' with 'apply_filter' set to remove-all"
    )
    
    createDistrict(
      name = "MyDistrict", 
      apply_filter = "add-all", 
      remove_area = areas
    )
    
    expect_true("mydistrict" %in% antaresRead::getDistricts())
  })
  
  test_that("Remove a district", {
    
    removeDistrict(
      name = "MyDistrict", 
      opts = simOptions()
    )
    
    expect_false("mydistrict" %in% antaresRead::getDistricts())
  })
  
  # remove temporary study
  unlink(x = file.path(pathstd, "test_case"), recursive = TRUE)
  
})

