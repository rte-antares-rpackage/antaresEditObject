

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
    
    createDistrict(
      name = "MyDistrict2", 
      apply_filter = "remove-all", 
      add_area = areas,
      output = TRUE
    )
    
    expect_true("mydistrict" %in% antaresRead::getDistricts())
    expect_true("mydistrict2" %in% antaresRead::getDistricts())
    
    sets <- readIniFile(file = file.path(opts[["inputPath"]], "areas", "sets.ini"))
    
    sets_f <- sets[["MyDistrict"]]
    expect_true("output" %in% names(sets_f))
    expect_false(sets_f[["output"]])
    expect_true("apply-filter" %in% names(sets_f))
    expect_equal(sets_f[["apply-filter"]], "add-all")
    expect_equal(length(sets_f[names(sets_f) == "-"]), length(areas))
    
    sets_f <- sets[["MyDistrict2"]]
    expect_true("output" %in% names(sets_f))
    expect_true(sets_f[["output"]])
    expect_true("apply-filter" %in% names(sets_f))
    expect_equal(sets_f[["apply-filter"]], "remove-all")
    expect_equal(length(sets_f[names(sets_f) == "+"]), length(areas))
  }) 

  test_that("Edit a district", {
  
    areas <- sort(sample(x = getOption("antares")$areaList, size = 2))
    
    expect_error(  
      editDistrict(
        name = "not_a_district",
        add_area = areas
      )
      , regexp = "No district not_a_district in the study."
    )
    
    expect_error(  
      editDistrict(
        name = "MyDistrict", 
        apply_filter = "remove-all", 
        remove_area = areas,
        add_area = areas
      )
      , regexp = "You can not use 'add_area' and 'remove_area' at the same time"
    )
    
    expect_error(  
      editDistrict(
        name = "MyDistrict", 
        apply_filter = "remove-all", 
        remove_area = c(areas, "fake_area")
      )
      , regexp = "Invalid area in 'remove_area'"
    )

    expect_error(  
      editDistrict(
        name = "MyDistrict", 
        apply_filter = "remove-all", 
        add_area = c(areas, "fake_area")
      )
      , regexp = "Invalid area in 'add_area'"
    )
    
    editDistrict(name = "MyDistrict",
                 output = TRUE,
                 comments = "my comment",
                 opts = simOptions()
                 )
    
    new_areas <- sample(x = getOption("antares")$areaList, size = 4)
    editDistrict(name = "MyDistrict2",
                 add_area = new_areas,
                 opts = simOptions()
                 )
    sets <- readIniFile(file = file.path(opts[["inputPath"]], "areas", "sets.ini"))
    
    sets_f <- sets[["MyDistrict"]]
    expect_true("output" %in% names(sets_f))
    expect_true(sets_f[["output"]])
    expect_true("comments" %in% names(sets_f))
    expect_equal(sets_f[["comments"]], "my comment")
    
    sets_f <- sets[["MyDistrict2"]]
    expect_true("output" %in% names(sets_f))
    expect_true(sets_f[["output"]])
    expect_equal(length(sets_f[names(sets_f) == "+"]), length(new_areas))
  })
  
  test_that("Remove a district", {
    
    removeDistrict(
      name = "MyDistrIcT", 
      opts = simOptions()
    )
    
    expect_false("mydistrict" %in% antaresRead::getDistricts())
  })
  
  # remove temporary study
  unlink(x = file.path(pathstd, "test_case"), recursive = TRUE)
  
})

