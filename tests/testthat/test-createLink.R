

context("Function createLink")


sapply(studies, function(study) {
  
  setup_study(study, sourcedir)
  opts <- antaresRead::setSimulationPath(studyPath, "input")
  
  
  test_that("Create a new link", {
    
    areas <- sort(sample(x = getOption("antares")$areaList, size = 2))
    
    createLink(from = areas[1], to = areas[2], overwrite = TRUE)
    
    expect_true(paste(areas, collapse = " - ") %in% levels(antaresRead::getLinks()))
  })
  
  
  test_that("Create a new link - respect alphabetical order", {
    
    areas <- sort(sample(x = getOption("antares")$areaList, size = 2))
    
    createLink(from = areas[2], to = areas[1], overwrite = TRUE)
    expect_true(paste(areas, collapse = " - ") %in% levels(antaresRead::getLinks()))
    
    removeLink(from = areas[2], to = areas[1])
    expect_false(paste(areas, collapse = " - ") %in% levels(antaresRead::getLinks()))
  })
  
  
  
  
  areas <- sort(sample(x = getOption("antares")$areaList, size = 2))
  
  
  test_that("Create a link with default properties", {
    
    if (is_antares_v7(opts)) {
      dataLink <- matrix(
        data = c(rep(0, 8760), rep(7500, 8760), rep(0, 8760*6)),
        ncol = 8
      ) 
    } else {
      dataLink <- matrix(
        data = c(rep(0, 8760), rep(7500, 8760), rep(0, 8760*3)),
        ncol = 5
      )
    }
    
    createLink(
      from = areas[1], 
      to = areas[2], 
      propertiesLink = propertiesLinkOptions(hurdles_cost = FALSE, transmission_capacities = "enabled"), 
      dataLink = dataLink,
      overwrite = TRUE
    )
    
    expect_true(paste(areas, collapse = " - ") %in% levels(antaresRead::getLinks()))
  })
  
  
  
  
  test_that("Remove a link", {
    
    removeLink(from = areas[1], to = areas[2])
    
    expect_false(paste(areas, collapse = " - ") %in% levels(antaresRead::getLinks()))
  })
  
  
  
  
  test_that("Remove a link that doesn't exist", {
    expect_message(removeLink(from = "myimaginaryarea", to = "myimaginaryareabis"))
  })
  
  # remove temporary study
  unlink(x = file.path(pathstd, "test_case"), recursive = TRUE)
  
})


test_that("removeLink() in 8.2.0 : check if the expected files are deleted/updated", {
  
  ant_version <- "8.2.0"
  st_test <- paste0("my_study_820_", paste0(sample(letters,5),collapse = ""))
  suppressWarnings(opts <- createStudy(path = pathstd, study_name = st_test, antares_version = ant_version))
  area <- "zone51"
  area2 <- "aone51"
  createArea(area)
  createArea(area2)
  opts <- setSimulationPath(opts$studyPath, simulation = "input")
  
  
  # with alphabetical order
  from <- min(area, area2)
  to <- max(area, area2)
  createLink(from = from, to = to, opts = opts)
  properties_links <- readIniFile(
    file = file.path(opts$inputPath, "links", from, "properties.ini")
  )
  
  expect_true(paste(c(from,to), collapse = " - ") %in% levels(antaresRead::getLinks()))
  expect_true(to %in% names(properties_links))
  expect_true(file.exists(file.path(opts$inputPath, "links", from, paste0(to,"_parameters.txt"))))
  expect_true(file.exists(file.path(opts$inputPath, "links", from, "capacities", paste0(to,"_direct.txt"))))
  expect_true(file.exists(file.path(opts$inputPath, "links", from, "capacities", paste0(to,"_indirect.txt"))))
  
  removeLink(from = from, to = to, opts = opts)
  properties_links <- readIniFile(
    file = file.path(opts$inputPath, "links", from, "properties.ini")
  )
  
  expect_false(paste(c(from,to), collapse = " - ") %in% levels(antaresRead::getLinks()))
  expect_false(to %in% names(properties_links))
  expect_false(file.exists(file.path(opts$inputPath, "links", from, paste0(to,"_parameters.txt"))))
  expect_false(file.exists(file.path(opts$inputPath, "links", from, "capacities", paste0(to,"_direct.txt"))))
  expect_false(file.exists(file.path(opts$inputPath, "links", from, "capacities", paste0(to,"_indirect.txt"))))
  
  
  # without alphabetical order
  from <- max(area, area2)
  to <- min(area, area2)
  createLink(from = from, to = to, opts = opts)
  properties_links <- readIniFile(
    file = file.path(opts$inputPath, "links", to, "properties.ini")
  )
  
  expect_true(paste(c(to,from), collapse = " - ") %in% levels(antaresRead::getLinks()))
  expect_true(from %in% names(properties_links))
  expect_true(file.exists(file.path(opts$inputPath, "links", to, paste0(from,"_parameters.txt"))))
  expect_true(file.exists(file.path(opts$inputPath, "links", to, "capacities", paste0(from,"_direct.txt"))))
  expect_true(file.exists(file.path(opts$inputPath, "links", to, "capacities", paste0(from,"_indirect.txt"))))
  
  removeLink(from = from, to = to, opts = opts)
  properties_links <- readIniFile(
    file = file.path(opts$inputPath, "links", to, "properties.ini")
  )
  
  expect_false(paste(c(to,from), collapse = " - ") %in% levels(antaresRead::getLinks()))
  expect_false(from %in% names(properties_links))
  expect_false(file.exists(file.path(opts$inputPath, "links", to, paste0(from,"_parameters.txt"))))
  expect_false(file.exists(file.path(opts$inputPath, "links", to, "capacities", paste0(from,"_direct.txt"))))
  expect_false(file.exists(file.path(opts$inputPath, "links", to, "capacities", paste0(from,"_indirect.txt"))))
  
  
  unlink(x = opts$studyPath, recursive = TRUE)
})


