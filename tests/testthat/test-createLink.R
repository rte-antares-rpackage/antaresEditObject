

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



test_that("Check if createLink() in version >= 8.2 writes time series link in the right file regardless alphabetical order", {

  ant_version <- "8.2.0"
  st_test <- paste0("my_study_820_", paste0(sample(letters,5),collapse = ""))
  suppressWarnings(opts <- createStudy(path = pathstd, study_name = st_test, antares_version = ant_version))
  
  area <- "aa"
  area2 <- "zz"
  createArea(area)
  createArea(area2)
  suppressWarnings(opts <- setSimulationPath(opts$studyPath, simulation = "input"))
  
  dat_mat <- c(1,3,2,4)
  dat_mat_inv <- c(2,4,1,3)
  nb_cols <- length(dat_mat)
  mat_multi_scen <- matrix(data = rep(dat_mat, each = 8760), ncol = nb_cols)
  mat_multi_scen_inv <- matrix(data = rep(dat_mat_inv, each = 8760), ncol = nb_cols)
  
  path_direct_link_file <- file.path(opts$inputPath, "links", area, "capacities", paste0(area2,"_direct.txt"))
  path_indirect_link_file <- file.path(opts$inputPath, "links", area, "capacities", paste0(area2,"_indirect.txt"))
  
  # alphabetical order ----
  createLink(from = area, to = area2, opts = opts, tsLink = mat_multi_scen, overwrite = TRUE)
  suppressWarnings(opts <- setSimulationPath(opts$studyPath, simulation = "input"))
  
  # first columns go to direct file
  expect_equal(antaresRead:::fread_antares(opts = opts,
                                           file = path_direct_link_file),
               as.data.table(mat_multi_scen[,seq(1, nb_cols/2)]))
  
  # last columns go to indirect file
  expect_equal(antaresRead:::fread_antares(opts = opts,
                                           file = path_indirect_link_file),
               as.data.table(mat_multi_scen[,seq((nb_cols/2)+1, nb_cols)]))
  
  editLink(from = area, to = area2, opts = opts, tsLink = mat_multi_scen_inv)
  
  # first columns go to direct file
  expect_equal(antaresRead:::fread_antares(opts = opts,
                                           file = path_direct_link_file),
               as.data.table(mat_multi_scen_inv[,seq(1, nb_cols/2)]))
  
  # last columns go to indirect file
  expect_equal(antaresRead:::fread_antares(opts = opts,
                                           file = path_indirect_link_file),
               as.data.table(mat_multi_scen_inv[,seq((nb_cols/2)+1, nb_cols)]))
  
  # no alphabetical order ----
  createLink(from = area2, to = area, opts = opts, tsLink = mat_multi_scen_inv, overwrite = TRUE)
  
  # first columns go to indirect file
  expect_equal(antaresRead:::fread_antares(opts = opts,
                                           file = path_indirect_link_file),
               as.data.table(mat_multi_scen_inv[,seq(1, nb_cols/2)]))
  
  # last columns go to direct file
  expect_equal(antaresRead:::fread_antares(opts = opts,
                                           file = path_direct_link_file),
               as.data.table(mat_multi_scen_inv[,seq((nb_cols/2)+1, nb_cols)]))
  
  editLink(from = area2, to = area, opts = opts, tsLink = mat_multi_scen)
  
  # first columns go to direct file
  expect_equal(antaresRead:::fread_antares(opts = opts,
                                           file = path_indirect_link_file),
               as.data.table(mat_multi_scen[,seq(1, nb_cols/2)]))
  
  # last columns go to indirect file
  expect_equal(antaresRead:::fread_antares(opts = opts,
                                           file = path_direct_link_file),
               as.data.table(mat_multi_scen[,seq((nb_cols/2)+1, nb_cols)]))

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
