#Copyright © 2019 RTE Réseau de transport d’électricité

test_that("Create a new v8.6.0 study", {
  path <- file.path(tempdir(), "tests_createStudy")
  suppressWarnings(
    opts <- createStudy(path, antares_version = "8.6.0")
  )
  properties <- antaresRead:::readIniFile(file.path(path, "study.antares"))
  expect_identical(properties$antares$version, 860L)
  expect_true(dir.exists(file.path(path,"input","st-storage")))
  expect_true(dir.exists(file.path(path,"input","st-storage","clusters")))
  expect_true(dir.exists(file.path(path,"input","st-storage","series")))
  unlink(path, recursive = TRUE)
})

test_that("Create a new v8.1.0 study", {
  path <- file.path(tempdir(), "tests_createStudy")
  suppressWarnings(
    opts <- createStudy(path, antares_version = "8.1.0")
  )
  properties <- antaresRead:::readIniFile(file.path(path, "study.antares"))
  expect_identical(properties$antares$version, 810L)
  expect_true(is_active_RES(opts))
  unlink(path, recursive = TRUE)
})

test_that("Create a new v7 study", {
  path <- file.path(tempdir(), "tests_createStudy")
  suppressWarnings(
    createStudy(path, antares_version = "7.0.0")
  )
  properties <- antaresRead:::readIniFile(file.path(path, "study.antares"))
  expect_identical(properties$antares$version, 700L)
  unlink(path, recursive = TRUE)
})

test_that("Create a new v6 study", {
  path <- file.path(tempdir(), "tests_createStudy")
  suppressWarnings(
    createStudy(path, antares_version = "6.0.0")
  )
  properties <- antaresRead:::readIniFile(file.path(path, "study.antares"))
  expect_identical(properties$antares$version, 600L)
  unlink(path, recursive = TRUE)
})


test_that("delete v8.1.0 study", {
  path <- file.path(tempdir(), "tests_createStudy")
  suppressWarnings(
    opts <- createStudy(path, antares_version = "8.1.0")
  )
  properties <- antaresRead:::readIniFile(file.path(path, "study.antares"))
  expect_identical(properties$antares$version, 810L)
  expect_true(is_active_RES(opts))
  testthat::expect_true(file.exists(opts$studyPath))
  deleteStudy(opts = simOptions())
  testthat::expect_true(!file.exists(opts$studyPath))
})


test_that("delete v8.6.0 simulation", {
  #setup_study_860(sourcedir860)
  createStudy(path = tempdir(), 
              study_name = "createStudy8.6", 
              antares_version = "8.6.0")
  suppressWarnings(
    #opts_test <- setSimulationPath(study_temp_path)
    opts_test <- simOptions()
  )
 # split_simPath <- strsplit(opts_test$simPath,"/")[[1]]
  #simulation <- split_simPath[length(split_simPath)]
  testthat::expect_true(file.exists(opts_test$studyPath))
  deleteStudy(opts = opts_test)
  testthat::expect_true(!file.exists(opts_test$studyPath))
})

