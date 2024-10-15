#Copyright © 2019 RTE Réseau de transport d’électricité

# create ----

  ## v9.0----
test_that("Create a new v9.0 study", {
  path <- file.path(tempdir(), "tests_createStudy")
  suppressWarnings(
    opts <- createStudy(path, antares_version = "9.0")
  )
  properties <- antaresRead:::readIniFile(file.path(path, "study.antares"))
  expect_identical(properties$antares$version, 9)
  unlink(path, recursive = TRUE)
})

test_that("Create a new v9.15 (2 digits) study", {
  path <- file.path(tempdir(), "tests_createStudy")
  suppressWarnings(
    opts <- createStudy(path, antares_version = "9.15")
  )
  properties <- antaresRead:::readIniFile(file.path(path, "study.antares"))
  expect_identical(properties$antares$version, 9.15)
  unlink(path, recursive = TRUE)
})

test_that("Create a new v9.15.2 (error bad format version) study", {
  path <- file.path(tempdir(), "tests_createStudy")
  
  expect_error(
    opts <- createStudy(path, antares_version = "9.15.2"), 
    regexp = 'From Antares version 9, put version like this : \'9.0\' or')
  
  expect_error(
    opts <- createStudy(path, antares_version = "9.153"), 
    regexp = "Invalid antares_version format, good format is like \'9.99\' \\(two digits on minor\\)" )
  
  unlink(path, recursive = TRUE)
})

  ## v8.7.0----
test_that("Create a new v8.7.0 study", {
  path <- file.path(tempdir(), "tests_createStudy")
  suppressWarnings(
    opts <- createStudy(path, antares_version = "8.7.0")
  )
  properties <- antaresRead:::readIniFile(file.path(path, "study.antares"))
  expect_identical(properties$antares$version, 870L)
  unlink(path, recursive = TRUE)
})

  ## v8.6.0----
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

  ## v8.1.0----
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

  ## v7.0.0----
test_that("Create a new v7 study", {
  path <- file.path(tempdir(), "tests_createStudy")
  suppressWarnings(
    createStudy(path, antares_version = "7.0.0")
  )
  properties <- antaresRead:::readIniFile(file.path(path, "study.antares"))
  expect_identical(properties$antares$version, 700L)
  unlink(path, recursive = TRUE)
})

  ## v6.0.0----
test_that("Create a new v6 study", {
  path <- file.path(tempdir(), "tests_createStudy")
  suppressWarnings(
    createStudy(path, antares_version = "6.0.0")
  )
  properties <- antaresRead:::readIniFile(file.path(path, "study.antares"))
  expect_identical(properties$antares$version, 600L)
  unlink(path, recursive = TRUE)
})
