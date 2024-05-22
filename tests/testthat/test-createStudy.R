#Copyright © 2019 RTE Réseau de transport d’électricité

# create ----

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
