#Copyright © 2019 RTE Réseau de transport d’électricité

test_that("Create a new v8.6.0 study", {
  path <- file.path(tempdir(), "tests_createStudy")
  suppressWarnings(
    opts <- createStudy(path, antares_version = "8.6.0")
  )
  properties <- antaresRead:::readIniFile(file.path(path, "study.antares"))
  expect_identical(properties$antares$version, 860L)
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
  deleteStudy(opts = simOptions())
})


