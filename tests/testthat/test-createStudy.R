#Copyright © 2019 RTE Réseau de transport d’électricité


context("Function createStudy")



path <- file.path(tempdir(), "tests_createStudy")

test_that("Create a new v7 study", {
  createStudy(path, antares_version = "7.0.0")
  properties <- antaresRead:::readIniFile(file.path(path, "study.antares"))
  expect_identical(properties$antares$version, 700L)
})

unlink(path, recursive = TRUE)


path <- file.path(tempdir(), "tests_createStudy")

test_that("Create a new v6 study", {
  createStudy(path, antares_version = "6.0.0")
  properties <- antaresRead:::readIniFile(file.path(path, "study.antares"))
  expect_identical(properties$antares$version, 600L)
})

unlink(path, recursive = TRUE)



