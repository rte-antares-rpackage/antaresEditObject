test_that("ActivateST works", {
  
  tmp <- tempfile()
  suppressWarnings({
    createStudy(path = tmp, antares_version = "8.1.0")
    opts <- antaresRead::setSimulationPath(tmp)
  })
  activateST(quietly = TRUE)
  expect_true(dir.exists(file.path(tmp,"input","ST-storages")))
  expect_true(dir.exists(file.path(tmp,"input","ST-storages","clusters")))
  expect_true(dir.exists(file.path(tmp,"input","ST-storages","series")))
})