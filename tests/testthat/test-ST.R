test_that("ActivateST works", {
  
  tmp <- tempfile()
  suppressWarnings({
    createStudy(path = tmp, antares_version = "8.1.0")
    opts <- antaresRead::setSimulationPath(tmp)
  })
  activateST(quietly = TRUE)
  expect_true(dir.exists(file.path(tmp,"input","st-storage")))
  expect_true(dir.exists(file.path(tmp,"input","st-storage","clusters")))
  expect_true(dir.exists(file.path(tmp,"input","st-storage","series")))
})