
test_that("RES works", {
  
  tmp <- tempfile()
  createStudy(path = tmp)
  suppressWarnings(opts <- antaresRead::setSimulationPath(tmp))
  activateRES(quietly = TRUE)
  
  expect_true(is_active_RES(opts))
  expect_true(file.exists(file.path(tmp, "input", "renewables")))
  
  createArea(name = "area51")
  createClusterRES(
    area = "area51", 
    cluster_name = "ren01", 
    add_prefix = FALSE
  )
  
  clusterResIni <- file.path(opts$inputPath, "renewables", "clusters", "area51", "list.ini")
  expect_true(file.exists(clusterResIni))
  expect_true(file.size(clusterResIni) > 0)
  
  clusterRes <- readIniFile(clusterResIni)
  expect_true("ren01" %in% names(clusterRes))
  
  editClusterRES(
    area = "area51", 
    cluster_name = "ren01", 
    group = "Solar Rooftop",
    add_prefix = FALSE
  )
  clusterRes <- readIniFile(clusterResIni)
  expect_identical(clusterRes$ren01$group, "Solar Rooftop")
  
  removeClusterRES(
    area = "area51", 
    cluster_name = "ren01", 
    add_prefix = FALSE
  )
  clusterRes <- readIniFile(clusterResIni)
  expect_false("ren01" %in% names(clusterRes))
  
  
  unlink(tmp, recursive = TRUE)
})
