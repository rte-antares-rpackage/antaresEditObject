#Copyright © 2016 RTE Réseau de transport d’électricité


context("Function createCluster")



# Setup study -------------------------------------------------------------


path <- tempdir()
# Unzip the study
setup_study(path, sourcedir)
# set simulation path in mode input
opts <- antaresRead::setSimulationPath(studyPath, 'input')




# Tests -------------------------------------------------------------------

test_that("Create a new cluster", {
  
  area <- sample(x = getOption("antares")$areaList, size = 1)
  
  createCluster(area = area, cluster_name = "mycluster")
  
  expect_true(paste(area, "mycluster", sep = "_") %in% levels(antaresRead::readClusterDesc()$cluster))
})



test_that("Create a new cluster with bad parameters", {
  
  area <- sample(x = getOption("antares")$areaList, size = 1)
  expect_error(createCluster(area = area, cluster_name = "mybadcluster", time_series = matrix(rep(0, 100), ncol = 1)))
  expect_error(createCluster(area = area, cluster_name = "mybadcluster", prepro_modulation = matrix(rep(0, 100), ncol = 1)))
  expect_error(createCluster(area = area, cluster_name = "mybadcluster", prepro_modulation = matrix(rep(0, 2*8760), ncol = 2)))
})





area2 <- sample(x = getOption("antares")$areaList, size = 1)

test_that("Create a cluster with default properties", {
  
  createCluster(
    area = area2, 
    cluster_name = "mycluster2",
    group = "other",
    unitcount = 1,
    nominalcapacity = 8000,
    `min-down-time` = 0,
    `marginal-cost` = 0.010000,
    `market-bid-cost` = 0.010000,
    time_series = matrix(rep(c(0, 8000), each = 24*365), ncol = 2),
    prepro_modulation = matrix(rep(c(1, 1, 1, 0), each = 24*365), ncol = 4)
  )
  
  expect_true(paste(area2, "mycluster2", sep = "_") %in% levels(antaresRead::readClusterDesc()$cluster))
})



test_that("Remove a cluster", {
  
  removeCluster(area = area2, cluster_name = "mycluster2")
  
  expect_false(paste(area2, "mycluster2", sep = "_") %in% levels(antaresRead::readClusterDesc()$cluster))
})



test_that("Remove all clusters", {
  
  all_clusters <- antaresRead::readClusterDesc()
  for (i in seq_len(nrow(all_clusters))) {
    removeCluster(
      area = unlist(all_clusters[i, ]$area), 
      cluster_name = unlist(all_clusters[i, ]$cluster),
      add_prefix = FALSE
    )
  }
  expect_error(antaresRead::readClusterDesc())
  
})




# End ---------------------------------------------------------------------


# remove temporary study
unlink(x = file.path(path, "test_case"), recursive = TRUE)


