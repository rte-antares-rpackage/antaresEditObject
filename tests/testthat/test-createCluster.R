
# v710 ----
context("Function createCluster")


sapply(studies, function(study) {
  
  setup_study(study, sourcedir)
  opts <- antaresRead::setSimulationPath(studyPath, "input")
  
  
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
      group = "Other",
      unitcount = 1,
      nominalcapacity = 8000,
      `min-down-time` = 0,
      `marginal-cost` = 0.010000,
      `market-bid-cost` = 0.010000,
      time_series = matrix(rep(c(0, 8000), each = 24*364), ncol = 2),
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
  
  # remove temporary study
  unlink(x = file.path(pathstd, "test_case"), recursive = TRUE)
  
})


# v860 ----
# global params for structure v8.6 
setup_study_860(sourcedir860)
opts_test <- antaresRead::setSimulationPath(study_temp_path, "input")

test_that("Create cluster with polluants params (new feature v8.6)",{
  
  polluants_params <- list(
    "nh3"= 0.25, "nox"= 0.45, "pm2_5"= 0.25, 
    "pm5"= 0.25, "pm10"= 0.25, "nmvoc"= 0.25, "so2"= 0.25,
    "op1"= 0.25, "op2"= 0.25, "op3"= 0.25, 
    "op4"= 0.25, "op5"= 0.25, "co2"= NULL
  )
  
  createCluster(
    area = getAreas()[1], 
    cluster_name = "mycluster_polluant",
    group = "Other",
    unitcount = 1,
    nominalcapacity = 8000,
    `min-down-time` = 0,
    `marginal-cost` = 0.010000,
    `market-bid-cost` = 0.010000, 
    list_polluants = polluants_params,
    time_series = matrix(rep(c(0, 8000), each = 24*364), ncol = 2),
    prepro_modulation = matrix(rep(c(1, 1, 1, 0), each = 24*365), ncol = 4), 
    opts = opts_test
  )
  
  res_cluster <- antaresRead::readClusterDesc()
  
  # check if cluster is created
  testthat::expect_true(paste(getAreas()[1], "mycluster_polluant", sep = "_") %in% 
                levels(res_cluster$cluster))
  
  names_polluants <- names(polluants_params)
  
  # check if polluants is read well
  testthat::expect_true(all(names_polluants %in% 
                              names(res_cluster)))
  
  # remove temporary study
  unlink(x = opts_test$studyPath, recursive = TRUE)
})
