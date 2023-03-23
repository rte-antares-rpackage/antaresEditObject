

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


# new ----
test_that("test v8, test timer", {
  
  # load template etude V8
  sourcedir <- system.file("test_v8", package = "antaresRead")
  studies <- list.files(sourcedir, pattern = "\\.tar\\.gz$", full.names = TRUE)
  
  # untar etude
  untar(studies, exdir = tempdir())
  study_temp_path <- file.path(tempdir(), "test_case")
  opts_temp <- antaresRead::setSimulationPath(study_temp_path, "input")
  
  # liste des areas
  antaresRead::getAreas()
  
  # ajout d'un nouveau cluster
  createCluster(area = antaresRead::getAreas()[1], 
                cluster_name = "nuclear",
                group = "other 4", 
                overwrite = TRUE)
  
  
  ##
  # tests creation plusieurs cluster pour plusieurs areas
  ##
  
  
  # preparation des donnees pour sutructures
  ts <- matrix(rep(c(0, 8000), each = 24*364), 
               ncol = 2)
  
  df_pd <- matrix(rep(c(1, 1, 1, 0), each = 24*365), 
                  ncol = 4)
  
  df_pm <- matrix(data = c(rep(1, times = 365 * 24 * 3), rep(0, times = 365 * 24 * 1)), 
                  ncol = 4)
  
  # strucuture de donnees pour un zone/area
  zone_test_1 <- list(
    `CCGT old 1`= list(parameter= list(
      name= "CCGT old 1",
      group = "Other",
      unitcount= 10L,
      nominalcapacity= 100,
      enabled= "true",
      `min-stable-power`= 80L,
      `min-up-time`= 20L,
      `min-down_time`= 30L),
      time_series = ts,
      prepro_data = df_pd,
      prepro_modulation = df_pm),
    
    `CCGT old 2`= list(parameter= list(
      name= "CCGT old 2",
      group = "Other"),
      time_series = ts,
      prepro_data = df_pd,
      prepro_modulation = df_pm)
  )
  
  zone_test_2 <- list(
    `CCGT CCS`= list(parameter= list(
      name= "CCGT CCS",
      group = "Other"),
      time_series = ts,
      prepro_data = df_pd,
      prepro_modulation = df_pm),
    `CCGT present 1`= list(parameter= list(
      name= "CCGT present 1",
      group = "Other"),
      time_series = ts,
      prepro_data = df_pd,
      prepro_modulation = df_pm)
  )
  
  # creation pour plusieurs cluster
  
  # lapply(antaresRead::getAreas()[1:5], createCluster,
  #        cluster_name = "nuclear",
  #        group = "other 4", 
  #        unitcount= 10L,
  #        nominalcapacity= 100,
  #        enabled= "true",
  #        `min-stable-power`= 80L, 
  #        time_series = ts,
  #        prepro_data = df_pd,
  #        prepro_modulation = df_pm,
  #        overwrite = TRUE)
  
  data_bloc <- c(zone_test_1, zone_test_2)
  
  start_time <- Sys.time()
  
  lapply(antaresRead::getAreas()[1:5], function(.x){
    for (i in seq_along(data_bloc)) {
      # multi ecriture
      multi_cluster <- do.call("createCluster",
                               list(    
                                 area= .x,
                                 cluster_name= data_bloc[[i]]$parameter$name,
                                 group= data_bloc[[i]]$parameter$group,
                                 
                                 #data_bloc[[i]]$parameter[setdiff(names(data_bloc[[i]]$parameter), c("name", "group"))],
                                 
                                 unitcount=  data_bloc[[i]]$parameter$unitcount,
                                 nominalcapacity= data_bloc[[i]]$parameter$nominalcapacity,
                                 enabled= data_bloc[[i]]$parameter$enabled,
                                 `min-stable-power`= data_bloc[[i]]$parameter$`min-stable-power`,
                                 `min-up-time`= data_bloc[[i]]$parameter$`min-up-time`,
                                 `min-down_time`= data_bloc[[i]]$parameter$`min-down_time`,
                                 time_series= data_bloc[[i]]$time_series,
                                 prepro_data=  data_bloc[[i]]$prepro_data,
                                 prepro_modulation = data_bloc[[i]]$prepro_modulation,
                                 add_prefix = TRUE,
                                 overwrite = TRUE,
                                 opts = opts_temp
                                 ))
    }
  })
  
  end_time <- Sys.time()
  print(end_time-start_time)
  
  
})
