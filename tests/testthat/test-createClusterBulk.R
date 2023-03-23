test_that("Create cluster bulk v8", {
  # load template etude V8
  sourcedir <- system.file("test_v8", package = "antaresRead")
  studies <- list.files(sourcedir, pattern = "\\.tar\\.gz$", full.names = TRUE)
  
  # untar etude
  untar(studies, exdir = tempdir())
  study_temp_path <- file.path(tempdir(), "test_case")
  opts_temp <- antaresRead::setSimulationPath(study_temp_path, "input")
  
  # liste des areas
  antaresRead::getAreas()
  
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
  
  # traitement pour 1 seul area
  # maj_opts <- createClusterBulk(cluster_object = c(zone_test_1, zone_test_2), 
  #                   add_prefix = TRUE,
  #                   area_zone = antaresRead::getAreas()[1],
  #                   opts = opts_temp)
  
  
  # traitements pour plusieurs areas
  list_areas <- antaresRead::getAreas()[1:5]
  
  start_time <- Sys.time()
  
  maj_opts <- lapply(list_areas, createClusterBulk,
         cluster_object = c(zone_test_1, zone_test_2),
         add_prefix = TRUE, 
         opts = opts_temp)
  
  end_time <- Sys.time()
  print(end_time-start_time)
  
  # recuperer les meta de l'etude la plus recente
  maj_opts[[length(list_areas)]]
  
  # lister tous les cluster
  antaresRead::readClusterDesc()
  
  
  ## tests
  
  # tester diff entre les etudes 
  testthat::expect_error(testthat::expect_equal(maj_opts[[4]], maj_opts[[5]]))
  
  # l'etude la plus a jour a plus de cluster
  len_old <- length(maj_opts[[4]]$areasWithClusters)
  len_last <- length(maj_opts[[5]]$areasWithClusters)
  
  testthat::expect_true(len_old<len_last)
  
  
  
  
  
})
