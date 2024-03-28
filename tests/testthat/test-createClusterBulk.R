

##
# test bulk performance Versus creatCluster()
##

# global params ----
# load template etude V8
sourcedir <- system.file("test_v8", package = "antaresRead")
studies <- list.files(sourcedir, pattern = "\\.tar\\.gz$", full.names = TRUE)

# untar etude
untar(studies[1], exdir = tempdir()) # v8
study_latest_version <- file.path(tempdir(), "test_case")
opts_temp <- antaresRead::setSimulationPath(study_latest_version, "input")

# areas list
antaresRead::getAreas()

# data to write
ts <- matrix(rep(c(0, 8000), each = 24*364), 
             ncol = 2)

ts_8760 <- matrix(rep(c(0, 8000), each = 24*365), 
             ncol = 2)

df_pd <- matrix(rep(c(1, 1, 1, 0), each = 24*365), 
                ncol = 4)

df_pm <- matrix(data = c(rep(1, times = 365 * 24 * 3), rep(0, times = 365 * 24 * 1)), 
                ncol = 4)


# Cluster object 

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
    overwrite= TRUE,
    time_series = ts_8760,
    prepro_data = df_pd,
    prepro_modulation = df_pm),
  
  # overwrite existing cluster
  `PEAK`= list(parameter= list(
    name= "PEAK",
    group = "Other"),
    overwrite= TRUE,
    time_series = ts,
    prepro_data = df_pd,
    prepro_modulation = df_pm)
)

zone_test_2 <- list(
  `CCGT CCS`= list(parameter= list(
    name= "CCGT CCS",
    group = "Other"),
    overwrite= TRUE,
    time_series = ts,
    prepro_data = df_pd,
    prepro_modulation = df_pm),
  `CCGT present 1`= list(parameter= list(
    name= "CCGT present 1",
    group = "Other"),
    overwrite= TRUE,
    time_series = NULL,
    prepro_data = NULL,
    prepro_modulation = NULL)
)

# cluster to make error (no overwrite on existing cluster name)
zone_test_error <- list(
  `BASE`= list(parameter= list(
    name= "BASE",
    group = "not_important"
  ),
  # overwrite= FALSE, (default FALSE)
  time_series = NULL,
  prepro_data = NULL,
  prepro_modulation = NULL)
)



# bulk ----
test_that("Create cluster bulk v8, time performance", {
  
  # /!\ this template study has no prefix on cluster's names
  
  # multiple areas
  list_areas <- antaresRead::getAreas()[1:5]
  
  start_time <- Sys.time()
  
  # with no prefix 
    # launch BULK
  maj_opts <- lapply(list_areas, createClusterBulk,
         cluster_object = c(zone_test_1, zone_test_2),
         add_prefix = FALSE, 
         opts = opts_temp)
  
  end_time <- Sys.time()
  print(end_time-start_time)
  
  # keep the most recent modified study
  maj_opts[[length(list_areas)]]
  
  # FI : listing clusters
  antaresRead::readClusterDesc()

  
  # compare study modification (have to be different) 
  testthat::expect_error(testthat::expect_equal(maj_opts[[4]], maj_opts[[5]]))
  
  # last modified study has more informations (clusters)
  len_old <- length(maj_opts[[4]]$areasWithClusters)
  len_last <- length(maj_opts[[5]]$areasWithClusters)
  
  testthat::expect_true(len_old<len_last)
  
  # test error 
  testthat::expect_error(createClusterBulk(cluster_object = zone_test_error, area_zone = list_areas[1]))
  
  
  ## SAME tests with new areas and prefix (to be not confusing with template v8)
  # with prefix
  new_areas <- c("fr", "es", "DE")
  
  lapply(new_areas, createArea,
         nodalOptimization = nodalOptimizationOptions())
  
  # launch BULK
  lapply(new_areas, createClusterBulk,
         cluster_object = c(zone_test_1,
                            zone_test_2,
                            zone_test_error),
         add_prefix = TRUE)
  
  # last modified study has more informations (clusters)
  expect_true(all(tolower(new_areas) %in% simOptions()$areasWithClusters))
})


# createCluster----
test_that("create cluster test v8, timer information", {
  # several objects
  data_bloc <- c(zone_test_1, zone_test_2)
  
  start_time <- Sys.time()
  
  lapply(antaresRead::getAreas(opts = opts_temp)[1:5], function(.x){
    for (i in seq_along(data_bloc)) {
      # multi ecriture
      multi_cluster <- do.call("createCluster",
                               list(    
                                 area= .x,
                                 cluster_name= data_bloc[[i]]$parameter$name,
                                 group= data_bloc[[i]]$parameter$group,
                                 
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
