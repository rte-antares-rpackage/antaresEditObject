

##
# test bulk performance Versus creatCluster()
##

# global params ----
# load template etude V8
sourcedir <- system.file("test_v8", package = "antaresRead")
studies <- list.files(sourcedir, pattern = "\\.tar\\.gz$", full.names = TRUE)

# untar etude
untar(studies, exdir = tempdir())
study_temp_path <- file.path(tempdir(), "test_case")
opts_temp <- antaresRead::setSimulationPath(study_temp_path, "input")

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


# Cluster object with each parameters and data
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
    time_series = ts_8760,
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
    time_series = NULL,
    prepro_data = NULL,
    prepro_modulation = NULL)
)

# bulk ----
test_that("Create cluster bulk v8, time performance", {
  # multiple areas
  list_areas <- antaresRead::getAreas()[1:5]
  
  start_time <- Sys.time()
  
  maj_opts <- lapply(list_areas, createClusterBulk,
         cluster_object = c(zone_test_1, zone_test_2),
         add_prefix = TRUE, 
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
})


# createCluster----
test_that("create cluster test v8, timer information", {
  # several objects
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