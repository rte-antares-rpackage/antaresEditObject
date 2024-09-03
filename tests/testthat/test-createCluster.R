
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

    # list .ini files
    path_thermal <- file.path(opts$inputPath, "thermal", "clusters")
    all_ini_files <- list.files(path_thermal, full.names = TRUE, recursive = TRUE)
    
    # extract number or raw
    suppressWarnings(
      ini_nb_raw <- sapply(
        lapply(all_ini_files, 
               data.table::fread), 
        function(x) dim(x)[2])
    )
    
    # test all ini files are empty
    expect_equal(sum(ini_nb_raw), 0)
  })
  
  # remove temporary study
  unlink(x = file.path(pathstd, "test_case"), recursive = TRUE)
  
})


# v860 ----
test_that("Create cluster with pollutants params (new feature v8.6)",{
  # INIT STUDY 
  suppressWarnings(
    createStudy(path = tempdir(), 
                study_name = "test_pollutants", 
                antares_version = "8.6.0"))
  
  createArea(name = "test")
  
  test_that("Create cluster with bad parameter pollutant",{
    bad_pollutants_param <- "not_a_list"
    
    testthat::expect_error(
      createCluster(area = getAreas()[1],
                    cluster_name = "bad_cluster",
                    group = "Other",
                    unitcount = as.integer(1),
                    nominalcapacity = 100,
                    list_pollutants = bad_pollutants_param), 
      regexp = "'list_pollutants' must be a 'list'")
  })
 
  test_that("Create cluster with parameter pollutant",{
    pollutants_params <- list(
      "nh3"= 0.25, "nox"= 0.45, "pm2_5"= 0.25, 
      "pm5"= 0.25, "pm10"= 0.25, "nmvoc"= 0.25, "so2"= 0.25,
      "op1"= 0.25, "op2"= 0.25, "op3"= 0.25, 
      "op4"= 0.25, "op5"= 0.25, "co2"= NULL
    )
    
    createCluster(
      area = getAreas()[1], 
      cluster_name = "mycluster_pollutant",
      group = "Other",
      unitcount = 1,
      nominalcapacity = 8000,
      `min-down-time` = 0,
      `marginal-cost` = 0.010000,
      `market-bid-cost` = 0.010000, 
      list_pollutants = pollutants_params,
      time_series = matrix(rep(c(0, 8000), each = 24*364), ncol = 2),
      prepro_modulation = matrix(rep(c(1, 1, 1, 0), each = 24*365), ncol = 4))
    
    res_cluster <- antaresRead::readClusterDesc()
    
    # check if cluster is created
    testthat::expect_true(paste(getAreas()[1], "mycluster_pollutant", sep = "_") %in% 
                            levels(res_cluster$cluster))
    
    names_pollutants <- names(pollutants_params)
    
    # check if pollutants is read well
    testthat::expect_true(all(names_pollutants %in% 
                                names(res_cluster)))
  })
  
  # remove temporary study
  deleteStudy(opts = simOptions())
  testthat::expect_true(TRUE)
})


# Cluster in binding constraint not removed ----
test_that("removeCluster() : cluster is not removed if it is referenced in a binding constraint", {
  
  ant_version <- "8.2.0"
  st_test <- paste0("my_study_820_", paste0(sample(letters,5),collapse = ""))
  suppressWarnings(opts <- createStudy(path = pathstd, study_name = st_test, antares_version = ant_version))
  
  nb_areas <- 5
  ids_areas <- seq(1,nb_areas)
  my_areas <- paste0("zone",ids_areas)
  
  clusters <- c("nuclear", "gas", "coal")
  my_clusters <- expand.grid("area" = my_areas, "cluster_name" = clusters)
  my_clusters$cluster_name_prefixed <- paste0(my_clusters$area, "_", my_clusters$cluster_name)
  my_clusters$cluster_name_binding <- paste0(my_clusters$area, ".", my_clusters$cluster_name_prefixed)
  lst_clusters <- split(my_clusters[,c("cluster_name_binding")], my_clusters$cluster_name)
  
  # Areas
  lapply(my_areas, FUN = function(area){createArea(name = area, opts = simOptions())})
  
  # Clusters
  apply(my_clusters[,c("area","cluster_name")],
        MARGIN = 1,
        FUN = function(row){
          createCluster(area = as.character(row[1]),
                        cluster_name = as.character(row[2]),
                        add_prefix = TRUE,
                        opts = simOptions()
                        )
        }
  )
  
  suppressWarnings(opts <- setSimulationPath(path = opts$studyPath, simulation = "input"))
  
  nb_cols_per_matrix <- 3
  nb_hours_per_year <- 8784
  nb_values_per_matrix <- nb_hours_per_year * nb_cols_per_matrix
  for (cluster in names(lst_clusters)) {
    names_coefs_bc <- lst_clusters[[cluster]]
    coefs <- seq_len(length(names_coefs_bc))
    names(coefs) <- names_coefs_bc
    createBindingConstraint(name = paste0("bc_",cluster),
                            timeStep = "hourly",
                            operator = "less",
                            coefficients = coefs,
                            values = matrix(rep(0,nb_values_per_matrix), ncol = nb_cols_per_matrix),
                            opts = opts
                            )
  }
  
  suppressWarnings(opts <- setSimulationPath(path = opts$studyPath, simulation = "input"))
  
  expect_warning(removeCluster(area = "zone1", cluster_name = "nuclear", add_prefix = TRUE, opts = opts),
                 regexp = "The following binding constraints have the cluster to remove as a coefficient :")
  
  unlink(x = opts$studyPath, recursive = TRUE)
})


# Delete expected files ----
test_that("removeCluster(): check if the expected files are deleted", {
  
  st_test <- paste0("my_study_820_", paste0(sample(letters,5),collapse = ""))
  suppressWarnings(opts <- createStudy(path = pathstd, study_name = st_test, antares_version = "8.2.0"))

  ## Areas
  area <- "zone1"
  createArea(name = area, opts = simOptions())
  
  ## Clusters
  clusters <- c("nuclear", "gas", "coal")
  nb_clusters <- length(clusters)
  my_clusters <- expand.grid("area" = area, "cluster_name" = clusters)
  apply(my_clusters[,c("area","cluster_name")],
          MARGIN = 1,
          FUN = function(row){
            createCluster(area = as.character(row[1]),
                          cluster_name = as.character(row[2]),
                          add_prefix = TRUE,
                          opts = simOptions()
            )
          }
  )
  
  suppressWarnings(opts <- setSimulationPath(path = opts$studyPath, simulation = "input"))
  
  all_clusters <- readClusterDesc(opts = simOptions())
  expect_true(nrow(all_clusters) == nb_clusters)
  
  i <- 0
  preproPath <- file.path(opts$inputPath, "thermal", "prepro")
  seriesPath <- file.path(opts$inputPath, "thermal", "series")
  # remove N-1 first clusters
  for (cluster in clusters[-length(clusters)]) {
    i <- i + 1
    suppressWarnings(removeCluster(area = area, cluster_name = cluster, add_prefix = TRUE, opts = simOptions()))
    all_clusters <- readClusterDesc(opts = simOptions())
    expect_true(nrow(all_clusters) == nb_clusters - i)
    expect_false(dir.exists(file.path(preproPath, area, paste0(area, "_", cluster))))
    expect_false(dir.exists(file.path(seriesPath, area, paste0(area, "_", cluster))))
    expect_true(dir.exists(file.path(preproPath, area)))
    expect_true(dir.exists(file.path(seriesPath, area)))
  }
  
  all_clusters <- readClusterDesc(opts = simOptions())
  expect_true(nrow(all_clusters) == 1)
  
  # last cluster
  suppressWarnings(removeCluster(area = area, cluster_name = clusters[length(clusters)], add_prefix = TRUE, opts = simOptions()))
  suppressWarnings(all_clusters <- readClusterDesc(opts = simOptions()))
  expect_true(nrow(all_clusters) == 0)
  # Remove area directory when removing last cluster of the area
  expect_false(dir.exists(file.path(preproPath, area)))
  expect_false(dir.exists(file.path(seriesPath, area)))
  
  unlink(x = opts$studyPath, recursive = TRUE)
})
