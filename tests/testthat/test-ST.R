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


# Behaviour on existing or non-existing cluster ----
test_that("createClusterST(), editClusterST() and removeClusterST() work as expected if the cluster exists or does not exist", {
  
  ant_version <- "8.6.0"
  st_test <- paste0("my_study_860_", paste0(sample(letters,5), collapse = ""))
  suppressWarnings(opts <- createStudy(path = pathstd, study_name = st_test, antares_version = ant_version))
  area_test <- "zone1"
  opts <- createArea(name = area_test, opts = simOptions())
  
  ## createClusterST
  # Create a cluster on a non-existing area
  expect_error(createClusterST(area = "bla", cluster_name = "cluster_st_test_create", add_prefix = TRUE, storage_parameters = list("efficiency" = 0.6), opts = simOptions()),
               regexp = "is not a valid area name")
  # Create a non-existing cluster
  expect_no_error(createClusterST(area = area_test, cluster_name = "cluster_st_test_create", add_prefix = TRUE, storage_parameters = list("efficiency" = 0.6), opts = simOptions()))
  # Create an existing cluster - idempotence
  expect_error(createClusterST(area = area_test, cluster_name = "cluster_st_test_create", add_prefix = TRUE, storage_parameters = list("efficiency" = 0.7), opts = simOptions()),
               regexp = "Cluster already exists.")  
  # Create a non-existing cluster - CI
  expect_no_error(createClusterST(area = toupper(area_test), cluster_name = "clUstEr_st_tEst_crEAtE2", add_prefix = TRUE, storage_parameters = list("efficiency" = 0.6), opts = simOptions()))
  # Create an existing cluster - CI - idempotence
  expect_error(createClusterST(area = toupper(area_test), cluster_name = toupper("clUstEr_st_tEst_crEAtE2"), add_prefix = TRUE, storage_parameters = list("efficiency" = 0.7), opts = simOptions()),
               regexp = "Cluster already exists.")     
  
  ## editClusterST
  # Edit a cluster on a non-existing area
  expect_error(editClusterST(area = "bla", cluster_name = "cluster_st_not_exists", add_prefix = TRUE, storage_parameters = list("efficiency" = 0.6), opts = simOptions()),
               regexp = "is not a valid area name")
  # Edit a non-existing cluster
  expect_error(editClusterST(area = area_test, cluster_name = "cluster_st_not_exists", add_prefix = TRUE, storage_parameters = list("efficiency" = 0.6), opts = simOptions()),
               regexp = "Cluster 'zone1_cluster_st_not_exists' does not exist. It can not be edited.")
  # Edit an existing cluster
  expect_no_error(editClusterST(area = area_test, cluster_name = "cluster_st_test_create", add_prefix = TRUE, storage_parameters = list("efficiency" = 0.789), opts = simOptions()))
  # Edit the same existing cluster
  expect_no_error(editClusterST(area = area_test, cluster_name = "cluster_st_test_create", add_prefix = TRUE, storage_parameters = list("efficiency" = 0.890), opts = simOptions()))
  # Edit an existing cluster - CI
  expect_no_error(editClusterST(area = toupper(area_test), cluster_name = "ClUStER_st_tEst_crEAtE2", add_prefix = TRUE, storage_parameters = list("efficiency" = 0.789), opts = simOptions()))
  # Edit an existing cluster - CI - idempotence
  expect_no_error(editClusterST(area = toupper(area_test), cluster_name = toupper("clUstEr_st_tEst_crEAtE2"), add_prefix = TRUE, storage_parameters = list("efficiency" = 0.890), opts = simOptions()))
  
  ## removeClusterST
  # Remove a cluster on a non-existing area
  expect_error(removeClusterST(area = "bla", cluster_name = "cluster_st_not_exists", add_prefix = TRUE, opts = simOptions()),
               regexp = "is not a valid area name")
  # Remove a non-existing cluster
  expect_error(removeClusterST(area = area_test, cluster_name = "cluster_st_not_exists", add_prefix = TRUE, opts = simOptions()),
               regexp = "Cluster can not be removed")
  # Remove an existing cluster
  expect_no_error(removeClusterST(area = area_test, cluster_name = "cluster_st_test_create", add_prefix = TRUE, opts = simOptions()))
  # Remove an existing cluster - idempotence
  expect_error(removeClusterST(area = area_test, cluster_name = "cluster_st_test_create", add_prefix = TRUE, opts = simOptions()),
               regexp = "Cluster can not be removed")
  # Remove an existing cluster - CI
  expect_no_error(removeClusterST(area = area_test, cluster_name = "CLuSTeR_ST_TeST_CReaTe2", add_prefix = TRUE, opts = simOptions()))
  
  unlink(x = opts$studyPath, recursive = TRUE)
})


# Delete expected files ----
test_that("removeClusterST(): check if the expected files are deleted", {
  
  st_test <- paste0("my_study_860_", paste0(sample(letters,5),collapse = ""))
  suppressWarnings(opts <- createStudy(path = pathstd, study_name = st_test, antares_version = "8.6.0"))

  ## Areas
  area <- "zone1"
  createArea(name = area, opts = simOptions())
  
  ## ST clusters
  clusters <- c("batteries1", "batteries2", "batteries3")
  nb_clusters <- length(clusters)
  my_clusters <- expand.grid("area" = area, "cluster_name" = clusters)
  apply(my_clusters[,c("area","cluster_name")],
          MARGIN = 1,
          FUN = function(row){
            createClusterST(area = as.character(row[1]),
                            cluster_name = as.character(row[2]),
                            group = "Other1",
                            add_prefix = TRUE,
                            opts = simOptions()
            )
          }
  )
  
  suppressWarnings(opts <- setSimulationPath(path = opts$studyPath, simulation = "input"))
  
  all_st_clusters <- readClusterSTDesc(opts = simOptions())
  expect_true(nrow(all_st_clusters) == nb_clusters)
  
  i <- 0
  seriesPath <- file.path(opts$inputPath, "st-storage", "series")
  # remove N-1 first clusters
  for (cluster in clusters[-length(clusters)]) {
    i <- i + 1
    suppressWarnings(removeClusterST(area = area, cluster_name = cluster, add_prefix = TRUE, opts = simOptions()))
    all_st_clusters <- readClusterSTDesc(opts = simOptions())
    expect_true(nrow(all_st_clusters) == nb_clusters - i)
    expect_false(dir.exists(file.path(seriesPath, area, paste0(area, "_", cluster))))
    expect_true(dir.exists(file.path(seriesPath, area)))
  }
  
  all_st_clusters <- readClusterSTDesc(opts = simOptions())
  expect_true(nrow(all_st_clusters) == 1)
  
  # last cluster
  suppressWarnings(removeClusterST(area = area, cluster_name = clusters[length(clusters)], add_prefix = TRUE, opts = simOptions()))
  suppressWarnings(all_st_clusters <- readClusterSTDesc(opts = simOptions()))
  expect_true(nrow(all_st_clusters) == 0)
  # Remove area directory when removing last cluster of the area
  expect_false(dir.exists(file.path(seriesPath, area)))
  
  unlink(x = opts$studyPath, recursive = TRUE)
})
