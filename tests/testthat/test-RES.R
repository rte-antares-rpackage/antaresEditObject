
test_that("RES works", {
  
  tmp <- tempfile()
  suppressWarnings({
    createStudy(path = tmp, antares_version = "8.1.0")
    opts <- antaresRead::setSimulationPath(tmp)
  })
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


# Delete expected files ----
test_that("removeClusterRES(): check if the expected files are deleted", {
  
  st_test <- paste0("my_study_820_", paste0(sample(letters,5),collapse = ""))
  suppressWarnings(opts <- createStudy(path = pathstd, study_name = st_test, antares_version = "8.2.0"))

  ## Areas
  area <- "zone1"
  createArea(name = area, opts = simOptions())
  
  ## RES clusters
  clusters <- c("renewables1", "renewables2", "renewables3")
  nb_clusters <- length(clusters)
  my_clusters <- expand.grid("area" = area, "cluster_name" = clusters)
  apply(my_clusters[,c("area","cluster_name")],
          MARGIN = 1,
          FUN = function(row){
            createClusterRES(area = as.character(row[1]),
                             cluster_name = as.character(row[2]),
                             group = "Wind Onshore",
                             add_prefix = TRUE,
                             opts = simOptions()
            )
          }
  )
  
  suppressWarnings(opts <- setSimulationPath(path = opts$studyPath, simulation = "input"))
  
  all_res_clusters <- readClusterResDesc(opts = simOptions())
  expect_true(nrow(all_res_clusters) == nb_clusters)
  
  i <- 0
  seriesPath <- file.path(opts$inputPath, "renewables", "series")
  # remove N-1 first clusters
  for (cluster in clusters[-length(clusters)]) {
    i <- i + 1
    suppressWarnings(removeClusterRES(area = area, cluster_name = cluster, add_prefix = TRUE, opts = simOptions()))
    all_res_clusters <- readClusterResDesc(opts = simOptions())
    expect_true(nrow(all_res_clusters) == nb_clusters - i)
    expect_false(dir.exists(file.path(seriesPath, area, paste0(area, "_", cluster))))
    expect_true(dir.exists(file.path(seriesPath, area)))
  }
  
  all_res_clusters <- readClusterResDesc(opts = simOptions())
  expect_true(nrow(all_res_clusters) == 1)
  
  # last cluster
  suppressWarnings(removeClusterRES(area = area, cluster_name = clusters[length(clusters)], add_prefix = TRUE, opts = simOptions()))
  suppressWarnings(all_res_clusters <- readClusterResDesc(opts = simOptions()))
  expect_true(nrow(all_res_clusters) == 0)
  # Remove area directory when removing last cluster of the area
  expect_false(dir.exists(file.path(seriesPath, area)))
  
  unlink(x = opts$studyPath, recursive = TRUE)
})
