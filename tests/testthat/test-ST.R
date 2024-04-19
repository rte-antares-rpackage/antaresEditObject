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
