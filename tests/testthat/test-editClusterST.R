# global params for structure v8.6 ----
setup_study_850(sourcedir850)
opts_test <- antaresRead::setSimulationPath(study_temp_path, "input")

# need to create a "st-storage" folder
path_master <- file.path(opts_test$inputPath, "st-storage")
dir.create(path_master)
dir.create(file.path(path_master, "clusters"))
dir.create(file.path(path_master, "series"))
# temporary to test with "860"
# force version
opts_test$antaresVersion <- 860

test_that("edit st-storage clusters (only for study >= v8.6.0" , {
  
  # areas tests 
  area_test = getAreas()[1]
  dir.create(file.path(path_master, "clusters",area_test))
  writeIni(NULL, file.path(path_master, "clusters", area_test, "list"))
  
  # create tests clusters
  createClusterST(area_test, 
                  "cluster-st-1", 
                  opts = opts_test) 
  
  createClusterST(area_test, 
                  "cluster-st-2", 
                  opts = opts_test) 
  
  st_clusters <- readClusterSTDesc(opts = opts_test)
  
  editClusterST(area = area_test, 
                cluster_name = levels(st_clusters$cluster)[1], 
                test_param1= "test",
                test_param2= 0.002154,
                opts = opts_test, 
                add_prefix = FALSE)
  
  st_clusters <- readClusterSTDesc(opts = opts_test)
  
  res <- st_clusters[cluster == levels(st_clusters$cluster)[1], test.param1, test.param2]
  
  # test parameters values edited
  testthat::expect_true(all(res %in% c(0.002154, "test")))
  
  # delete study
  unlink(opts_test$studyPath, recursive = TRUE)
})
