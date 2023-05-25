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
  opts_test <- createClusterST(area_test, 
                  "cluster-st-1", 
                  opts = opts_test) 
  
  opts_test$antaresVersion <- 860
  
  opts_test <- createClusterST(area_test, 
                  "cluster-st-2", 
                  opts = opts_test) 
  
  opts_test$antaresVersion <- 860
  
  st_clusters <- readClusterSTDesc(opts = opts_test)
  
  # edit cluster
  val <- 0.007
  opts_test <- editClusterST(area = area_test, 
                cluster_name = levels(st_clusters$cluster)[1], 
                test_param1= "test",
                test_param2= 0.002154,
                PMAX_injection = matrix(val, 8760), 
                PMAX_withdrawal = matrix(val, 8760),
                inflows =  matrix(0.007, 8760), 
                lower_rule_curve = matrix(val, 8760), 
                upper_rule_curve = matrix(val, 8760),
                opts = opts_test, 
                add_prefix = FALSE)
  
  opts_test$antaresVersion <- 860
  
  st_clusters <- readClusterSTDesc(opts = opts_test)
  
  res <- st_clusters[cluster == levels(st_clusters$cluster)[1], test.param1, test.param2]
  
  # test parameters values edited
  testthat::expect_true(all(res %in% c(0.002154, "test")))
  
  # test data value (with fread_antares)
  path_dir_test <- file.path(opts_test$inputPath, "st-storage", "series", area_test, 
                             paste(area_test, "cluster-st-1", sep = "_"))
  files_test <- list.files(path_dir_test, full.names = TRUE)
  l_file_series <- lapply(files_test, antaresRead:::fread_antares, opts = opts_test)
  
  value_test <- mean(sapply(l_file_series, 
                       function(.x){
                         mean(.x$V1)}))
  
  testthat::expect_equal(value_test, val)
  
  # test data value with readInputTS
  st_ts <- readInputTS(st_storage = "all", opts = opts_test)
  
  # check clusters
  testthat::expect_true(all(levels(st_clusters$cluster) %in% unique(st_ts$cluster)))
  
  # delete study
  unlink(opts_test$studyPath, recursive = TRUE)
})
