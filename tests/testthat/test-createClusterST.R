context("createClusterST function")

test_that("Create short-term storage cluster",{
  setup_study_850(sourcedir850)
  opts_test <- antaresRead::setSimulationPath(study_temp_path, "input")
  
  # Need to create a "ST-storages" folder.
  #TODO Use createStudy and createArea instead
  path_master <- file.path(opts_test$inputPath, "ST-storages")
  dir.create(path_master)
  dir.create(file.path(path_master, "clusters"))
  dir.create(file.path(path_master, "series"))
  # temporary to test with "860"
  # force version
  opts_test$antaresVersion <- 860
  
  if (opts_test$antaresVersion >= 860){
    area_test = getAreas()[1]
    dir.create(file.path(path_master, "clusters",area_test))
    writeIni(NULL, file.path(path_master, "clusters", area_test, "list"))
    
    # Test 1 : createClusterST throws error for invalid area name
      expect_error(createClusterST("INVALID_AREA", "cluster_name", opts = opts_test),
                   regexp = "is not a valid area name")
    
    # Test 2 : createClusterST throws error for incorrect number of rows in storage values.
      
      expect_error(createClusterST(area_test, "cluster1",
                                   PMAX_charging = matrix(1, 2, 2),
                                   opts = opts_test))
      
    # Test 3 : createClusterST throws error when cluster already exist.
      
      createClusterST(area_test, "cluster1", opts = opts_test) #TODO
      readClusterSTDesc(opts = opts_test)
      expect_true(file.exists(file.path(opts_test$inputPath, 
                                        "ST-storages",
                                        "series",
                                        area_test,
                                        paste0(area_test, "_", "cluster1"),
                                        "inflow.txt")))

      expect_error(createClusterST(area_test, 
                                   "cluster1",
                                   opts = opts_test),
                   regexp = "already exist")
    # Test 4 : RemoveClusterST works.
      
      removeClusterST(area = area_test, "cluster1")
      expect_true(!dir.exists(file.path(opts_test$inputPath, 
                                        "ST-storages",
                                        "series",
                                        area_test,
                                        paste0(area_test, "_", "cluster1"))))
  }
  #Delete study
  unlink(opts_test$studyPath, recursive = TRUE)
})
