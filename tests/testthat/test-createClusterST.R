
# global params for structure v8.6 ----
setup_study_850(sourcedir850)
opts_test <- antaresRead::setSimulationPath(study_temp_path, "input")

# Need to create a "st-storage" folder
path_master <- file.path(opts_test$inputPath, "st-storage")
dir.create(path_master)
dir.create(file.path(path_master, "clusters"))
dir.create(file.path(path_master, "series"))
# temporary to test with "860"
# force version
opts_test$antaresVersion <- 860

test_that("Create short-term storage cluster (new feature v8.6)",{
  
  if (opts_test$antaresVersion >= 860){
    #TODO Use createStudy and createArea instead
    area_test = getAreas()[1]
    dir.create(file.path(path_master, "clusters",area_test))
    writeIni(NULL, file.path(path_master, "clusters", area_test, "list"))
    
    # createClusterST throws error for invalid area name
    testthat::expect_error(createClusterST("INVALID_AREA", "cluster_name", opts = opts_test),
                 regexp = "is not a valid area name")
    
    # createClusterST throws error for incorrect number of rows in storage values.
    testthat::expect_error(createClusterST(area_test, "cluster1", 
                                           PMAX_injection = matrix(1, 2, 2),
                                           opts = opts_test))
      
    # check cluster exists with default values
    createClusterST(area_test, 
                    "cluster1", 
                    opts = opts_test) 
    
    testthat::expect_true(paste(area_test, "cluster1", sep = "_") 
                %in% levels(readClusterSTDesc(opts = opts_test)$cluster))
    
    # read series
    # TODO => test with readInputTS
    file_series <- antaresRead:::fread_antares(opts = opts_test, 
                                file = file.path(path_master, 
                                                 "series",
                                                 area_test, 
                                                 paste(area_test, "cluster1", sep = "_"),
                                                 "lower-rule-curve.txt"))
    # check default value and dimension
    testthat::expect_equal(dim(file_series), c(8760, 1))
    testthat::expect_equal(mean(file_series$V1), 0)
    
    # createClusterST throws error when cluster already exist.
    testthat::expect_error(createClusterST(area_test, 
                                 "cluster1",
                                 opts = opts_test),
                 regexp = "already exist")
      
    test_that("Remove storage cluster (new feature v8.6)", {
      # RemoveClusterST (if no cluster => function read return error => see readClusterDesc tests)
      removeClusterST(area = area_test, "cluster1")
      
      testthat::expect_error(readClusterSTDesc(opts = opts_test))
    })
    
  }
  #Delete study
  unlink(opts_test$studyPath, recursive = TRUE)
})
