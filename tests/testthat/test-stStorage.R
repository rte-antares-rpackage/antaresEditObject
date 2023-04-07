context("createClusterST function")

sapply(studies, function(study) {
  
  setup_study(study, sourcedir)
  opts <- antaresRead::setSimulationPath(studyPath, "input")
  
  if (opts$antaresVersion >= 860){
    area_test = getAreas()[1]

    # Test 1
    test_that("createClusterST throws error for invalid area name", {
      expect_error(createClusterST("INVALID_AREA", "cluster_name"),
                   regexp = "is not a valid area name")
    })
    
    # Test 2
    test_that("createClusterST throws error for incorrect number of rows in storage values", {
      
      expect_error(createClusterST(area_test, "cluster1", PMAX_charging = matrix(1, 2, 2)))
      
      createClusterST(area_test, "cluster1", add_prefix = F)
      expect_true(file.exists(file.path(simOptions()$inputPath, 
                                        "ST-storages", "series", area_test, "cluster1", "inflow.txt")))
      
    })
    # Test 3
    test_that("createClusterST throws error when cluster already exist.", {
      expect_error(createClusterST(area_test, "cluster1", add_prefix = F),
                   regexp = "already exist")
    })
  }

  
})