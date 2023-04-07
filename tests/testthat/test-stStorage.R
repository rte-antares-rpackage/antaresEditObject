
# Test the createClusterST function
context("createClusterST function")

sapply(studies, function(study) {
  
  setup_study(study, sourcedir)
  opts <- antaresRead::setSimulationPath(studyPath, "input")

  # Test 1
  test_that("createClusterST throws error for invalid area name", {
    expect_error(createClusterST("INVALID_AREA", "cluster_name"))
  })
  
  # Test 2
  test_that("createClusterST throws error for incorrect number of rows in storage values", {
    
    expect_error(createClusterST("area", "cluster_name", PMAX_charging = matrix(1, 2, 2)))
    expect_error(createClusterST("area", "cluster_name", PMAX_discharging = matrix(1, 2, 2)))
    expect_error(createClusterST("area", "cluster_name", inflow = matrix(1, 2, 2)))
    expect_error(createClusterST("area", "cluster_name", lower_rule_curve = matrix(1, 2, 2)))
    expect_error(createClusterST("area", "cluster_name", upper_rule_curve = matrix(1, 2, 2)))
    
  })
  # Test 3
  test_that("createClusterST throws error when cluster already exist.", {
    createClusterST("area", "cluster_name")
    expect_error(createClusterST("area", "cluster_name"))
  })
})