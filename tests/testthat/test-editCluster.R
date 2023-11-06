

context("Function editCluster")


sapply(studies, function(study) {
  
  setup_study(study, sourcedir)
  opts <- antaresRead::setSimulationPath(studyPath, "input")
  
  
  test_that("Edit nominal capacity", {
    editCluster(area = "a", cluster_name = "peak", nominalcapacity = 10600, add_prefix = FALSE)
    res <- antaresRead::readClusterDesc()[area == "a" & cluster == "peak", nominalcapacity]
    expect_equal(res, 10600)
  })
  
  test_that("Edit time series", {
    editCluster(area = "a", cluster_name = "peak",
                time_series = rep(8, 8760), add_prefix = FALSE)
    res <- fread(file.path(opts$inputPath, "thermal", "series", "a", "peak", "series.txt"))
    expect_equal(res$V1, rep(8, 8760))
  })
  
  test_that("Edit pre-process data", {
    m <- matrix(5, nrow = 365, ncol = 6)
    editCluster(area = "a", cluster_name = "peak",
                prepro_data = m, add_prefix = FALSE)
    res <- fread(file.path(opts$inputPath, "thermal", "prepro", "a", "peak", "data.txt"))
    m_res <- as.matrix(res)
    dimnames(m_res) <- NULL
    expect_equal(m_res, m)
  })
  
  test_that("Edit pre-process modulation", {
    m <- matrix(1, nrow = 8760, ncol = 4)
    editCluster(area = "a", cluster_name = "peak",
                prepro_modulation = m, add_prefix = FALSE)
    res <- fread(file.path(opts$inputPath, "thermal", "prepro", "a", "peak", "modulation.txt"))
    m_res <- as.matrix(res)
    dimnames(m_res) <- NULL
    expect_equal(m_res, m)
  })
  
  
  
  # remove temporary study
  unlink(x = file.path(pathstd, "test_case"), recursive = TRUE)
  
})



# v860 ----
# global params for structure v8.6 
setup_study_last(sourcedir_last_study)
opts_test <- antaresRead::setSimulationPath(study_temp_path, "input")

test_that("Edit cluster with pollutants params (new feature v8.6)",{
  
  pollutants_params <- list(
    "nh3"= 0.25, "nox"= 0.45, "pm2_5"= 0.25, 
    "pm5"= 0.25, "pm10"= 0.25, "nmvoc"= 0.25, "so2"= 0.25,
    "op1"= 0.25, "op2"= 0.25, "op3"= 0.25, 
    "op4"= 0.25, "op5"= 0.25, "co2"= NULL
  )
  
  opts_test <- createCluster(
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
    prepro_modulation = matrix(rep(c(1, 1, 1, 0), each = 24*365), ncol = 4), 
    opts = opts_test
  )
  
  res_cluster <- antaresRead::readClusterDesc(opts = opts_test)
  
  # NULL as to effect to delete parameters
  opts_test <- editCluster(area = getAreas()[1], 
              cluster_name = levels(res_cluster$cluster)[1], 
              list_pollutants = list(
                "nh3"= 0.07, 
                "nox"= 0.07, 
                "pm2_5"= 0.07, 
                "pm5"= NULL), 
              add_prefix = FALSE,
              opts = opts_test)
  
  res_cluster <- antaresRead::readClusterDesc(opts = opts_test)
  
  res_cluster <- res_cluster[cluster %in% levels(res_cluster$cluster)[1]]
  
  vect_params <- as.vector(res_cluster[, c("nh3", 
                  "nox", 
                  "pm2_5")])
  
  # check values edited
  testthat::expect_true(all(c("nh3"= 0.07, 
                          "nox"= 0.07, 
                          "pm2_5"= 0.07) %in%
                          vect_params))
  
  # remove temporary study
  unlink(x = opts_test$studyPath, recursive = TRUE)
})
