

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
