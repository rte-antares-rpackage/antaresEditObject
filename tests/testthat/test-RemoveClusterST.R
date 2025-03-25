# >=860 ----

test_that("New feature v8.6", {
  suppressWarnings(
    createStudy(path = tempdir(), 
                study_name = "st-storage", 
                antares_version = "8.6.0"))
  
  # just need at least one area
  area_test_clust = "al" 
  createArea(name = area_test_clust)
  
  testthat::test_that("remove the only one cluster", {
    # at least need 1 st cluster
    createClusterST(area = area_test_clust, 
                    cluster_name = "cluster_init")
    
    # remove cluster
    removeClusterST(area = area_test_clust, 
                    cluster_name = "cluster_init")
    
    # removed from ini file ?
    
    # read prop
    path_st_ini <- file.path("input", 
                             "st-storage", 
                             "clusters", 
                             area_test_clust,
                             "list")
    
    read_ini <- antaresRead::readIni(path_st_ini)
    target_prop <- read_ini[[paste(area_test_clust, 
                                   "cluster_init",
                                   sep = "_")]]
    
    testthat::expect_null(target_prop)
    
    # remove directory ? 
    path_st_dir <- file.path("input", 
                             "st-storage", 
                             "clusters", 
                             "series",
                             area_test_clust,
                             "al_cluster_init")
    
    testthat::expect_false(dir.exists(path_st_dir))
  })
  
  deleteStudy()
})

# >=880 ----

testthat::test_that("New feature v8.8", {
  suppressWarnings(
    createStudy(path = tempdir(), 
                study_name = "st-storage", 
                antares_version = "8.8.0"))
  
  # just need at least one area
  area_test_clust = "al" 
  createArea(name = area_test_clust)
  
  testthat::test_that("remove the only one cluster", {
    # at least need 1 st cluster
    createClusterST(area = area_test_clust, 
                    cluster_name = "cluster_init")
    
    # remove cluster
    removeClusterST(area = area_test_clust, 
                    cluster_name = "cluster_init")
    
    # removed from ini file ?
    
    # read prop
    path_st_ini <- file.path("input", 
                             "st-storage", 
                             "clusters", 
                             area_test_clust,
                             "list")
    
    read_ini <- antaresRead::readIni(path_st_ini)
    target_prop <- read_ini[[paste(area_test_clust, 
                                   "cluster_init",
                                   sep = "_")]]
    
    testthat::expect_null(target_prop)
    
    # remove directory ? 
    path_st_dir <- file.path("input", 
                             "st-storage", 
                             "clusters", 
                             "series",
                             area_test_clust,
                             "al_cluster_init")
    
    testthat::expect_false(dir.exists(path_st_dir))
  })
  
  deleteStudy()
})

# >=920 ----

testthat::test_that("New feature v9.2", {
  suppressWarnings(
    createStudy(path = tempdir(), 
                study_name = "st-storage", 
                antares_version = "9.2"))
  
  # just need at least one area
  area_test_clust = "al" 
  createArea(name = area_test_clust)
  
  testthat::test_that("remove the only one cluster", {
    # at least need 1 st cluster
    createClusterST(area = area_test_clust, 
                    cluster_name = "cluster_init")
    
    # remove cluster
    removeClusterST(area = area_test_clust, 
                    cluster_name = "cluster_init")
    
    # removed from ini file ?
    
    # read prop
    path_st_ini <- file.path("input", 
                             "st-storage", 
                             "clusters", 
                             area_test_clust,
                             "list")
    
    read_ini <- antaresRead::readIni(path_st_ini)
    target_prop <- read_ini[[paste(area_test_clust, 
                                   "cluster_init",
                                   sep = "_")]]
    
    testthat::expect_null(target_prop)
    
    # remove directory ? 
    path_st_dir <- file.path("input", 
                             "st-storage", 
                             "clusters", 
                             "series",
                             area_test_clust,
                             "al_cluster_init")
    
    testthat::expect_false(dir.exists(path_st_dir))
  })
  
  deleteStudy()
})
