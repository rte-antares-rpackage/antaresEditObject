# >=860 ----
suppressWarnings(
  createStudy(path = tempdir(), 
              study_name = "st-storage", 
              antares_version = "8.6.0"))

# just need at least one area
area_test_clust = "al" 
createArea(name = area_test_clust)

test_that("Check area", {
  createClusterST(area = area_test_clust, 
                  cluster_name = "check_area")
  
  expect_error(
    removeClusterST(area = "check_area_area", 
                    cluster_name = "check_area"), 
    regexp = "'check_area_area' is not a valid area name, possible names are: al"
  )
  
  test_that("no case sensitive", {
    expect_no_error(
      removeClusterST(area = "AL", 
                      cluster_name = "check_area"))
  })
})

test_that("Check cluster name", {
  createClusterST(area = area_test_clust, 
                  cluster_name = "check_cluster_name")
  
  expect_warning(
    removeClusterST(area = area_test_clust, 
                    cluster_name = "check_cluster"), 
    regexp = "Cluster 'al_check_cluster' you want to remove doesn't seem to exist in area 'al'"
  )
  
  test_that("no case sensitive", {
    expect_no_warning(
      removeClusterST(area = area_test_clust, 
                      cluster_name = "check_ClusteR_NAME"))
  })
})

test_that("remove with 'prefix=TRUE'", {
  # at least need 1 st cluster
  createClusterST(area = area_test_clust, 
                  cluster_name = "prefix")
  
  # remove cluster
  removeClusterST(area = area_test_clust, 
                  cluster_name = "prefix")
  
  # removed from ini file ?
  
  # read prop
  path_st_ini <- file.path("input", 
                           "st-storage", 
                           "clusters", 
                           area_test_clust,
                           "list")
  
  read_ini <- antaresRead::readIni(path_st_ini)
  target_prop <- read_ini[[paste(area_test_clust, 
                                 "prefix",
                                 sep = "_")]]
  
  expect_null(target_prop)
  
  # remove directory ? 
  path_st_dir <- file.path("input", 
                           "st-storage", 
                           "clusters", 
                           "series",
                           area_test_clust,
                           "al_cluster_init")
  
  expect_false(dir.exists(path_st_dir))
})

test_that("remove with 'prefix=FALSE'",{
  # no prefix
  createClusterST(
    area = area_test_clust, 
    cluster_name = "no_prefix")
  
  # no prefix (add area)
  removeClusterST(
    area = area_test_clust, 
    cluster_name = "al_no_prefix", 
    add_prefix = FALSE)
  
  # removed from ini file ?
  
  # read prop
  path_st_ini <- file.path("input", 
                           "st-storage", 
                           "clusters", 
                           area_test_clust,
                           "list")
  
  read_ini <- antaresRead::readIni(path_st_ini)
  target_prop <- read_ini[[paste(area_test_clust, 
                                 "al_no_prefix",
                                 sep = "_")]]
  
  expect_null(target_prop)
  
  # remove directory ? 
  path_st_dir <- file.path("input", 
                           "st-storage", 
                           "clusters", 
                           "series",
                           area_test_clust,
                           "al_no_prefix")
  
  expect_false(dir.exists(path_st_dir))
})

deleteStudy()


# >=880 ----
suppressWarnings(
  createStudy(path = tempdir(), 
              study_name = "st-storage", 
              antares_version = "8.8.0"))

# just need at least one area
area_test_clust = "al" 
createArea(name = area_test_clust)

test_that("Remove cluster", {
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
  
  expect_null(target_prop)
  
  # remove directory ? 
  path_st_dir <- file.path("input", 
                           "st-storage", 
                           "clusters", 
                           "series",
                           area_test_clust,
                           "al_cluster_init")
  
  expect_false(dir.exists(path_st_dir))
})

deleteStudy()


# >=920 ----
suppressWarnings(
  createStudy(path = tempdir(), 
              study_name = "st-storage", 
              antares_version = "9.2"))

# just need at least one area
area_test_clust = "al" 
createArea(name = area_test_clust)

test_that("Remove cluster without constraints", {
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
  
  expect_null(target_prop)
  
  # remove directory ? 
  path_st_dir <- file.path("input", 
                           "st-storage", 
                           "clusters", 
                           "series",
                           area_test_clust,
                           "al_cluster_init")
  
  expect_false(dir.exists(path_st_dir))
})


test_that("Remove cluster with constraints", {
  # given
  name_no_prefix <- "remove_constraints"
  
  constraints_properties <- list(
    "withdrawal-1"=list(
      variable = "withdrawal",
      operator = "equal",
      hours = c("[1,3,5]", 
                "[120,121,122,123,124,125,126,127,128]")
    ),
    "netting-1"=list(
      variable = "netting",
      operator = "less",
      hours = c("[1, 168]")
    ))
  
  good_ts <- data.table::as.data.table(matrix(10, 8760))
  constraints_ts <- list(
    "withdrawal-1"=good_ts,
    "netting-1"=good_ts)
  
  createClusterST(area = area_test_clust, 
                  cluster_name = name_no_prefix, 
                  constraints_properties = constraints_properties, 
                  constraints_ts = constraints_ts)
  
  # when
  # remove cluster
  removeClusterST(area = area_test_clust, 
                  cluster_name = name_no_prefix)
  
  # then
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
  
  expect_null(target_prop)
  
  # remove directory ? 
  path_st_dir <- file.path("input", 
                           "st-storage", 
                           "clusters", 
                           "series",
                           area_test_clust,
                           "al_cluster_init")
  
  expect_false(dir.exists(path_st_dir))
})

deleteStudy()

