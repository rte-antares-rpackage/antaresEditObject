
# >=860 ----
test_that("Create short-term storage cluster (new feature v8.6)",{
  ## basics errors cases ----
  suppressWarnings(
    createStudy(path = tempdir(), 
                study_name = "st-storage", 
                antares_version = "8.6.0"))
  
  # default area with st cluster
  area_test_clust = "al" 
  createArea(name = area_test_clust)
  
  # study parameters
  # version ? == is ST study compatibility
  # valid groups ?
  
  # valid area ?
  testthat::expect_error(createClusterST("INVALID_AREA", "cluster_name"),
                         regexp = "is not a valid area name")
  
  # bad dimension of data parameters
  cluster_test_name = "cluster34" 
  group_test_name = "Other1" 
  testthat::expect_error(createClusterST(area_test_clust, cluster_test_name, group_test_name,
                                         PMAX_injection = matrix(1, 2, 2)),
                         regexp = "Input data for")
  
  # cluster already exist in given area, with same name and group
  createClusterST(area_test_clust, 
                  cluster_test_name, 
                  group_test_name,
                  add_prefix = TRUE)
  
  testthat::expect_error(createClusterST(area_test_clust, 
                                         cluster_test_name, group_test_name,
                                         add_prefix = TRUE),
                         regexp = "already exist")

  ## default creation cluster ----
    
    ##
    # check parameters (ini file)
    ##
  
    # check name cluster
  opts_test <- createClusterST(area_test_clust, 
                               "cluster1") 

  
  namecluster_check <- paste(area_test_clust, "cluster1", sep = "_")
  testthat::expect_true(namecluster_check %in% 
                          levels(readClusterSTDesc(opts = opts_test)$cluster))
  
    # check default parameters(names + values)
  info_clusters <- readClusterSTDesc()
  info_clusters <- info_clusters[cluster %in% namecluster_check, ]
  
  # default values (only v860 properties)
  default_values <- storage_values_default()
  
  info_clusters <- info_clusters[, .SD, .SDcols= names(default_values)]
  
  # compare to default list
  info_clusters <- as.list(info_clusters)
  
  testthat::expect_equal(default_values, info_clusters)
  
  ##
  # check data (series files)
  ##
  
  # read series (with fread_antares)
  file_series <- antaresRead:::fread_antares(opts = opts_test,
                                             file = file.path(opts_test$inputPath, "st-storage",
                                                              "series",
                                                              area_test_clust,
                                                              namecluster_check,
                                                              "lower-rule-curve.txt"))
  # # check default value and dimension
  testthat::expect_equal(dim(file_series), c(8760, 1))
  testthat::expect_equal(mean(file_series$V1), 0)
  # 
  # read series (with readInputTS)
  st_ts <- readInputTS(st_storage = "all", opts = opts_test)
  # 
  # check to find 5 names files created previously
  files_names <- unique(st_ts$name_file)
  # 
  # names files from code 
  original_files_names <- c("inflows", 
                             "lower-rule-curve", 
                            "PMAX-injection",
                            "PMAX-withdrawal" ,
                            "upper-rule-curve")
  # 
  testthat::expect_true(all(original_files_names %in%
                              files_names))
  # 
  # check default values of txt files
  storage_value <- list(PMAX_injection = list(N=1, string = "PMAX-injection"),
                        PMAX_withdrawal = list(N=1, string = "PMAX-withdrawal"),
                        inflows = list(N=0, string = "inflows"),
                        lower_rule_curve = list(N=0, string = "lower-rule-curve"),
                        upper_rule_curve = list(N=1, string = "upper-rule-curve"))
  # 
  real_names_cols <- unlist(lapply(storage_value, `[[`, 2), use.names = FALSE)
  names(storage_value) <- real_names_cols

  df_ref_default_value <- data.table::setDT(lapply(storage_value, `[[`, 1), )
  df_ref_default_value <- melt(df_ref_default_value,
                               variable.name = "name_file",
                               value.name = "mean",
                               variable.factor = FALSE)

  # Sort by name_file
  df_ref_default_value <- df_ref_default_value[base::order(df_ref_default_value$name_file)]

  # mean of default TS created
  test_txt_value <- st_ts[area %in% area_test_clust,
                          list(mean=mean(`st-storage`)),
                          by=name_file]

  # check default values
  testthat::expect_equal(df_ref_default_value$mean, test_txt_value$mean)
  # 
  
  ## creation cluster (explicit data) ----
  val <- 0.7
  val_mat <- matrix(val, 8760)
  
  opts_test <- createClusterST(area = area_test_clust, 
                  cluster_name = "test_storage", 
                  storage_parameters = storage_values_default()[1], 
                  PMAX_injection = val_mat, 
                  PMAX_withdrawal = val_mat, 
                  inflows = val_mat, 
                  lower_rule_curve = val_mat, 
                  upper_rule_curve = val_mat, 
                  overwrite = TRUE, 
                  opts = opts_test)
  
  ## check name cluster created
  namecluster_check <- paste(area_test_clust, "test_storage", sep = "_")
  testthat::expect_true(namecluster_check %in% 
                          levels(readClusterSTDesc(opts = opts_test)$cluster))
  
  ## check data
  
  # read series (with readInputTS)
  st_ts <- readInputTS(st_storage = "all", opts = opts_test)
  
  # check to find 5 names files created previously
  filter_st_ts <- st_ts[cluster %in% namecluster_check, 
                        list(mean=mean(`st-storage`)), 
                        by=name_file]
  
  testthat::expect_true(all(filter_st_ts$name_file %in% 
                           original_files_names))
  testthat::expect_equal(val, unique(filter_st_ts$mean))
  

## remove cluster----  
  # RemoveClusterST (if no cluster => function read return error => see readClusterDesc tests)
  opts_test <- removeClusterST(area = area_test_clust, "cluster1", 
                               opts = opts_test)
  
  testthat::expect_false(paste(area_test_clust, "cluster1", sep = "_") %in% 
                           levels(readClusterSTDesc(opts = opts_test)$cluster))
  #Delete study
  unlink(opts_test$studyPath, recursive = TRUE)
  
  })

# >=880 ----

test_that("Create short-term storage cluster (new feature v8.8.0)",{
  ## basics errors cases ----
  suppressWarnings(
    createStudy(path = tempdir(), 
                study_name = "st-storage880", 
                antares_version = "8.8.0"))
  
  # default area with st cluster
  area_test_clust = "al" 
  createArea(name = area_test_clust)
  
  # default 
  createClusterST(area = area_test_clust, 
                  cluster_name = "default")
  
  read_prop <- readClusterSTDesc()
  
  # "enabled" must be present with TRUE values default 
  testthat::expect_true("enabled"%in%names(read_prop))
  testthat::expect_true(read_prop$enabled[1]%in%TRUE)
  
  deleteStudy()
  })








