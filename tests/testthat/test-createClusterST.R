

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
                  cluster_test_name, group_test_name,
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
  
  # default values
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


test_that("Test the behaviour of createClusterST() if the ST cluster already exists", {
  
  ant_version <- "8.6.0"
  st_test <- paste0("my_study_860_", paste0(sample(letters,5),collapse = ""))
  suppressWarnings(opts <- createStudy(path = pathstd, study_name = st_test, antares_version = ant_version))
  area <- "zone51"
  createArea(area)
  suppressWarnings(opts <- setSimulationPath(opts$studyPath, simulation = "input"))
  
  val <- 0.7
  val_mat <- matrix(val, 8760)
  cl_name <- "test_storage"
  createClusterST(area = area, 
                  cluster_name = cl_name, 
                  storage_parameters = storage_values_default()[1], 
                  PMAX_injection = val_mat, 
                  PMAX_withdrawal = val_mat, 
                  inflows = val_mat, 
                  lower_rule_curve = val_mat, 
                  upper_rule_curve = val_mat, 
                  opts = opts)
  
  suppressWarnings(opts <- setSimulationPath(opts$studyPath, simulation = "input"))
  
  ## createClusterST()
  # With overwrite FALSE  
  expect_error(createClusterST(area = area, 
                               cluster_name = cl_name, 
                               storage_parameters = storage_values_default()[1], 
                               PMAX_injection = val_mat, 
                               PMAX_withdrawal = val_mat, 
                               inflows = val_mat, 
                               lower_rule_curve = val_mat, 
                               upper_rule_curve = val_mat,
                               overwrite = FALSE,                    
                               opts = opts), regexp = "Cluster already exists.")
  
  # With overwrite TRUE  
  expect_no_error(createClusterST(area = area, 
                                  cluster_name = cl_name, 
                                  storage_parameters = storage_values_default()[1], 
                                  PMAX_injection = val_mat, 
                                  PMAX_withdrawal = val_mat, 
                                  inflows = val_mat, 
                                  lower_rule_curve = val_mat, 
                                  upper_rule_curve = val_mat,
                                  overwrite = TRUE,                    
                                  opts = opts))
  
  # Test case insensitive
  cl_name_2 <- "clUstEr_st_tEst_crEAtE2"
  expect_no_error(createClusterST(area = area, 
                                  cluster_name = cl_name_2, 
                                  storage_parameters = storage_values_default()[1], 
                                  PMAX_injection = val_mat, 
                                  PMAX_withdrawal = val_mat, 
                                  inflows = val_mat, 
                                  lower_rule_curve = val_mat, 
                                  upper_rule_curve = val_mat,
                                  overwrite = FALSE,                    
                                  opts = simOptions()))
  
  expect_error(createClusterST(area = toupper(area),
                               cluster_name = toupper(cl_name_2), 
                               storage_parameters = storage_values_default()[1], 
                               PMAX_injection = val_mat, 
                               PMAX_withdrawal = val_mat, 
                               inflows = val_mat, 
                               lower_rule_curve = val_mat, 
                               upper_rule_curve = val_mat,
                               overwrite = FALSE,                    
                               opts = simOptions()), regexp = "Cluster already exists.")
  
  ## removeClusterST()
  # On a non-existing area
  expect_error(removeClusterST(area = "bla",
                               cluster_name = cl_name,
                               add_prefix = TRUE,
                               opts = simOptions()), regexp = "is not a valid area name")
  
  # On a non-existing cluster
  expect_error(removeClusterST(area = area, 
                               cluster_name = "not_a_cluster",                   
                               opts = simOptions()), regexp = "Cluster can not be removed.")
  
  # On an existing cluster
  expect_no_error(removeClusterST(area = area, 
                                  cluster_name = cl_name,
                                  add_prefix = TRUE,               
                                  opts = simOptions()))
  
  # On an existing cluster - idempotence
  expect_error(removeClusterST(area = area, 
                               cluster_name = cl_name,                   
                               opts = simOptions()), regexp = "Cluster can not be removed.")
  
  # On an existing cluster case insensitive 
  expect_no_error(removeClusterST(area = area,
                                  cluster_name = "CLuSTeR_ST_TeST_CReaTe2",
                                  add_prefix = TRUE,
                                  opts = simOptions()))
  
  unlink(x = opts$studyPath, recursive = TRUE)
})

test_that("Test warning when storage_parameters incorrect", {
  
  ant_version <- "8.8.0"
  st_test <- "my_study_880"
  suppressWarnings(opts <- createStudy(path = pathstd, study_name = st_test, antares_version = ant_version))
  area <- "zone51"
  createArea(area)
  suppressWarnings(opts <- setSimulationPath(opts$studyPath, simulation = "input"))
  
  val <- 0.7
  val_mat <- matrix(val, 8760)
  cl_name <- "test_warning_storage"
  
  wrong_storage_parameters <- storage_values_default()
  wrong_storage_parameters$initiallevel <- 0.8 #different of 0.5
  
  expect_warning(createClusterST(area = area, 
                                 cluster_name = cl_name, 
                                 storage_parameters = wrong_storage_parameters, 
                                 PMAX_injection = val_mat, 
                                 PMAX_withdrawal = val_mat, 
                                 inflows = val_mat, 
                                 lower_rule_curve = val_mat, 
                                 upper_rule_curve = val_mat, 
                                 opts = opts), 
                 regexp = "`initiallevel` value will be replaced by 0.5")
  
})


# API ----

test_that("API Command test for createClusterST", {
  # Simulation parameters for api code
  opts_mock <- mockSimulationAPI(force = TRUE, 
                                 antares_version = "860")
  
  # create complete cluster st-storage
  
  area_name <- "area01"
  cluster_name <- "ClusTER01"
  
  # no casse sensitiv
  createClusterST(area = area_name, 
                  cluster_name = cluster_name, 
                  group = "Other", 
                  storage_parameters = storage_values_default(),
                  PMAX_injection = matrix(1,8760),
                  PMAX_withdrawal = matrix(0.5,8760),
                  inflows = matrix(0.25,8760),
                  lower_rule_curve = matrix(0.2,8760),
                  upper_rule_curve = matrix(0.9,8760))
  
  # use getVariantCommands to catch information
  # here (specific st-storage : `list` with 1 group (parameters) + 5 data parameters)
  res_list <- getVariantCommands(last = 6)
  
  ## test first group of list for ini parameters
  action_api_1 <- res_list[[1]]
  
  # name of api instruction/action
  testthat::expect_equal(action_api_1$action, "create_st_storage")
  # check names and values parameters
  names_st_paramas <- names(storage_values_default())
  names_vector_parameters <-setdiff(names(action_api_1$args$parameters), 
                                    c("name", "group"))
  # check if all parameters are present
  testthat::expect_true(all(names_st_paramas 
                            %in% names_vector_parameters))
  # check casse name cluster
  name_ori <- paste0(area_name, "_", cluster_name)
  
  testthat::expect_equal(tolower(name_ori), 
                         action_api_1$args$parameters$name) 
  
  ## test other group for data
  # search "replace_matrix" action
  index_data <- lapply(res_list, `[[`, 1) %in% 
    "replace_matrix"
  
  data_list <- res_list[index_data]
  
  # test for every floor in "args" : 
  # "target" (path of txt file)
  # "matrix" (data)
  data_path_files <- lapply(data_list, function(x){
    x$args$target
  })
  
  # test for every path, the path destination + name of txt file 
  # name txt files corresponding data parameters of function `createClusterST()`
  full_root_path_name <- file.path("input", "st-storage", "series", area_name,
                                   tolower(name_ori))
  
  # from code 
  # these names ares approved with antares desktop but not with API
  names_file_list <- c("PMAX-injection", "PMAX-withdrawal", "inflows", 
                       "lower-rule-curve", "upper-rule-curve")
  
  # reformat API 
  names_file_list <- transform_name_to_id(names_file_list, id_dash = TRUE)
  
  # check root path for every file
  is_good_path <- lapply(data_path_files, function(x){
    grepl(pattern = full_root_path_name, x = x)
  })
  
  testthat::expect_true(all(unlist(is_good_path))) 
  
  # check names of files
  names_file_api <- lapply(data_path_files, function(x){
    regmatches(x,regexpr("([^\\/]+$)",x))
  })
  
  testthat::expect_true(all(unlist(names_file_api) %in% 
                              names_file_list))
})
