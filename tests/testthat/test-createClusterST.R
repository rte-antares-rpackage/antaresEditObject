
# global params for structure v8.6 ----
setup_study_860(sourcedir860)
opts_test <- antaresRead::setSimulationPath(study_temp_path, "input")

path_master <- file.path(opts_test$inputPath, "st-storage")

if (opts_test$antaresVersion >= 860){
  test_that("Create short-term storage cluster (new feature v8.6)",{
    ## basics errors cases ----
    
    # default area with st cluster
    area_test_clust = "al" 
    
    # study parameters
    # version ? == is ST study compatibility
    # valid groups ?
    
    # valid area ?
    testthat::expect_error(createClusterST("INVALID_AREA", "cluster_name", opts = opts_test),
                           regexp = "is not a valid area name")
    
    # bad dimension of data parameters
    testthat::expect_error(createClusterST(area_test_clust, "cluster1", 
                                           PMAX_injection = matrix(1, 2, 2),
                                           opts = opts_test),
                           regexp = "Input data for")
    
    # cluster already exist
    name_st_clust <-levels(readClusterSTDesc(opts = opts_test)$cluster)
    testthat::expect_error(createClusterST(area_test_clust, 
                                           name_st_clust, 
                                           add_prefix = FALSE,
                                           opts = opts_test),
                           regexp = "already exist")
  
    ## default creation cluster ----
      
      ##
      # check parameters (ini file)
      ##
    
      # check name cluster
    area_test <- getAreas()[1]
    opts_test <- createClusterST(area_test, 
                                 "cluster1", 
                                 opts = opts_test) 
    
    namecluster_check <- paste(area_test, "cluster1", sep = "_")
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
                                               file = file.path(path_master, 
                                                                "series",
                                                                area_test, 
                                                                paste(area_test, "cluster1", sep = "_"),
                                                                "lower-rule-curve.txt"))
    # check default value and dimension
    testthat::expect_equal(dim(file_series), c(8760, 1))
    testthat::expect_equal(mean(file_series$V1), 0)
    
    # read series (with readInputTS)
    st_ts <- readInputTS(st_storage = "all", opts = opts_test)
    
    # check to find 5 names files created previously
    files_names <- unique(st_ts$name_file)
    
    # names files from code 
    original_files_names <- c("inflows", 
                              "lower-rule-curve", 
                              "PMAX-injection", 
                              "PMAX-withdrawal" , 
                              "upper-rule-curve")
    
    testthat::expect_true(all(original_files_names %in%
                                files_names))
    
    # check default values of txt files
    storage_value <- list(PMAX_injection = list(N=1, string = "PMAX-injection"),
                          PMAX_withdrawal = list(N=1, string = "PMAX-withdrawal"),
                          inflows = list(N=0, string = "inflows"),
                          lower_rule_curve = list(N=0, string = "lower-rule-curve"),
                          upper_rule_curve = list(N=1, string = "upper-rule-curve"))
    
    real_names_cols <- unlist(lapply(storage_value, `[[`, 2), use.names = FALSE)
    names(storage_value) <- real_names_cols
    
    df_ref_default_value <- data.table::setDT(lapply(storage_value, `[[`, 1), )
    df_ref_default_value <- melt(df_ref_default_value, 
                                 variable.name = "name_file", 
                                 value.name = "mean", 
                                 variable.factor = FALSE)
    
    df_ref_default_value <- df_ref_default_value[base::order(df_ref_default_value$name_file)]
    
    # mean of default TS created
    test_txt_value <- st_ts[area %in% area_test, 
                            list(mean=mean(`st-storage`)), 
                            by=name_file]
    
    # check default values
    testthat::expect_equal(df_ref_default_value$mean, test_txt_value$mean)
  
    
    ## creation cluster (explicit data) ----
    val <- 0.7
    val_mat <- matrix(val, 8760)
    
    opts_test <- createClusterST(area = area_test, 
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
    namecluster_check <- paste(area_test, "test_storage", sep = "_")
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
    opts_test <- removeClusterST(area = area_test, "cluster1", 
                                 opts = opts_test)
    
    testthat::expect_false(paste(area_test, "cluster1", sep = "_") %in% 
                             levels(readClusterSTDesc(opts = opts_test)$cluster))
    #Delete study
    unlink(opts_test$studyPath, recursive = TRUE)
    
    })
}


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
  
  ## removeClusterST()
  # On a non-existing cluster
  expect_error(removeClusterST(area = area, 
               cluster_name = "not_a_cluster",                   
               opts = opts), regexp = "Cluster can not be removed.")
  
  # On an existing cluster
  expect_no_error(removeClusterST(area = area, 
               cluster_name = cl_name,                   
               opts = opts))
  
  unlink(x = opts$studyPath, recursive = TRUE)
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


test_that("createClusterST(), editClusterST() and removeClusterST() work as expected if the cluster exists or does not exist", {
  
  ant_version <- "8.6.0"
  st_test <- paste0("my_study_860_", paste0(sample(letters,5), collapse = ""))
  suppressWarnings(opts <- createStudy(path = pathstd, study_name = st_test, antares_version = ant_version))
  area_test <- "zone1"
  opts <- createArea(name = area_test, opts = simOptions())
  
  ## createClusterST
  # Create a cluster on a non-existing area
  expect_error(createClusterST(area = "bla", cluster_name = "cluster_st_test_create", add_prefix = TRUE, storage_parameters = list("efficiency" = 0.6), opts = simOptions()),
               regexp = "is not a valid area name")
  # Create a non-existing cluster
  expect_no_error(createClusterST(area = area_test, cluster_name = "cluster_st_test_create", add_prefix = TRUE, storage_parameters = list("efficiency" = 0.6), opts = simOptions()))
  # Create an existing cluster - idempotence
  expect_error(createClusterST(area = area_test, cluster_name = "cluster_st_test_create", add_prefix = TRUE, storage_parameters = list("efficiency" = 0.7), opts = simOptions()),
               regexp = "Cluster already exists.")  
  # Create a non-existing cluster - CI
  expect_no_error(createClusterST(area = toupper(area_test), cluster_name = "clUstEr_st_tEst_crEAtE2", add_prefix = TRUE, storage_parameters = list("efficiency" = 0.6), opts = simOptions()))
  # Create an existing cluster - CI - idempotence
  expect_error(createClusterST(area = toupper(area_test), cluster_name = toupper("clUstEr_st_tEst_crEAtE2"), add_prefix = TRUE, storage_parameters = list("efficiency" = 0.7), opts = simOptions()),
               regexp = "Cluster already exists.")     
  
  ## editClusterST
  # Edit a cluster on a non-existing area
  expect_error(editClusterST(area = "bla", cluster_name = "cluster_st_not_exists", add_prefix = TRUE, storage_parameters = list("efficiency" = 0.6), opts = simOptions()),
               regexp = "is not a valid area name")
  # Edit a non-existing cluster
  expect_error(editClusterST(area = area_test, cluster_name = "cluster_st_not_exists", add_prefix = TRUE, storage_parameters = list("efficiency" = 0.6), opts = simOptions()),
               regexp = "Cluster 'zone1_cluster_st_not_exists' does not exist. It can not be edited.")
  # Edit an existing cluster
  expect_no_error(editClusterST(area = area_test, cluster_name = "cluster_st_test_create", add_prefix = TRUE, storage_parameters = list("efficiency" = 0.789), opts = simOptions()))
  # Edit the same existing cluster
  expect_no_error(editClusterST(area = area_test, cluster_name = "cluster_st_test_create", add_prefix = TRUE, storage_parameters = list("efficiency" = 0.890), opts = simOptions()))
  # Edit an existing cluster - CI
  expect_no_error(editClusterST(area = toupper(area_test), cluster_name = "ClUStER_st_tEst_crEAtE2", add_prefix = TRUE, storage_parameters = list("efficiency" = 0.789), opts = simOptions()))
  # Edit an existing cluster - CI - idempotence
  expect_no_error(editClusterST(area = toupper(area_test), cluster_name = toupper("clUstEr_st_tEst_crEAtE2"), add_prefix = TRUE, storage_parameters = list("efficiency" = 0.890), opts = simOptions()))
  
  ## removeClusterST
  # Remove a cluster on a non-existing area
  expect_error(removeClusterST(area = "bla", cluster_name = "cluster_st_not_exists", add_prefix = TRUE, opts = simOptions()),
               regexp = "is not a valid area name")
  # Remove a non-existing cluster
  expect_error(removeClusterST(area = area_test, cluster_name = "cluster_st_not_exists", add_prefix = TRUE, opts = simOptions()),
               regexp = "Cluster can not be removed")
  # Remove an existing cluster
  expect_no_error(removeClusterST(area = area_test, cluster_name = "cluster_st_test_create", add_prefix = TRUE, opts = simOptions()))
  # Remove an existing cluster - idempotence
  expect_error(removeClusterST(area = area_test, cluster_name = "cluster_st_test_create", add_prefix = TRUE, opts = simOptions()),
               regexp = "Cluster can not be removed")
  # Remove an existing cluster - CI
  expect_no_error(removeClusterST(area = area_test, cluster_name = "CLuSTeR_ST_TeST_CReaTe2", add_prefix = TRUE, opts = simOptions()))
  
  unlink(x = opts$studyPath, recursive = TRUE)
})
