
# global params for structure v8.6 ----
setup_study_860(sourcedir860)
opts_test <- antaresRead::setSimulationPath(study_temp_path, "input")

path_master <- file.path(opts_test$inputPath, "st-storage")


test_that("Create short-term storage cluster (new feature v8.6)",{
  
  if (opts_test$antaresVersion >= 860){
    area_test = getAreas()[1]
    
    # createClusterST throws error for invalid area name
    testthat::expect_error(createClusterST("INVALID_AREA", "cluster_name", opts = opts_test),
                 regexp = "is not a valid area name")
    
    # createClusterST throws error for incorrect number of rows in storage values.
    testthat::expect_error(createClusterST(area_test, "cluster1", 
                                           PMAX_injection = matrix(1, 2, 2),
                                           opts = opts_test))
      
    # check cluster exists with default values
    opts_test <- createClusterST(area_test, 
                    "cluster1", 
                    opts = opts_test) 
    
    testthat::expect_true(paste(area_test, "cluster1", sep = "_") %in% 
                            levels(readClusterSTDesc(opts = opts_test)$cluster))
    
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
    
    testthat::expect_equal(c("inflows", "lower-rule-curve", "PMAX-injection", "PMAX-withdrawal" , "upper-rule-curve"),
                           files_names)
    
      # chech default values of txt files
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
    
    # createClusterST throws error when cluster already exist.
    testthat::expect_error(createClusterST(area_test, 
                                 "cluster1",
                                 opts = opts_test),
                 regexp = "already exist")
      
    test_that("Remove storage cluster (new feature v8.6)", {
      # RemoveClusterST (if no cluster => function read return error => see readClusterDesc tests)
      opts_test <- removeClusterST(area = area_test, "cluster1", 
                                   opts = opts_test)
      
      testthat::expect_false(paste(area_test, "cluster1", sep = "_") %in% 
                               levels(readClusterSTDesc(opts = opts_test)$cluster))
    })
    
  }
  #Delete study
  unlink(opts_test$studyPath, recursive = TRUE)
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
                  unitcount = 1,
                  nominalcapacity = 8000,
                  `min-down-time` = 0,
                  `marginal-cost` = 0.010000,
                  `market-bid-cost` = 0.010000, 
                  PMAX_injection = matrix(1,8760),
                  PMAX_withdrawal = matrix(0.5,8760),
                  inflows = matrix(0.25,8760),
                  lower_rule_curve = matrix(0.2,8760),
                  upper_rule_curve = matrix(0.9,8760))
  
  # use getVariantCommands to catch information
    # here (specific st-storage : list with 1 group (parameters) + 5 data parameters)
  res_list <- getVariantCommands(last = 6)
  
  ## test first group of list for ini parameters
  action_api_1 <- res_list[[1]]
  
    # name of api instruction/action
  testthat::expect_equal(action_api_1$action, "create_st_storage")
    # check names and values parameters
  names_vector_parameters <-setdiff(names(action_api_1$args$parameters), 
                                    c("name", "group"))
      # just check for some parameters
  testthat::expect_true(all(c("min-down-time", "marginal-cost") 
                            %in% names_vector_parameters))
    # check casse name cluster
  name_ori <- paste0(area_name, "_", cluster_name)
  
  testthat::expect_equal(tolower(name_ori), 
                         action_api_1$args$parameters$name) 
  testthat::expect_equal(tolower(name_ori), 
                         action_api_1$args$storage_name) 
  
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
