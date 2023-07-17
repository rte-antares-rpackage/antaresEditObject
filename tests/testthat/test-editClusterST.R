
test_that("edit st-storage clusters (only for study >= v8.6.0" , {
  # global params for structure v8.6 ----
  setup_study_860(sourcedir860)
  opts_test <- antaresRead::setSimulationPath(study_temp_path, "input")
  
  # areas tests 
  area_test = getAreas()[1]
   
  # create tests clusters
  opts_test <- createClusterST(area_test, 
                  "cluster-st-1", 
                  opts = opts_test) 
  
  opts_test <- createClusterST(area_test, 
                  "cluster-st-2", 
                  opts = opts_test) 
  
  st_clusters <- readClusterSTDesc(opts = opts_test)
  
  # edit cluster
  val <- 0.007
  opts_test <- editClusterST(area = area_test, 
                cluster_name = levels(st_clusters$cluster)[1], 
                test_param1= "test",
                test_param2= 0.002154,
                PMAX_injection = matrix(val, 8760), 
                PMAX_withdrawal = matrix(val, 8760),
                inflows =  matrix(0.007, 8760), 
                lower_rule_curve = matrix(val, 8760), 
                upper_rule_curve = matrix(val, 8760),
                opts = opts_test, 
                add_prefix = FALSE)
  
  st_clusters <- readClusterSTDesc(opts = opts_test)
  
  res <- st_clusters[cluster == levels(st_clusters$cluster)[1], test.param1, test.param2]
  
  # test parameters values edited
  testthat::expect_true(all(res %in% c(0.002154, "test")))
  
  # test data value (with fread_antares)
  path_dir_test <- file.path(opts_test$inputPath, "st-storage", "series", area_test, 
                             paste(area_test, "cluster-st-1", sep = "_"))
  files_test <- list.files(path_dir_test, full.names = TRUE)
  l_file_series <- lapply(files_test, antaresRead:::fread_antares, opts = opts_test)
  
  value_test <- mean(sapply(l_file_series, 
                       function(.x){
                         mean(.x$V1)}))
  
  testthat::expect_equal(value_test, val)
  
  # test data value with readInputTS
  st_ts <- readInputTS(st_storage = "all", opts = opts_test)
  
  # check clusters
  testthat::expect_true(all(levels(st_clusters$cluster) %in% unique(st_ts$cluster)))
  
  # delete study
  unlink(opts_test$studyPath, recursive = TRUE)
})



# API ----

test_that("API Command test for editClusterST", {
  # Simulation parameters for api code
  opts_mock <- mockSimulationAPI(force = TRUE,
                                 antares_version = "860")
  
  # create complete cluster st-storage
  area_name <- "area01"
  cluster_name <- "ClusTER01"

  # create complete cluster st-storage
  editClusterST(area = area_name,
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
  # here (specific st-storage : list with 5 group (parameters) + 5 data parameters)
  res_list <- getVariantCommands(last = 10)

  ## test first group of list for ini parameters
  action_api_1 <- res_list[[1]]

  # name of api instruction/action
  testthat::expect_equal(action_api_1$action, "update_config")
  # check "args" name parameters (just for one parameter/one action)
  param_target <- action_api_1$args$target
  param_target <- regmatches(param_target, regexpr("([^\\/]+$)",param_target))
  testthat::expect_equal(param_target, "unitcount")
  
  # check "data" (value of parameter)
  testthat::expect_equal("1.000000", action_api_1$args$data)

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
    # name txt files corresponding data parameters of function `editClusterST()`
    # check casse of name cluster name in every path
  name_ori <- paste0(area_name, "_", cluster_name)
  
  full_root_path_name <- file.path("input", "st-storage", "series", area_name,
                                   tolower(name_ori))
  
  # from code 
  # these names are approved with antares desktop but not with API
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

