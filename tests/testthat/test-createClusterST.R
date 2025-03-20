
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
  
  # test restrictions on 'group' parameter
  testthat::test_that("static 'group",{
    testthat::expect_error(
      createClusterST(area = area_test_clust, 
                      cluster_name = "bad_group", 
                      group = "not_allowed"), 
      regexp = paste0(
        "Group: '", "not_allowed", "' is not a valid name recognized by Antares,"
      )
    )
    
  })
  
  deleteStudy()
  })

# >=9.2 ---- 
testthat::test_that("New features v9.2",{
  suppressWarnings(
    createStudy(path = tempdir(), 
                study_name = "st-storage9.2", 
                antares_version = "9.2"))
  
  # default area with st cluster
  area_test_clust = "al" 
  createArea(name = area_test_clust)
  
  
  # TEST dynamic groups
  testthat::test_that("Allow dynamic `group`",{
    # default with group
    createClusterST(area = area_test_clust, 
                    cluster_name = "dynamic_grp", 
                    group = "toto")
    
    # read properties
    opts_ <- simOptions()
    st_path <- file.path("input",
                         "st-storage", 
                         "clusters", 
                         area_test_clust, 
                         "list")
    
    st_file <- readIni(pathIni = st_path)
    
    # group has no restrictions
    testthat::expect_equal(st_file[[names(st_file)]][["group"]], 
                           "toto")
  })
  
  
  # ALL TEST about new properties 
  testthat::test_that("New properties",{
    testthat::test_that("Default values",{
      
      # default call 
      createClusterST(area = area_test_clust, 
                      cluster_name = "default_prop")
      
      # read prop
      path_st_ini <- file.path("input", 
                               "st-storage", 
                               "clusters", 
                               area_test_clust,
                               "list")
      
      read_ini <- antaresRead::readIni(path_st_ini)
      target_prop <- read_ini[[paste(area_test_clust, 
                                     "default_prop",
                                     sep = "_")]]
      
      # test default values
      testthat::expect_equal(
        target_prop[setdiff(names(target_prop), 
                            c("name", "group"))], 
        storage_values_default())
    })
    
    testthat::test_that("Wrong type/values",{
      # add new parameters 
      all_params <- storage_values_default()
      
      # "efficiencywithdrawal"
      # type
      all_params[["efficiencywithdrawal"]] <- TRUE
      
      testthat::expect_error(
        createClusterST(area = area_test_clust, 
                        cluster_name = "err", 
                        storage_parameters = all_params), 
        regexp = "x does not inherit from class numeric"
      )
      
      # value
      all_params[["efficiencywithdrawal"]] <- 2.89
      
      testthat::expect_error(
        createClusterST(area = area_test_clust, 
                        cluster_name = "err", 
                        storage_parameters = all_params), 
        regexp = "efficiencywithdrawal must be in range 0-1"
      )
     
      
      # "penalize-variation-injection"
      all_params <- storage_values_default()
      
      # type
      all_params[["penalize-variation-injection"]] <- 0.9
      
      testthat::expect_error(
        createClusterST(area = area_test_clust, 
                        cluster_name = "err", 
                        storage_parameters = all_params), 
        regexp = "does not inherit from class logical"
      )
      
      # NO TEST value (only TRUE/FALSE)
   
      
      # "penalize-variation-withdrawal"
      all_params <- storage_values_default()
      
      # type
      all_params[["penalize-variation-withdrawal"]] <- "area"
      
      testthat::expect_error(
        createClusterST(area = area_test_clust, 
                        cluster_name = "err", 
                        storage_parameters = all_params), 
        regexp = "does not inherit from class logical"
      )
      
      # NO TEST value (only TRUE/FALSE)
    })
    
    testthat::test_that("Add right values",{
      # add new parameters 
      all_params <- storage_values_default()
      all_params[["efficiencywithdrawal"]] <- 0.9
      all_params[["penalize-variation-injection"]] <- TRUE
      all_params[["penalize-variation-withdrawal"]] <- TRUE
      
      # default with new parameters 
      createClusterST(area = area_test_clust, 
                      cluster_name = "new_properties", 
                      storage_parameters = all_params)
      
      # read prop
      path_st_ini <- file.path("input", 
                               "st-storage", 
                               "clusters", 
                               area_test_clust,
                               "list")
      
      read_ini <- antaresRead::readIni(path_st_ini)
      target_prop <- read_ini[[paste(area_test_clust, 
                                     "new_properties",
                                     sep = "_")]]
      
      # test params created if identical with .ini readed 
      testthat::expect_equal(
        target_prop[setdiff(names(target_prop), 
                            c("name", "group"))], 
        all_params)
    })
    
  })
  
  # ALL TEST about TS values
  testthat::test_that("New TS Values",{
    testthat::test_that("Default TS dim and values",{
      # default with new parameters 
      createClusterST(area = area_test_clust, 
                      cluster_name = "default_ts")
      
      # read series 
      list_value_920 <- list(
        cost_injection = list(N=0, string = "cost-injection"),
        cost_withdrawal = list(N=0, string = "cost-withdrawal"),
        cost_level = list(N=0, string = "cost-level"),
        cost_variation_injection = list(N=0, string = "cost-variation-injection"),
        cost_variation_withdrawal = list(N=0, string = "cost-variation-withdrawal"))
      
      names_files <- unlist(lapply(list_value_920, 
                            function(x)x[["string"]]), 
                            use.names = FALSE)
      opts_ <- simOptions()
      path_ts <- file.path(opts_$inputPath, 
                           "st-storage",
                           "series",
                           area_test_clust,
                           "al_default_ts",
                           paste0(names_files, 
                                  ".txt"))
      
      files_series <- lapply(path_ts, 
                             data.table::fread) 
      
      # test dim all equal
      dim_files_series <- sapply(files_series, 
                                 dim) 
      testthat::expect_equal(mean(dim_files_series[1,]), 8760)
      testthat::expect_equal(mean(dim_files_series[2,]), 1)
      
      # test all value equal to 0 
      values_files_series <- sapply(files_series, 
                                    sum)
      testthat::expect_equal(sum(values_files_series), 0)
    })
    
    testthat::test_that("Wrong dim TS",{
      # like 8.6, these TS are dim [8760;1]
      bad_ts <- matrix(3, 8760*2, ncol = 2)
      
      # default with bad TS (just test 2 param)
      testthat::expect_error(
        createClusterST(area = area_test_clust, 
                        cluster_name = "wrong_ts_dim", 
                        cost_injection = bad_ts), 
        regexp = "Input data for cost_injection must be 8760\\*1"
      )
      testthat::expect_error(
        createClusterST(area = area_test_clust, 
                        cluster_name = "wrong_ts_dim", 
                        cost_withdrawal = bad_ts), 
        regexp = "Input data for cost_withdrawal must be 8760\\*1"
      )
    })
    
    testthat::test_that("Add right TS values",{
      good_ts <- matrix(0.7, 8760)
      
      # default with new optional TS
      createClusterST(area = area_test_clust, 
                      cluster_name = "good_ts_value", 
                      cost_injection = good_ts, 
                      cost_withdrawal = good_ts, 
                      cost_level = good_ts, 
                      cost_variation_injection = good_ts,
                      cost_variation_withdrawal = good_ts,overwrite = TRUE)
      
      # read series 
      list_value_920 <- list(
        cost_injection = list(N=0, string = "cost-injection"),
        cost_withdrawal = list(N=0, string = "cost-withdrawal"),
        cost_level = list(N=0, string = "cost-level"),
        cost_variation_injection = list(N=0, string = "cost-variation-injection"),
        cost_variation_withdrawal = list(N=0, string = "cost-variation-withdrawal"))
      
      names_files <- unlist(lapply(list_value_920, 
                                   function(x)x[["string"]]), 
                            use.names = FALSE)
      opts_ <- simOptions()
      path_ts <- file.path(opts_$inputPath, 
                           "st-storage",
                           "series",
                           area_test_clust,
                           "al_good_ts_value",
                           paste0(names_files, 
                                  ".txt"))
      
      files_series <- lapply(path_ts, 
                             data.table::fread) 
    
      # test all value not equal to 0 (default)
      values_files_series <- sapply(files_series, 
                                    sum)
      testthat::expect_true(sum(values_files_series)>0)
    })
    
  })
  
  deleteStudy()
})






