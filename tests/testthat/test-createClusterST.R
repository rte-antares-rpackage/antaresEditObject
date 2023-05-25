
# global params for structure v8.6 ----
setup_study_850(sourcedir850)
opts_test <- antaresRead::setSimulationPath(study_temp_path, "input")

# Need to create a "st-storage" folder
path_master <- file.path(opts_test$inputPath, "st-storage")
dir.create(path_master)
dir.create(file.path(path_master, "clusters"))
dir.create(file.path(path_master, "series"))
# temporary to test with "860"
# force version
opts_test$antaresVersion <- 860

test_that("Create short-term storage cluster (new feature v8.6)",{
  
  if (opts_test$antaresVersion >= 860){
    #TODO Use createStudy and createArea instead
    area_test = getAreas()[1]
    dir.create(file.path(path_master, "clusters",area_test))
    writeIni(NULL, file.path(path_master, "clusters", area_test, "list"))
    
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
    
    opts_test$antaresVersion <- 860
    
    testthat::expect_true(paste(area_test, "cluster1", sep = "_") 
                %in% levels(readClusterSTDesc(opts = opts_test)$cluster))
    
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
    test_txt_value <- st_ts[, list(mean=mean(`st-storage`)), by=name_file]
    
    # check default values
    testthat::expect_equal(df_ref_default_value$mean, test_txt_value$mean)
    
    # createClusterST throws error when cluster already exist.
    testthat::expect_error(createClusterST(area_test, 
                                 "cluster1",
                                 opts = opts_test),
                 regexp = "already exist")
      
    test_that("Remove storage cluster (new feature v8.6)", {
      # RemoveClusterST (if no cluster => function read return error => see readClusterDesc tests)
      removeClusterST(area = area_test, "cluster1")
      
      testthat::expect_error(readClusterSTDesc(opts = opts_test))
    })
    
  }
  #Delete study
  unlink(opts_test$studyPath, recursive = TRUE)
})
