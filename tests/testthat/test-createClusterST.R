
# >=860 ----
suppressWarnings(
  createStudy(path = tempdir(), 
              study_name = "st-storage", 
              antares_version = "8.6.0"))

# just need at least one area
area_test_clust = "al" 
createArea(name = area_test_clust)

test_that("study opts parameters",{
   expect_error(
    createClusterST(
      area = area_test_clust, 
      cluster_name = "err",
      opts = list(studyName="test",
                  studyPath="C:/Users/beta/AppData/Local/Temp/RtmpQtOZyt/st-storage")), 
    regexp = "opts does not inherit from class simOptions"
  )
})

test_that("create cluster only for >=8.6",{
  bad_opts <- simOptions()
  bad_opts$antaresVersion <- 850
  
  expect_error(
    createClusterST(
      area = area_test_clust, 
      cluster_name = "err",
      opts = bad_opts), 
    regexp = "only available if using Antares >= 8.6.0"
  )
})

test_that("Check group",{
  expect_error(
    createClusterST(
      area = area_test_clust, 
      cluster_name = "err", 
      group = "myGroup"), 
    regexp = "Group: 'myGroup' is not a valid name recognized by Antares"
  )
})

test_that("Check area",{
  expect_error(
    createClusterST(
      area = "area_test_clust", 
      cluster_name = "err"), 
    regexp = "'area_test_clust' is not a valid area name, possible names are: al"
  )
  test_that("no case sensitive",{
    expect_no_error(
      createClusterST(
        area = "AL", 
        cluster_name = "case_sensitive"))
  })
})

test_that("Check input list 'storage_parameters'",{
  # respect list format
  expect_error(
    createClusterST(
      area = area_test_clust, 
      cluster_name = "err", 
      storage_parameters = c(efficiency=1)), 
    regexp = "storage_parameters does not inherit from class list"
  )
  
  # list with formatted names
  expect_error(
    createClusterST(
      area = area_test_clust, 
      cluster_name = "err", 
      storage_parameters = list(efficiencyy=1)), 
    regexp = "Parameter 'st-storage' must be named with the following elements: efficiency, reservoircapacity"
  )
  
  # check values parameters
  # check is ratio ? 
  expect_error(
    createClusterST(
      area = area_test_clust, 
      cluster_name = "err", 
      storage_parameters = list(efficiency = 2, 
                                reservoircapacity = 100)), 
    regexp = "efficiency must be in range 0-1"
  )
  
  # check positive capacity ? 
  expect_error(
    createClusterST(
      area = area_test_clust, 
      cluster_name = "err", 
      storage_parameters = list(efficiency = 0.9, 
                                reservoircapacity = -100)), 
    regexp = "reservoircapacity must be >= 0"
  )
  
  # check is logical ? 
  expect_error(
    createClusterST(
      area = area_test_clust, 
      cluster_name = "err", 
      storage_parameters = list(efficiency = 0.9, 
                                reservoircapacity = 100,
                                initialleveloptim = "false")), 
    regexp = 'list_values\\[\\[\\"initialleveloptim\\"\\]\\] does not inherit from class logical'
  )
})


test_that("Check dimension TS input",{
  # test col dim
  bad_matrix_data_dim <- matrix(1, 8760*2, ncol = 2)
  
  expect_error(
    createClusterST(
      area = area_test_clust, 
      cluster_name = "err", 
      PMAX_injection = bad_matrix_data_dim), 
    regexp = "Input data for PMAX_injection must be 8760\\*1"
  )
  
  # test raw dim
  bad_matrix_data_dim <- matrix(1, 8784)
  
  expect_error(
    createClusterST(
      area = area_test_clust, 
      cluster_name = "err", 
      PMAX_injection = bad_matrix_data_dim), 
    regexp = "Input data for PMAX_injection must be 8760\\*1"
  )
})

test_that("Prefix is working?",{
  # default with prefix
  createClusterST(
    area = area_test_clust, 
    cluster_name = "prefix")
  
  # no prefix
  createClusterST(
    area = area_test_clust, 
    cluster_name = "no_prefix", 
    add_prefix = FALSE)
  
  # read prop
  path_st_ini <- file.path("input", 
                           "st-storage", 
                           "clusters", 
                           area_test_clust,
                           "list")
  
  read_ini <- antaresRead::readIni(path_st_ini)
  
  # test 
  expect_true(
    "al_prefix" %in% names(read_ini))
  expect_true(
    "no_prefix" %in% names(read_ini))
})

test_that("Cluster already exist?",{
  createClusterST(
    area = area_test_clust, 
    cluster_name = "exist")
  
  expect_error(
    createClusterST(
      area = area_test_clust, 
      cluster_name = "exist"), 
    regexp = "al_exist already exist"
  )
  
  test_that("no case sensitive",{
    expect_error(
      createClusterST(
        area = area_test_clust, 
        cluster_name = "ExiST"), 
      regexp = "al_exist already exist"
    )
  })
})

test_that("Overwrite working ?",{
createClusterST(
    area = area_test_clust, 
    cluster_name = "overwrite")
  
  expect_no_error(
    createClusterST(
      area = area_test_clust, 
      cluster_name = "overwrite", 
      overwrite = TRUE)
  )
  
  test_that("no case sensitive",{
    expect_no_error(
      createClusterST(
        area = area_test_clust, 
        cluster_name = "OverWRITE", 
        overwrite = TRUE)
    )
  })
})

## New properties ----
test_that("Default values",{
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
  expect_equal(
    target_prop[setdiff(names(target_prop), 
                        c("name", "group"))], 
    storage_values_default())
})

test_that("Add right values",{
  # only 2 params among all list
  
  # two way to write 
  # 1- with all list
  
  # add new parameters 
  all_params <- storage_values_default()
  all_params[["efficiency"]] <- 0.9
  all_params[["reservoircapacity"]] <- 1000
  
  # default with new parameters 
  createClusterST(area = area_test_clust, 
                  cluster_name = "two_new_properties", 
                  storage_parameters = all_params)
  
  # read prop
  path_st_ini <- file.path("input", 
                           "st-storage", 
                           "clusters", 
                           area_test_clust,
                           "list")
  
  read_ini <- antaresRead::readIni(path_st_ini)
  target_prop <- read_ini[[paste(area_test_clust, 
                                 "two_new_properties",
                                 sep = "_")]]
  
  # test params created if identical with .ini read 
  expect_equal(
    target_prop[setdiff(names(target_prop), 
                        c("name", "group"))], 
    all_params)
  
  # 2- with list from sractch
  my_list <- list(
    efficiency = 0.9,
    reservoircapacity = 1000
  )
  
  # default with new parameters 
  createClusterST(area = area_test_clust, 
                  cluster_name = "two_new_properties_bis", 
                  storage_parameters = my_list)
  
  # read prop
  path_st_ini <- file.path("input", 
                           "st-storage", 
                           "clusters", 
                           area_test_clust,
                           "list")
  
  read_ini <- antaresRead::readIni(path_st_ini)
  target_prop <- read_ini[[paste(area_test_clust, 
                                 "two_new_properties_bis",
                                 sep = "_")]]
  
  # test params created if identical with .ini read 
  expect_equal(
    target_prop[setdiff(names(target_prop), 
                        c("name", "group"))], 
    my_list)
})

test_that("Overwrite properties",{
  # default 
  createClusterST(area = area_test_clust, 
                  cluster_name = "overwrite_prop")
  
  # overwrite prop
  all_params <- storage_values_default()
  all_params[["efficiency"]] <- 0.9
  all_params[["reservoircapacity"]] <- 1000
  all_params[["initiallevel"]] <- 0.5
  all_params[["withdrawalnominalcapacity"]] <- 250
  all_params[["injectionnominalcapacity"]] <- 200
  all_params[["initialleveloptim"]] <- TRUE
  
  createClusterST(area = area_test_clust, 
                  cluster_name = "overwrite_prop", 
                  storage_parameters = all_params, 
                  overwrite = TRUE)
  
  # read prop
  path_st_ini <- file.path("input", 
                           "st-storage", 
                           "clusters", 
                           area_test_clust,
                           "list")
  
  read_ini <- antaresRead::readIni(path_st_ini)
  target_prop <- read_ini[[paste(area_test_clust, 
                                 "overwrite_prop",
                                 sep = "_")]]
  
  # test params created if identical with .ini read 
  expect_equal(
    target_prop[setdiff(names(target_prop), 
                        c("name", "group"))], 
    all_params)
})


## New TS values ----
# global var test
default_ts_values <- antaresEditObject:::.default_values_st_TS(opts = simOptions())

original_files_names <- sapply(default_ts_values, 
                               function(x)x$string, 
                               USE.NAMES = FALSE)

test_that("Default TS dim and values",{
  # default with new parameters 
  createClusterST(area = area_test_clust, 
                  cluster_name = "default_ts")
  
  # read series 
  opts_ <- simOptions()
  path_ts <- file.path(opts_$inputPath, 
                       "st-storage",
                       "series",
                       area_test_clust,
                       "al_default_ts",
                       paste0(original_files_names, 
                              ".txt"))
  
  files_series <- lapply(path_ts, 
                         data.table::fread) 
  
  # test dim all equal
  dim_files_series <- sapply(files_series, 
                             dim) 
  expect_equal(mean(dim_files_series[1,]), 8760)
  expect_equal(mean(dim_files_series[2,]), 1)
  
  # test all value equal to default values 
  values_files_series <- sapply(files_series, 
                                sum)
  sum_val <- (1*8760)+(1*8760)+0+0+(1*8760)
  expect_equal(sum(values_files_series), sum_val)
})

test_that("Add right TS values",{
  good_ts <- matrix(0.7, 8760)
  
  # default with new optional TS
  createClusterST(area = area_test_clust, 
                  cluster_name = "good_ts_value", 
                  PMAX_injection = good_ts, 
                  PMAX_withdrawal = good_ts, 
                  inflows = good_ts, 
                  lower_rule_curve = good_ts, 
                  upper_rule_curve = good_ts)
  
  # read series 
  opts_ <- simOptions()
  path_ts <- file.path(opts_$inputPath, 
                       "st-storage",
                       "series",
                       area_test_clust,
                       "al_good_ts_value",
                       paste0(original_files_names, 
                              ".txt"))
  
  files_series <- lapply(path_ts, 
                         data.table::fread) 
  
  # test all value not equal to 0 (default)
  values_files_series <- sapply(files_series, 
                                sum)
  sum_val <- (0.7*8760)+(0.7*8760)+(0.7*8760)+(0.7*8760)+(0.7*8760)
  expect_equal(sum(values_files_series),sum_val)
})


#Delete study
deleteStudy()



# >=880 ----
test_that("Default values",{
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
  
  # read prop
  path_st_ini <- file.path("input", 
                           "st-storage", 
                           "clusters", 
                           area_test_clust,
                           "list")
  
  read_ini <- antaresRead::readIni(path_st_ini)
  target_prop <- read_ini[[paste(area_test_clust, 
                                 "default",
                                 sep = "_")]]
  
  # test default values
  expect_equal(
    target_prop[setdiff(names(target_prop), 
                        c("name", "group"))], 
    storage_values_default())
  
  deleteStudy()
})


# >=9.2 ---- 
suppressWarnings(
  createStudy(path = tempdir(), 
              study_name = "st-storage9.2", 
              antares_version = "9.2"))

# default area with st cluster
area_test_clust = "al" 
createArea(name = area_test_clust)


# TEST dynamic groups
test_that("Allow dynamic `group`",{
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
  expect_equal(st_file[[names(st_file)]][["group"]], 
                         "toto")
})
  
  
## New properties ----
test_that("Default values",{
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
  expect_equal(
    target_prop[setdiff(names(target_prop), 
                        c("name", "group"))], 
    storage_values_default())
})
  
test_that("Wrong type/values",{
  # add new parameters 
  all_params <- storage_values_default()
  
  # "efficiencywithdrawal"
  # type
  all_params[["efficiencywithdrawal"]] <- TRUE
  
  expect_error(
    createClusterST(area = area_test_clust, 
                    cluster_name = "err", 
                    storage_parameters = all_params), 
    regexp = "x does not inherit from class numeric"
  )
  
  # value
  all_params[["efficiencywithdrawal"]] <- 2.89
  
  expect_error(
    createClusterST(area = area_test_clust, 
                    cluster_name = "err", 
                    storage_parameters = all_params), 
    regexp = "efficiencywithdrawal must be in range 0-1"
  )
 
  
  # "penalize-variation-injection"
  all_params <- storage_values_default()
  
  # type
  all_params[["penalize-variation-injection"]] <- 0.9
  
  expect_error(
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
  
  expect_error(
    createClusterST(area = area_test_clust, 
                    cluster_name = "err", 
                    storage_parameters = all_params), 
    regexp = "does not inherit from class logical"
  )
  
  # NO TEST value (only TRUE/FALSE)
})
  
test_that("Add right values",{
  # add new parameters 
  all_params <- storage_values_default()
  all_params[["efficiency"]] <- 0.8
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
  
  # test params created if identical with .ini read 
  expect_equal(
    target_prop[setdiff(names(target_prop), 
                        c("name", "group"))], 
    all_params)
})
    

## New TS Values ----
# global var 
list_value_920 <- c("cost-injection", 
                    "cost-withdrawal", 
                    "cost-level", 
                    "cost-variation-injection", 
                    "cost-variation-withdrawal")
  
test_that("Default TS dim and values",{
  # default with new parameters 
  createClusterST(area = area_test_clust, 
                  cluster_name = "default_ts")
  
  # read series 
  opts_ <- simOptions()
  path_ts <- file.path(opts_$inputPath, 
                       "st-storage",
                       "series",
                       area_test_clust,
                       "al_default_ts",
                       paste0(list_value_920, 
                              ".txt"))
  
  files_series <- lapply(path_ts, 
                         data.table::fread) 
  
  # test dim all equal
  dim_files_series <- sapply(files_series, 
                             dim) 
  expect_equal(mean(dim_files_series[1,]), 8760)
  expect_equal(mean(dim_files_series[2,]), 1)
  
  # test all value equal to 0 
  values_files_series <- sapply(files_series, 
                                sum)
  expect_equal(sum(values_files_series), 0)
})
    
test_that("Wrong dim TS",{
  # like 8.6, these TS are dim [8760;1]
  bad_ts <- matrix(3, 8760*2, ncol = 2)
  
  # default with bad TS (just test 2 param)
  expect_error(
    createClusterST(area = area_test_clust, 
                    cluster_name = "wrong_ts_dim", 
                    cost_injection = bad_ts), 
    regexp = "Input data for cost_injection must be 8760\\*1"
  )
  expect_error(
    createClusterST(area = area_test_clust, 
                    cluster_name = "wrong_ts_dim", 
                    cost_withdrawal = bad_ts), 
    regexp = "Input data for cost_withdrawal must be 8760\\*1"
  )
})
    
test_that("Add right TS values",{
  good_ts <- matrix(0.7, 8760)
  
  # default with new optional TS
  createClusterST(area = area_test_clust, 
                  cluster_name = "good_ts_value", 
                  cost_injection = good_ts, 
                  cost_withdrawal = good_ts, 
                  cost_level = good_ts, 
                  cost_variation_injection = good_ts,
                  cost_variation_withdrawal = good_ts)
  
  # read series 
  opts_ <- simOptions()
  path_ts <- file.path(opts_$inputPath, 
                       "st-storage",
                       "series",
                       area_test_clust,
                       "al_good_ts_value",
                       paste0(list_value_920, 
                              ".txt"))
  
  files_series <- lapply(path_ts, 
                         data.table::fread) 

  # test all value not equal to 0 (default)
  values_files_series <- sapply(files_series, 
                                sum)
  expect_true(sum(values_files_series)>0)
})

## Optional constraints ----
test_that("Add new binding constraint properties", {
  # given
  
  name_no_prefix <- "add_constraints"

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
  
  # when
  createClusterST(area = area_test_clust, 
                  cluster_name = name_no_prefix, 
                  constraints_properties = constraints_properties)
  
  # then
  # read prop
  path_st_ini <- file.path("input", 
                           "st-storage", 
                           "constraints", 
                           area_test_clust,
                           paste0(area_test_clust, "_",name_no_prefix),
                           "additional-constraints")
  
  read_ini <- antaresRead::readIni(path_st_ini)
  target_names <- names(read_ini)
  
  # test params created if identical with .ini read 
  expect_true(all(
    names(constraints_properties)%in%target_names))
  
  
  test_that("Ovewrite not permiss", {
    expect_error(
      createClusterST(area = area_test_clust, 
                      cluster_name = name_no_prefix, 
                      constraints_properties = constraints_properties)
    )
  })
  
  test_that("Ovewrite permiss", {
    expect_no_error(
      createClusterST(area = area_test_clust, 
                      cluster_name = name_no_prefix, 
                      constraints_properties = constraints_properties, 
                      overwrite = TRUE)
    )
  })
  
})


test_that("Add new TS constraint", {
  # /!\ you can add ts only with properties
  
  # given
  name_no_prefix <- "add_ts"
  
  constraints_properties <- list(
    "withdrawal-2"=list(
      variable = "withdrawal",
      operator = "equal",
      hours = c("[1,3,5]", 
                "[120,121,122,123,124,125,126,127,128]")
    ),
    "netting-2"=list(
      variable = "netting",
      operator = "less",
      hours = c("[1, 168]")
    ))
  
  good_ts <- matrix(0.7, 8760)
  constraints_ts <- list(
    "withdrawal-2"=good_ts,
    "netting-2"=good_ts)
  
  # when
  createClusterST(area = area_test_clust, 
                  cluster_name = name_no_prefix, 
                  constraints_properties = constraints_properties, 
                  constraints_ts = constraints_ts)
  
  # then
  # read prop
  path_st_ini <- file.path("input", 
                           "st-storage", 
                           "constraints", 
                           area_test_clust,
                           paste0(area_test_clust, "_",name_no_prefix),
                           "additional-constraints")
  
  read_ini <- antaresRead::readIni(path_st_ini)
  target_names <- names(read_ini)
  
  # test params created if identical with .ini read 
  expect_true(all(
    names(constraints_properties)%in%target_names))
  
  # read ts
  opts_ <- simOptions()
  ts_path <- file.path(opts_$inputPath, 
                       "st-storage", 
                       "constraints", 
                       area_test_clust,
                       paste0(area_test_clust, "_",name_no_prefix),
                       paste0("rhs_", names(constraints_ts), ".txt"))
  
  # exist ?
  expect_true(all(
    file.exists(ts_path)
  ))
  
  # dim ? 
  dim <- lapply(ts_path, function(x){
    file_ts <- fread(input = x)
    dim(file_ts)
  })
  
  expect_equal(dim[[1]], dim[[2]])
  expect_equal(dim[[1]][1], 8760)
  expect_equal(dim[[1]][2], 1)
  
  
  test_that("Ovewrite TS not permiss", {
    expect_error(
      createClusterST(area = area_test_clust, 
                      cluster_name = name_no_prefix, 
                      constraints_properties = constraints_properties, 
                      constraints_ts = constraints_ts)
    )
  })
  
  test_that("Ovewrite permiss", {
    expect_no_error(
      createClusterST(area = area_test_clust, 
                      cluster_name = name_no_prefix, 
                      constraints_properties = constraints_properties, 
                      overwrite = TRUE)
    )
  })
})

deleteStudy()


# >=9.3 ---- 
suppressWarnings(
  createStudy(path = tempdir(), 
              study_name = "st-storage9.3", 
              antares_version = "9.3"))

# default area with st cluster
area_test_clust = "al" 
createArea(name = area_test_clust)

# add new parameters 
all_params <- storage_values_default()

# "allow-overflow"
test_that("Wrong type/values",{
  # value
  all_params[["allow-overflow"]] <- 1
  
  expect_error(
    createClusterST(area = area_test_clust, 
                    cluster_name = "err", 
                    storage_parameters = all_params), 
    regexp = "x must be of type 'logical'"
  )
})

test_that("Add right values",{
  # add new parameters 
  all_params <- storage_values_default()
  all_params[["allow-overflow"]] <- TRUE
  
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
  
  # test params created if identical with .ini read 
  expect_equal(
    target_prop[setdiff(names(target_prop), 
                        c("name", "group"))], 
    all_params)
})

## New TS dimension ----
test_that("Wrong dim TS",{
  # like 8.6, these TS are dim [8760;1]
  bad_ts <- matrix(3, 8760, ncol = 4)
  
  # create default
  createClusterST(area = area_test_clust, 
                  cluster_name = "edit_wrong_ts")
  
  # default with bad TS (just test 2 param)
  # expect_error(
  #   editClusterST(area = area_test_clust, 
  #                 cluster_name = "edit_wrong_ts", 
  #                 cost_injection = bad_ts), 
  #   regexp = "Input data for cost_injection must be 8760\\*1"
  # )
  # expect_error(
  #   editClusterST(area = area_test_clust, 
  #                 cluster_name = "edit_wrong_ts", 
  #                 cost_withdrawal = bad_ts), 
  #   regexp = "Input data for cost_withdrawal must be 8760\\*1"
  # )
})

deleteStudy()
