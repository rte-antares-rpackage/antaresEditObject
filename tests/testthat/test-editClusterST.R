
# >=v860 ----
suppressWarnings(
  createStudy(path = tempdir(), 
              study_name = "st-storage", 
              antares_version = "8.6.0"))

# just need at least one area
area_test_clust = "al" 
createArea(name = area_test_clust)

# at least need 1 st cluster
createClusterST(area = area_test_clust, 
                cluster_name = "cluster_init")


test_that("study opts parameters",{
  expect_error(
    editClusterST(
      area = area_test_clust, 
      cluster_name = "cluster_init",
      opts = list(studyName="test",
                  studyPath="C:/Users/beta/AppData/Local/Temp/RtmpQtOZyt/st-storage")), 
    regexp = "opts does not inherit from class simOptions"
  )
})

test_that("create cluster only for >=8.6",{
  bad_opts <- simOptions()
  bad_opts$antaresVersion <- 850
  
  expect_error(
    editClusterST(
      area = area_test_clust, 
      cluster_name = "err",
      opts = bad_opts), 
    regexp = "only available if using Antares >= 8.6.0"
  )
})

test_that("Check area",{
  expect_error(
    editClusterST(
      area = "area_test_clust", 
      cluster_name = "err"), 
    regexp = "'area_test_clust' is not a valid area name, possible names are: al"
  )
  test_that("no case sensitive",{
    expect_no_error(
      editClusterST(
        area = "AL", 
        group = "PSP_open",
        cluster_name = "cluster_init"))
  })
})

test_that("Check group",{
  expect_error(
    editClusterST(
      area = area_test_clust, 
      cluster_name = "err", 
      group = "myGroup"), 
    regexp = "Group: 'myGroup' is not a valid name recognized by Antares"
  )
})

test_that("Check input list 'storage_parameters'",{
  # respect list format
  expect_error(
    editClusterST(
      area = area_test_clust, 
      cluster_name = "err", 
      storage_parameters = c(efficiency=1)), 
    regexp = "storage_parameters does not inherit from class list"
  )
  
  # list with formatted names
  expect_error(
    editClusterST(
      area = area_test_clust, 
      cluster_name = "err", 
      storage_parameters = list(efficiencyy=1)), 
    regexp = "Parameter 'st-storage' must be named with the following elements: efficiency, reservoircapacity"
  )
  
  # check values parameters
  # check is ratio ? 
  expect_error(
    editClusterST(
      area = area_test_clust, 
      cluster_name = "err", 
      storage_parameters = list(efficiency = 2, 
                                reservoircapacity = 100)), 
    regexp = "efficiency must be in range 0-1"
  )
  
  # check positive capacity ? 
  expect_error(
    editClusterST(
      area = area_test_clust, 
      cluster_name = "err", 
      storage_parameters = list(efficiency = 0.9, 
                                reservoircapacity = -100)), 
    regexp = "reservoircapacity must be >= 0"
  )
  
  # check is logical ? 
  expect_error(
    editClusterST(
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
    editClusterST(
      area = area_test_clust, 
      cluster_name = "err", 
      PMAX_injection = bad_matrix_data_dim), 
    regexp = "Input data for PMAX_injection must be 8760\\*1"
  )
  
  # test raw dim
  bad_matrix_data_dim <- matrix(1, 8784)
  
  expect_error(
    editClusterST(
      area = area_test_clust, 
      cluster_name = "err", 
      PMAX_injection = bad_matrix_data_dim), 
    regexp = "Input data for PMAX_injection must be 8760\\*1"
  )
})

test_that("Check existing cluster ?",{
  expect_error(
    editClusterST(area = area_test_clust, 
                  cluster_name = "casper", 
                  group = "Other1"), 
    regexp = "'al_casper' doesn't exist")
  
  test_that("no case sensitive",{
    expect_no_error(
      editClusterST(
        area = area_test_clust, 
        cluster_name = "Cluster_INIt",
        group = "Other1"))
  })
})

test_that("Default call warning",{
  # function can be called without modification
  expect_warning(
    editClusterST(area = area_test_clust, 
                  cluster_name = "cluster_init"), 
    regexp = "No edition for 'list.ini' file")
})

## Edit new properties ----
test_that("Edit group",{
  # group list
  st_storage_group <- c("PSP_open", 
                        "PSP_closed", 
                        "Pondage", 
                        "Battery",
                        paste0("Other", 
                               seq(1,5)))
  
  editClusterST(area = area_test_clust, 
                cluster_name = "cluster_init", 
                group = "Battery")
  
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
  
  # test params created if identical with .ini read 
  expect_equal(target_prop[["group"]], 
                         "Battery")
})

test_that("Edit list of parameters 'storage_parameters'",{
  # edit all params
  all_params <- storage_values_default()
  all_params[["efficiency"]] <- 0.9
  all_params[["reservoircapacity"]] <- 1000
  all_params[["initiallevel"]] <- 0.5
  all_params[["withdrawalnominalcapacity"]] <- 250
  all_params[["injectionnominalcapacity"]] <- 200
  all_params[["initialleveloptim"]] <- TRUE
  
  editClusterST(area = area_test_clust, 
                cluster_name = "cluster_init", 
                group = "Battery", 
                storage_parameters = all_params)
  
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
  
  # test all properties from .ini
  expect_equal(
    target_prop[setdiff(names(target_prop), 
                        c("name", "group"))], 
    all_params)
})
  

test_that("Edit new TS values",{
  # there are no test on values 
  
  # global var test
  default_ts_values <- antaresEditObject:::.default_values_st_TS(opts = simOptions())
  
  original_files_names <- sapply(default_ts_values, 
                                 function(x)x$string, 
                                 USE.NAMES = FALSE)
  
  good_dim_ts <- matrix(0.7, 8760)
  
  # default with new optional TS
  editClusterST(area = area_test_clust, 
                cluster_name = "cluster_init", 
                PMAX_injection = good_dim_ts, 
                PMAX_withdrawal = good_dim_ts, 
                inflows = good_dim_ts, 
                lower_rule_curve = good_dim_ts, 
                upper_rule_curve = good_dim_ts)
  
  # read series 
  opts_ <- simOptions()
  path_ts <- file.path(opts_$inputPath, 
                       "st-storage",
                       "series",
                       area_test_clust,
                       "al_cluster_init",
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


# >=v880 ----
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

test_that("New parameter 'enabled'",{
  # edit 
  list_params <- storage_values_default()
  list_params$efficiency <- 0.5
  list_params$reservoircapacity <- 50
  list_params$enabled <- FALSE
  
  editClusterST(area = area_test_clust, 
                cluster_name = "default", 
                storage_parameters = list_params)
  
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
  
  # test all properties from .ini
  expect_equal(
    target_prop[setdiff(names(target_prop), 
                        c("name", "group"))], 
    list_params)
})

deleteStudy()


# >=9.2 ---- 
suppressWarnings(
  createStudy(path = tempdir(), 
              study_name = "st-storage9.2", 
              antares_version = "9.2"))

# default area with st cluster
area_test_clust = "al" 
createArea(name = area_test_clust)


# dynamic groups
test_that("Allow dynamic `group`",{
  # create 
  createClusterST(area = area_test_clust, 
                  cluster_name = "dynamic_grp", 
                  group = "toto")
  
  # edit
  editClusterST(area = area_test_clust, 
                cluster_name = "dynamic_grp", 
                group = "titi")
  
  # read properties
  opts_ <- simOptions()
  st_path <- file.path("input",
                       "st-storage", 
                       "clusters", 
                       area_test_clust, 
                       "list")
  
  st_file <- readIni(pathIni = st_path)
  
  # group has no restrictions
  expect_equal(st_file[[names(st_file)]][["group"]], "titi")
})

## Edit new properties ----
# wrong type/values not eccepted
test_that("Wrong type/values",{
  
  # "efficiencywithdrawal" with bad type
  # type
  edit_params <- list("efficiencywithdrawal" = TRUE)
  
  # create default
  createClusterST(area = area_test_clust, 
                  cluster_name = "edit_wrong_prop")
  
  expect_error(
    editClusterST(area = area_test_clust, 
                    cluster_name = "edit_wrong_prop", 
                    storage_parameters = edit_params), 
    regexp = "x does not inherit from class numeric"
  )
  
  # value
  edit_params <- list("efficiencywithdrawal" = 2.89)
  
  expect_error(
    editClusterST(area = area_test_clust, 
                    cluster_name = "edit_wrong_prop", 
                    storage_parameters = edit_params), 
    regexp = "efficiencywithdrawal must be in range 0-1"
  )
  
  # "penalize-variation-injection"
  # type
  edit_params <- list("penalize-variation-injection" = 0.9)
  
  expect_error(
    editClusterST(area = area_test_clust, 
                    cluster_name = "edit_wrong_prop", 
                    storage_parameters = edit_params), 
    regexp = "does not inherit from class logical"
  )
  
  # NO TEST value (only TRUE/FALSE)
  
  
  # "penalize-variation-withdrawal"
  all_params <- storage_values_default()
  
  # type
  edit_params <- list("penalize-variation-withdrawal" = "area")
  
  expect_error(
    editClusterST(area = area_test_clust, 
                    cluster_name = "edit_wrong_prop", 
                    storage_parameters = edit_params), 
    regexp = "does not inherit from class logical"
  )
  
  # NO TEST value (only TRUE/FALSE)
})

# edit new properties
test_that("Edit right values",{
  # add new parameters 
  all_params <- list(
    "efficiencywithdrawal" = 0.9,
    `penalize-variation-injection` = TRUE,
    `penalize-variation-withdrawal` = TRUE
  )
  
  # default with new parameters 
  createClusterST(area = area_test_clust, 
                  cluster_name = "new_properties")
  
  # Edit with new parameters 
  editClusterST(area = area_test_clust, 
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
  
  # test params created if equal with .ini read 
  expect_equal(
    target_prop[names(all_params)], 
    all_params)
})



## New TS Values ----
test_that("Wrong dim TS",{
  # like 8.6, these TS are dim [8760;1]
  bad_ts <- matrix(3, 8760*2, ncol = 2)
  
  # create default
  createClusterST(area = area_test_clust, 
                  cluster_name = "edit_wrong_ts")
  
  # default with bad TS (just test 2 param)
  expect_error(
    editClusterST(area = area_test_clust, 
                    cluster_name = "edit_wrong_ts", 
                    cost_injection = bad_ts), 
    regexp = "Input data for cost_injection must be 8760\\*1"
  )
  expect_error(
    editClusterST(area = area_test_clust, 
                    cluster_name = "edit_wrong_ts", 
                    cost_withdrawal = bad_ts), 
    regexp = "Input data for cost_withdrawal must be 8760\\*1"
  )
})

test_that("Add right TS values",{
  good_ts <- matrix(0.7, 8760)
  
  # default with new optional TS
  createClusterST(area = area_test_clust, 
                  cluster_name = "edit_good_ts_value")
  
  # edit with good values (only new TS)
  editClusterST(area = area_test_clust, 
                cluster_name = "edit_good_ts_value", 
                cost_injection = good_ts, 
                cost_withdrawal = good_ts, 
                cost_level = good_ts, 
                cost_variation_injection = good_ts,
                cost_variation_withdrawal = good_ts)
  
  # read series 
  names_files <- c("cost-injection",
                   "cost-withdrawal",
                   "cost-level",
                   "cost-variation-injection",
                   "cost-variation-withdrawal")
  opts_ <- simOptions()
  path_ts <- file.path(opts_$inputPath, 
                       "st-storage",
                       "series",
                       area_test_clust,
                       "al_edit_good_ts_value",
                       paste0(names_files, 
                              ".txt"))
  
  files_series <- lapply(path_ts, 
                         data.table::fread) 
  
  # test all value not equal to 0 (default)
  values_files_series <- sapply(files_series, 
                                sum)
  expect_true(sum(values_files_series)>0)
})

deleteStudy()

  



