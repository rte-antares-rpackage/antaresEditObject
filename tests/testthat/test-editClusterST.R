
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

test_that("Check TS class",{
  bad_class <- list(matrix(3, nrow = 8760, ncol = 1))
  expect_error(
    editClusterST(
      area = area_test_clust, 
      cluster_name = "wrong_ts_class", 
      cost_injection = bad_class), 
    regexp = "The object must be of class matrix, data\\.frame, or data\\.table"
  )
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


## Optional constraints properties ----

test_that("Edit NULL dont update file", {
  # given
  name_no_prefix <- "add_nothing"
  
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
  
  createClusterST(area = area_test_clust, 
                  cluster_name = name_no_prefix, 
                  constraints_properties = constraints_properties)
  
  # when
  editClusterST(area = area_test_clust, 
                cluster_name = name_no_prefix)
  
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
  expect_equal(names(constraints_properties), names(read_ini))
})


test_that("Edit with bad element in list", {
  # given
  name_no_prefix <- "bad_list"
  
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
  
  createClusterST(area = area_test_clust, 
                  cluster_name = name_no_prefix, 
                  constraints_properties = constraints_properties)
  
  # when
  edit_constraints_properties <- list(
    "withdrawal-1"=list(
      variable = "withdrawal",
      operator = "equal",
      hours = c("[1,3,5]", 
                "[120,121,122,123,124,125,126,127,128]")
    ),
    "netting-1_bis"=list(
      variable = "netting",
      operator = "less",
      hours = c("[1, 168]")
    ))
  
  # then
  expect_error(
    editClusterST(area = area_test_clust, 
                  cluster_name = name_no_prefix, 
                  constraints_properties = edit_constraints_properties)
    , regexp = "'netting-1_bis' doesn't exist, it can't be edited."
  )
})


test_that("Edit constraint properties", {
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
  
  createClusterST(area = area_test_clust, 
                  cluster_name = name_no_prefix, 
                  constraints_properties = constraints_properties)
  
  # when
  edit_constraints_properties <- list(
    "netting-1"=list(
      variable = "variation-injection",
      operator = "less",
      hours = c("[1, 168]",
                "[240, 241]")
    ),
    "withdrawal-1"=list(
      variable = "injection",
      operator = "equal",
      hours = c("[1,3,5]", 
                "[120,121,122]")
    ))
  
  editClusterST(area = area_test_clust, 
                cluster_name = name_no_prefix, 
                constraints_properties = edit_constraints_properties)
  
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
})

test_that("Edit one or more of list of constraints", {
  # given
  name_no_prefix <- "one_of_list"
  
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
    ),
    "netting-2"=list(
      variable = "injection",
      operator = "less",
      hours = c("[1, 168, 170]")
    ),
    "withdrawal-2"=list(
      variable = "withdrawal",
      operator = "less",
      hours = c("[1,3]", 
                "[120,121,122,123]")
    ))
  
  createClusterST(area = area_test_clust, 
                  cluster_name = name_no_prefix, 
                  constraints_properties = constraints_properties)
  
  # when
  edit_constraints_properties <- list(
    "netting-1"=list(
      variable = "injection",
      operator = "less",
      hours = c("[1, 168]")
    ),
    "withdrawal-2"=list(
      variable = "netting",
      operator = "less",
      hours = c("[1,3]", 
                "[120]")
    )
  )
  
  editClusterST(area = area_test_clust, 
                cluster_name = name_no_prefix, 
                constraints_properties = edit_constraints_properties)
  
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
  
  expect_equal(read_ini$`netting-1`$variable, "injection")
  expect_equal(read_ini$`withdrawal-2`$variable, "netting")
})



## Optional constraints TS ----

test_that("Edit NULL dont create TS file", {
  # given
  name_no_prefix <- "edit_no_ts_default"
  
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
  
  createClusterST(area = area_test_clust, 
                  cluster_name = name_no_prefix, 
                  constraints_properties = constraints_properties)
  
  # when
  editClusterST(area = area_test_clust, 
                cluster_name = name_no_prefix)
  
  # then
  # check txt files dont exist
  opts_obj <- simOptions()
  ts_names <- paste0("rhs_", 
                     names(constraints_properties), 
                     ".txt")
  path_ts_files <- file.path(opts_obj$inputPath, 
                           "st-storage", 
                           "constraints", 
                           area_test_clust,
                           paste0(area_test_clust, "_",name_no_prefix),
                           ts_names)
  
  expect_false(all(
    file.exists(ts_names)
  ))
})

test_that("Edit NULL dont update existing TS file", {
  # given
  name_no_prefix <- "add_full_constraint"
  
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
  
  good_ts <- matrix(0.7, 8760)
  constraints_ts <- list(
    "withdrawal-1"=good_ts,
    "netting-1"=good_ts)
  
  createClusterST(area = area_test_clust, 
                  cluster_name = name_no_prefix, 
                  constraints_properties = constraints_properties, 
                  constraints_ts = constraints_ts)
  
  # when
  editClusterST(area = area_test_clust, 
                cluster_name = name_no_prefix)
  
  # then
  # check txt are not updated
  opts_obj <- simOptions()
  ts_names <- paste0("rhs_", 
                     names(constraints_properties), 
                     ".txt")
  path_ts_files <- file.path(opts_obj$inputPath, 
                             "st-storage", 
                             "constraints", 
                             area_test_clust,
                             paste0(area_test_clust, "_",name_no_prefix),
                             ts_names)
  
  ts_read <- lapply(path_ts_files, function(x){
    as.matrix(data.table::fread(x))
  })
  names(ts_read) <- names(constraints_ts)
  
  expect_equal(ts_read, constraints_ts, check.attributes = FALSE)
})

test_that("Edit TS which do not exist", {
  # given
  name_no_prefix <- "edit_create_ts"
  
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
  
  createClusterST(area = area_test_clust, 
                  cluster_name = name_no_prefix, 
                  constraints_properties = constraints_properties)
  
  # when
  good_ts <- data.table::as.data.table(matrix(0.7, 8760))
  constraints_ts <- list(
    "withdrawal-1"=good_ts,
    "netting-1"=good_ts)
  
  editClusterST(area = area_test_clust, 
                cluster_name = name_no_prefix, 
                constraints_ts = constraints_ts)
  
  # then
  opts_obj <- simOptions()
  ts_names <- paste0("rhs_", 
                     names(constraints_properties), 
                     ".txt")
  path_ts_files <- file.path(opts_obj$inputPath, 
                             "st-storage", 
                             "constraints", 
                             area_test_clust,
                             paste0(area_test_clust, "_",name_no_prefix),
                             ts_names)
  
  ts_read <- lapply(path_ts_files, data.table::fread)
  names(ts_read) <- names(constraints_ts)
  
  expect_equal(ts_read, constraints_ts, check.attributes = FALSE)
})

test_that("Edit existing TS", {
  # given
  name_no_prefix <- "edit_existing_ts"
  
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
  good_ts <- data.table::as.data.table(matrix(0.7, 8760))
  constraints_ts <- list(
    "withdrawal-1"=good_ts,
    "netting-1"=good_ts)
  
  editClusterST(area = area_test_clust, 
                cluster_name = name_no_prefix, 
                constraints_ts = constraints_ts)
  
  # then
  opts_obj <- simOptions()
  ts_names <- paste0("rhs_", 
                     names(constraints_properties), 
                     ".txt")
  path_ts_files <- file.path(opts_obj$inputPath, 
                             "st-storage", 
                             "constraints", 
                             area_test_clust,
                             paste0(area_test_clust, "_",name_no_prefix),
                             ts_names)
  
  ts_read <- lapply(path_ts_files, data.table::fread)
  names(ts_read) <- names(constraints_ts)
  
  expect_equal(ts_read, constraints_ts, check.attributes = FALSE)
})

test_that("Edit properties + TS", {
  # given
  name_no_prefix <- "edit_prop_ts"
  
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
  edit_constraints_properties <- list(
    "withdrawal-1"=list(
      variable = "withdrawal_edit",
      operator = "equal_edit",
      hours = c("[1,3,5]", 
                "[120,121,122,123,124,125,126,127,128, 000]")
    ),
    "netting-1"=list(
      variable = "netting_edit",
      operator = "less_edit",
      hours = c("[1, 168, 000]")
    ))
  
  good_ts <- data.table::as.data.table(matrix(0.2, 8760))
  constraints_ts <- list(
    "withdrawal-1"=good_ts,
    "netting-1"=good_ts)
  
  editClusterST(area = area_test_clust, 
                cluster_name = name_no_prefix, 
                constraints_properties = edit_constraints_properties,
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
    # 'hours' have a special format with ',' in braquet + as separator /!\
  expect_true(all(
    names(constraints_properties)%in%target_names))
  
  expect_equal(read_ini$`withdrawal-1`$variable, 
               edit_constraints_properties$`withdrawal-1`$variable)
  expect_equal(read_ini$`withdrawal-1`$operator, 
               edit_constraints_properties$`withdrawal-1`$operator)
  
  
  expect_equal(read_ini$`netting-1`$variable, 
               edit_constraints_properties$`netting-1`$variable)
  expect_equal(read_ini$`netting-1`$operator, 
               edit_constraints_properties$`netting-1`$operator)
  
  # TS
  opts_obj <- simOptions()
  ts_names <- paste0("rhs_", 
                     names(constraints_properties), 
                     ".txt")
  path_ts_files <- file.path(opts_obj$inputPath, 
                             "st-storage", 
                             "constraints", 
                             area_test_clust,
                             paste0(area_test_clust, "_",name_no_prefix),
                             ts_names)
  
  ts_read <- lapply(path_ts_files, data.table::fread)
  names(ts_read) <- names(constraints_ts)
  
  expect_equal(ts_read, constraints_ts, check.attributes = FALSE)
})


#Delete study
deleteStudy()

# >=9.3 ---- 
suppressWarnings(
  createStudy(path = tempdir(), 
              study_name = "st-storage9.3", 
              antares_version = "9.3"))

# default area with st cluster
area_test_clust = "al" 
createArea(name = area_test_clust)

## Edit new properties ----
# wrong type/values not eccepted
test_that("Wrong type/values",{
  
  # "allow-overflow" with bad type
  all_params <- storage_values_default()
  
  # type
  edit_params <- list("allow-overflow" = "area")
  
  expect_error(
    editClusterST(area = area_test_clust, 
                  cluster_name = "edit_wrong_prop", 
                  storage_parameters = edit_params), 
    regexp = "does not inherit from class logical"
  )
  
  # NO TEST value (only TRUE/FALSE)
})

test_that("Edit right values",{
  # add new parameters 
  all_params <- list(
    `allow-overflow` = TRUE
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

## New TS dimension ----
test_that("Wrong dim TS",{
  # like 8.6, these TS are dim [8760;N]
  bad_ts <- matrix(3, 8760*2, ncol = 2)
  
  # create default
  createClusterST(area = area_test_clust, 
                  cluster_name = "edit_wrong_ts")
  
  #default with bad TS (just test 2 param)
  expect_error(
    editClusterST(area = area_test_clust,
                  cluster_name = "edit_wrong_ts",
                  cost_injection = bad_ts),
    regexp = "Input data for cost_injection must be 8760\\*N \\(N>=1\\)"
  )
  expect_error(
    editClusterST(area = area_test_clust,
                  cluster_name = "edit_wrong_ts",
                  cost_withdrawal = bad_ts),
    regexp = "Input data for cost_withdrawal must be 8760\\*N \\(N>=1\\)"
  )
})
test_that("Add right TS dim",{
  good_ts <- matrix(0.7, 8760,3)
  
  # default with new optional TS
  createClusterST(area = area_test_clust, 
                  cluster_name = "edit_good_ts_value")
  
  # edit with good dimension (only new TS)
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
test_that("Edit TS which do not exist", {
  # given
  name_no_prefix <- "edit_create_ts"
  
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
  
  createClusterST(area = area_test_clust, 
                  cluster_name = name_no_prefix, 
                  constraints_properties = constraints_properties)
  
  # when
  good_ts <- data.table::as.data.table(matrix(0.7, 8760,2))
  constraints_ts <- list(
    "withdrawal-1"=good_ts,
    "netting-1"=good_ts)
  
  editClusterST(area = area_test_clust, 
                cluster_name = name_no_prefix, 
                constraints_ts = constraints_ts)
  
  # then
  opts_obj <- simOptions()
  ts_names <- paste0("rhs_", 
                     names(constraints_properties), 
                     ".txt")
  path_ts_files <- file.path(opts_obj$inputPath, 
                             "st-storage", 
                             "constraints", 
                             area_test_clust,
                             paste0(area_test_clust, "_",name_no_prefix),
                             ts_names)
  
  ts_read <- lapply(path_ts_files, data.table::fread)
  names(ts_read) <- names(constraints_ts)
  
  expect_equal(ts_read, constraints_ts, check.attributes = FALSE)
})

test_that("Edit existing TS", {
  # given
  name_no_prefix <- "edit_existing_ts"
  
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
  
  good_ts <- data.table::as.data.table(matrix(10, 8760,4))
  constraints_ts <- list(
    "withdrawal-1"=good_ts,
    "netting-1"=good_ts)
  
  createClusterST(area = area_test_clust, 
                  cluster_name = name_no_prefix, 
                  constraints_properties = constraints_properties, 
                  constraints_ts = constraints_ts)
  
  # when
  good_ts <- data.table::as.data.table(matrix(0.7, 8760,4))
  constraints_ts <- list(
    "withdrawal-1"=good_ts,
    "netting-1"=good_ts)
  
  editClusterST(area = area_test_clust, 
                cluster_name = name_no_prefix, 
                constraints_ts = constraints_ts)
  
  # then
  opts_obj <- simOptions()
  ts_names <- paste0("rhs_", 
                     names(constraints_properties), 
                     ".txt")
  path_ts_files <- file.path(opts_obj$inputPath, 
                             "st-storage", 
                             "constraints", 
                             area_test_clust,
                             paste0(area_test_clust, "_",name_no_prefix),
                             ts_names)
  
  ts_read <- lapply(path_ts_files, data.table::fread)
  names(ts_read) <- names(constraints_ts)
  
  expect_equal(ts_read, constraints_ts, check.attributes = FALSE)
})


#Delete study
deleteStudy()
