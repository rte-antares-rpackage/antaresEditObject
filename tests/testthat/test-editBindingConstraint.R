# v710 ----

test_that("editBindingConstraint v710", {
  
  setup_study(studies, sourcedir)
  opts <- antaresRead::setSimulationPath(studyPath, "input")
  
  # reading bc
  existing_bc <- antaresRead::readBindingConstraints()
  bc_names <- names(antaresRead::readBindingConstraints())
  
  details_bc <- existing_bc[[bc_names[1]]]
  details_bc$coefs
  
  # edit BC
  data_hourly <- matrix(data = rep(15, 8760 * 3), ncol = 3)
  data_daily <- matrix(data = rep(15, 365 * 3), ncol = 3)
  
  editBindingConstraint(name = bc_names[1], 
                        values = data_hourly, 
                        timeStep = "hourly",
                        operator = "less", 
                        coefficients = list("b%psp in"= 1.75,
                                         "b%psp out"= 2,
                                         "a%a_offshore"= "2%-5"),
                        opts = simOptions())
  
  bc_modified <- antaresRead::readBindingConstraints()
  new_coef <- bc_modified[[bc_names[1]]]$coefs
  
  # test
  testthat::expect_true(all(new_coef %in% c(1.75, 2, "2%-5")))
  
  # remove temporary study
  unlink(x = file.path(pathstd, "test_case"), recursive = TRUE)
  
})

# v870 Scenarized RHS ----

# read script to generate study v8.7.0
sourcedir_last_study <- system.file("study_test_generator/generate_test_study_870.R", 
                                    package = "antaresEditObject")

# create study
source(file = sourcedir_last_study)
opts_test <- simOptions()

## global data ----
# scenarized data 
  # hourly
n <- 10
lt_data <- matrix(data = rep(1, 8760 * n), ncol = n)
gt_data <- matrix(data = rep(2, 8760 * n), ncol = n)
eq_data <- matrix(data = rep(3, 8760 * n), ncol = n)

scenar_values_hourly <- list(lt= lt_data,
                      gt= gt_data, 
                      eq= eq_data)
  # daily
lt_data <- matrix(data = rep(1, 365 * n), ncol = n)
gt_data <- matrix(data = rep(2, 365 * n), ncol = n)
eq_data <- matrix(data = rep(3, 365 * n), ncol = n)

scenar_values_daily <- list(lt= lt_data,
                             gt= gt_data, 
                             eq= eq_data)

## default group ----
test_that("editBindingConstraint paramater one by one", {
  
  name_bc <- "bc_minimal"
  createBindingConstraint(name = name_bc)
  
  # read 
  bc_read <- readBindingConstraints()
  
  # edit nothing
  editBindingConstraint(name = name_bc)
  
  # read updated
  bc_read_updated <- readBindingConstraints()
  
  # test if is identical 
  testthat::expect_equal(bc_read, bc_read_updated)
  
  # edit  ["values"] (created by default with "both")
    # "operator" muy be filled 
    testthat::expect_error(
      editBindingConstraint(name = name_bc, 
                            values = scenar_values_hourly[c("lt", "gt")]), 
      regexp = "To modify the 'values' you must enter the 'operator'"
    )
  
  editBindingConstraint(name = name_bc, 
                        operator = "both",
                        values = scenar_values_hourly[c("lt", "gt")])
  
  # read updated
  bc_read_updated <- readBindingConstraints()
  
  # test dim
  dim_bc <- dim(bc_read_updated$bc_minimal$values$less)
  testthat::expect_equal(dim_bc[2], 10)
  
  # edit  ["enabled "] (TRUE by default)
  editBindingConstraint(name = name_bc, 
                        enabled = FALSE)
  
  # read updated
  bc_read_updated <- readBindingConstraints()
  
  testthat::expect_equal(bc_read_updated$bc_minimal$properties$enabled, FALSE)
  
 
  
})

## default group ----
test_that("editBindingConstraint with 'default' group v8.7.0", {
  # INIT with creation BC 
    # multi properties
  data_terms <- list("at%fr" = "1%10",
                     "at%fr" = "1%11",
                     "fr%it" = "1%-5",
                     "at.at_gas" = "1%10")
  
  name_bc <- "bc_multi_offset"
  
  createBindingConstraint(
    name = name_bc,
    values = scenar_values_hourly,
    enabled = TRUE,
    timeStep = "hourly",
    operator = "greater",
    overwrite = TRUE,
    coefficients = data_terms)

  # PS : in this study, "default" have 1 column dimension
  bc <- readBindingConstraints(opts = opts_test)
    
  ### greater to both ----
  # edit properties + values (good dimension)
    # edit "greater" to "both"
  bc_names_v870 <- bc[[name_bc]]$properties$id
  editBindingConstraint(name = bc_names_v870, 
                        values = scenar_values_daily, 
                        timeStep = "daily",
                        operator = "both", 
                        filter_year_by_year = "daily",
                        filter_synthesis = "daily",
                        coefficients = list("fr%it"= 7.45))
  
  # read
  bc_modified <- readBindingConstraints(opts = opts_test)
  new_coef <- bc_modified[[bc_names_v870]]$coefs
  timeStep <- bc_modified[[bc_names_v870]]$properties$timeStep
  operator <- bc_modified[[bc_names_v870]]$properties$operator
  filter_year <- bc_modified[[bc_names_v870]]$properties$`filter-year-by-year`
  filter_synthesis <- bc_modified[[bc_names_v870]]$properties$`filter-synthesis`
  
  # test properties
  testthat::expect_true(7.45 %in% new_coef)
  testthat::expect_true(timeStep %in% "daily")
  testthat::expect_true(operator %in% "both")
  testthat::expect_true(filter_year %in% "daily")
  testthat::expect_true(filter_synthesis %in% "daily")
  
  # test dim values
  dim_col_values_input <- dim(scenar_values_daily$lt)[2]
  dim_col_values_edited <- dim(bc_modified[[bc_names_v870]]$values$less)[2]
  testthat::expect_equal(dim_col_values_input, dim_col_values_edited)
  
  # test real values
  # for both
  operator_bc <- c("_lt", "_gt")
  path_bc <- file.path(opts_test$inputPath, "bindingconstraints")
  path_file_bc <- paste0(file.path(path_bc, bc_names_v870), 
                         operator_bc, ".txt")
  
  # read .txt (test values)
  res <- lapply(path_file_bc, 
                antaresRead:::fread_antares, 
                opts = opts_test)
  
  # txt files (test real value)
  # test just first values cause code convert 8760 to 8784 with 0
  testthat::expect_equal(head(res[[1]]), 
                         head(data.table::as.data.table(scenar_values_daily$lt)))
  testthat::expect_equal(head(res[[2]]), 
                         head(data.table::as.data.table(scenar_values_daily$gt)))
  
  
  
  ### greater to equal ----
    # edit properties + values (good dimension)
    # edit "both" to "equal"
  bc_names_v870 <- bc[[name_bc]]$properties$id
  editBindingConstraint(name = bc_names_v870, 
                        values = scenar_values_daily, 
                        timeStep = "daily",
                        operator = "equal", 
                        filter_year_by_year = "daily",
                        filter_synthesis = "daily",
                        coefficients = list("fr%it"= 7.45))
  
  # test real values
  # for equal
  operator_bc <- c("_eq")
  path_bc <- file.path(opts_test$inputPath, "bindingconstraints")
  path_file_bc <- paste0(file.path(path_bc, bc_names_v870), 
                         operator_bc, ".txt")
  
  # read .txt (test values)
  res <- lapply(path_file_bc, 
                antaresRead:::fread_antares, 
                opts = opts_test)
  
  # txt files (test real value)
  # test just first values cause code convert 8760 to 8784 with 0
  testthat::expect_equal(head(res[[1]]), 
                         head(data.table::as.data.table(scenar_values_daily$eq)))
  
  ### equal to less ----
    # edit properties + values (good dimension)
    # edit "equal" to "less"
  bc_names_v870 <- bc[[name_bc]]$properties$id
  editBindingConstraint(name = bc_names_v870, 
                        values = scenar_values_daily, 
                        timeStep = "daily",
                        operator = "less", 
                        filter_year_by_year = "daily",
                        filter_synthesis = "daily",
                        coefficients = list("fr%it"= 7.45))
  
  # test real values
  # for equal
  operator_bc <- c("_lt")
  path_bc <- file.path(opts_test$inputPath, "bindingconstraints")
  path_file_bc <- paste0(file.path(path_bc, bc_names_v870), 
                         operator_bc, ".txt")
  
  # read .txt (test values)
  res <- lapply(path_file_bc, 
                antaresRead:::fread_antares, 
                opts = opts_test)
  
  # txt files (test real value)
  # test just first values cause code convert 8760 to 8784 with 0
  testthat::expect_equal(head(res[[1]]), 
                         head(data.table::as.data.table(scenar_values_daily$lt)))

  
  
  
  
  # edit properties + values (bad dimension)
    ### error dimension ----  
  
  n <- 9
  # daily
  lt_data <- matrix(data = rep(1, 365 * n), ncol = n)
  gt_data <- matrix(data = rep(2, 365 * n), ncol = n)
  eq_data <- matrix(data = rep(3, 365 * n), ncol = n)
  
  scenar_values_daily_n <- list(lt= lt_data,
                              gt= gt_data, 
                              eq= eq_data)
  
  testthat::expect_error(
    editBindingConstraint(name = bc_names_v870, 
                          values = scenar_values_daily_n, 
                          timeStep = "daily",
                          operator = "both", 
                          coefficients = list("fr%it"= 7.45)), 
    regexp = "Put right columns dimension"
  )
  
  ### multi coeff ----
  editBindingConstraint(name = bc_names_v870, 
                        values = NULL, 
                        timeStep = "daily",
                        operator = "both", 
                        coefficients = list("fr%it" = 12, 
                                         "fr%at" = 0))
  
  # read
  bc_modified <- readBindingConstraints(opts = opts_test)
  new_coef <- bc_modified[[bc_names_v870]]$coefs
  
  # test coefs
  testthat::expect_true(all(
    c(12, 0) %in% new_coef))
})

## exisintg group ----
test_that("editBindingConstraint with existing group v8.7.0", {
  # INIT with creation BC 
    # multi properties
  data_terms <- list("at%fr" = "1%10",
                     "at%fr" = "1%11",
                     "fr%it" = "1%-5",
                     "at.at_gas" = "1%10")
  
  name_bc <- "bc_group_multi_offset"
  name_group <- "group_test"
  
  createBindingConstraint(
    name = name_bc,
    values = scenar_values_hourly,
    enabled = TRUE,
    timeStep = "hourly",
    operator = "both",
    group = name_group,
    overwrite = TRUE,
    coefficients = data_terms)
  
  createBindingConstraint(
    name = "bc_test_default_group",
    values = scenar_values_hourly,
    enabled = TRUE,
    timeStep = "hourly",
    operator = "both",
    overwrite = TRUE,
    coefficients = data_terms)
  
  # read existing binding
  bc <- readBindingConstraints(opts = opts_test)
 
  # list names group (none default)
  # dimension according BC/group
  info_bc <- lapply(bc, function(x){
    if(x$properties$operator %in% "both")
      list(group = x$properties$group,
           dim_values = dim(x$values$less)[2])
    else
      list(group = x$properties$group,
           dim_values = dim(x$values)[2])
  })
    
  index <- !lapply(info_bc, `[[`, 1) %in% 
    "default"
  
  # select on bc none "default"
  bc_no_default <- names(info_bc[index])[1]
  
  group_bc <- info_bc[index][[bc_no_default]]$group
  dim_bc <- info_bc[index][[bc_no_default]]$dim_values
  
  # edit bc with good dim
  n <- dim_bc
  # daily
  lt_data <- matrix(data = rep(1, 365 * n), ncol = n)
  gt_data <- matrix(data = rep(2, 365 * n), ncol = n)
  eq_data <- matrix(data = rep(3, 365 * n), ncol = n)
  
  scenar_values_daily_n <- list(lt= lt_data,
                                gt= gt_data, 
                                eq= eq_data)
  
  editBindingConstraint(name = bc_no_default, 
                        values = scenar_values_daily_n, 
                        group = group_bc,
                        timeStep = "daily",
                        operator = "both", 
                        coefficients = list("fr%it" = 12),
                        opts = opts_test)
  
  # read
  bc_modified <- readBindingConstraints(opts = opts_test)
  new_coef <- bc_modified[[bc_no_default]]$coefs
  timeStep <- bc_modified[[bc_no_default]]$properties$timeStep
  operator <- bc_modified[[bc_no_default]]$properties$operator
  
  # test properties
  testthat::expect_true(12 %in% new_coef)
  testthat::expect_true("daily" %in% timeStep)
  testthat::expect_true("both" %in% operator)
  
  # test values
  dim_col_values_input <- dim(scenar_values_daily_n$lt)[2]
  dim_col_values_edited <- dim(bc_modified[[bc_no_default]]$values$less)[2]
  testthat::expect_equal(dim_col_values_input, dim_col_values_edited)
  
  # edit properties + values (bad dimension)
  ### error dimension ----  
  
  n <- 9
  # daily
  lt_data <- matrix(data = rep(1, 365 * n), ncol = n)
  gt_data <- matrix(data = rep(2, 365 * n), ncol = n)
  eq_data <- matrix(data = rep(3, 365 * n), ncol = n)
  
  scenar_values_daily_n <- list(lt= lt_data,
                                gt= gt_data, 
                                eq= eq_data)
  
  testthat::expect_error(
    editBindingConstraint(name = bc_no_default, 
                          values = scenar_values_daily_n, 
                          timeStep = "daily",
                          operator = "both", 
                          coefficients = list("fr%it"= 7.45)), 
    regexp = "Put right columns dimension"
  )
  
})

# remove study ----
deleteStudy()
