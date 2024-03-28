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
                        coefficients = c("b%psp in"= 1.75,
                                         "b%psp out"= 2),
                        opts = simOptions())
  
  bc_modified <- antaresRead::readBindingConstraints()
  new_coef <- bc_modified[[bc_names[1]]]$coefs
  
  # test
  testthat::expect_true(all(new_coef %in% c(1.75, 2)))
  
  # remove temporary study
  unlink(x = file.path(pathstd, "test_case"), recursive = TRUE)
  
})

# v870 ----

## Global data 
# read / open template study
setup_study_last(dir_path = sourcedir_last_study)
opts_test <- antaresRead::setSimulationPath(study_latest_version, "input")

# areas list
antaresRead::getAreas(opts = opts_test)

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
test_that("editBindingConstraint with 'default' group v8.7.0", {

  # PS : in this study, "default" have 1 column dimension
  bc <- readBindingConstraints(opts = opts_test)
    
  # edit properties + values (good dimension)
    # edit "greater" to "both"
  bc_names_v870 <- bc$bc_2$properties$id
  editBindingConstraint(name = bc_names_v870, 
                        values = scenar_values_daily, 
                        timeStep = "daily",
                        operator = "both", 
                        filter_year_by_year = "daily",
                        filter_synthesis = "daily",
                        coefficients = c("fr%it"= 7.45),
                        opts = opts_test)
  
  # read
  bc_modified <- readBindingConstraints(opts = opts_test)
  new_coef <- bc_modified[[bc_names_v870]]$coefs
  timeStep <- bc_modified[[bc_names_v870]]$properties$timeStep
  operator <- bc_modified[[bc_names_v870]]$properties$operator
  filter_year <- bc_modified[[bc_names_v870]]$properties$`filter-year-by-year`
  filter_synthesis <- bc_modified[[bc_names_v870]]$properties$`filter-synthesis`
  
  # test properties
  testthat::expect_true(all(new_coef %in% c(7.45)))
  testthat::expect_true(timeStep %in% "daily")
  testthat::expect_true(operator %in% "both")
  testthat::expect_true(filter_year %in% "daily")
  testthat::expect_true(filter_synthesis %in% "daily")
  
  # test values
  dim_col_values_input <- dim(scenar_values_daily$lt)[2]
  dim_col_values_edited <- dim(bc_modified[[bc_names_v870]]$values$less)[2]
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
    editBindingConstraint(name = bc_names_v870, 
                          values = scenar_values_daily_n, 
                          timeStep = "daily",
                          operator = "both", 
                          coefficients = c("fr%it"= 7.45),
                          opts = opts_test), 
    regexp = "Put right columns dimension"
  )
  
  
  
  ### multi coeff ----
  editBindingConstraint(name = bc_names_v870, 
                        values = NULL, 
                        timeStep = "daily",
                        operator = "both", 
                        coefficients = c("fr%it" = 12, 
                                         "fr%at" = 0),
                        opts = opts_test)
  
  # read
  bc_modified <- readBindingConstraints(opts = opts_test)
  new_coef <- bc_modified[[bc_names_v870]]$coefs
  
  # test coefs
  testthat::expect_true(all(new_coef %in% c(12, 0)))
  
})

## exisintg group ----
test_that("editBindingConstraint with existing group v8.7.0", {
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
  n <- 2
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
                        coefficients = c("fr%it" = 12),
                        opts = opts_test)
  
  # read
  bc_modified <- readBindingConstraints(opts = opts_test)
  new_coef <- bc_modified[[bc_no_default]]$coefs
  timeStep <- bc_modified[[bc_no_default]]$properties$timeStep
  operator <- bc_modified[[bc_no_default]]$properties$operator
  
  # test properties
  testthat::expect_true(all(new_coef %in% c(1, 12)))
  testthat::expect_true(timeStep %in% "daily")
  testthat::expect_true(operator %in% "both")
  
  # test values
  dim_col_values_input <- dim(scenar_values_daily_n$lt)[2]
  dim_col_values_edited <- dim(bc_modified[[bc_no_default]]$values$less)[2]
  testthat::expect_equal(dim_col_values_input, dim_col_values_edited)
  
})

# remove study ----
unlink(x = study_latest_version, recursive = TRUE)
