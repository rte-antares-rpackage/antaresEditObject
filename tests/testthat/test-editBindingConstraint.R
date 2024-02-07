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

testthat::skip()

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
test_that("editBindingConstraint with 'default group' v8.7.0", {

  bc <- readBindingConstraints(opts = opts_test)
    
  # edit BC with NULL values (edit only .ini file)
  editBindingConstraint(name = bc_names_v870, 
                        values = NULL, 
                        timeStep = "daily",
                        operator = "both", 
                        coefficients = c("al%gr"= 7.45),
                        opts = opts_test)
  
  bc_modified <- readBindingConstraints(opts = opts_test)
  new_coef <- bc_modified[[bc_names_v870[1]]]$coefs
  
  # test
  testthat::expect_true(all(new_coef %in% c(7.45)))
  
  # edit BC with "daily" VALUES
    # add new coeff
  editBindingConstraint(name = bc_names_v870, 
                        values = scenar_values_daily, 
                        timeStep = "daily",
                        operator = "both", 
                        coefficients = c("00_pump_d%gr" = 12, 
                                         "gr%00_turb_d" = 0, 
                                         "al%gr"= 0.5),
                        opts = opts_test)
  
  bc_modified <- readBindingConstraints(opts = opts_test)
  new_coef <- bc_modified[[bc_names_v870[1]]]$coefs
  
  # test coefs
  testthat::expect_true(all(new_coef %in% c(0.5, 12.0, 0.0)))
  
  # test dim values "daily"
    # the length of a standard year is 366 
  res_dim_values <- lapply(bc_modified$myconstraint$values, dim)
  res_dim_values <- lapply(res_dim_values, `[[`, 1)
  res_dim_values <- unlist(res_dim_values)
  testthat::expect_equal(mean(res_dim_values), 366)
  
  # test error with bad values dimension
  testthat::expect_error(
    editBindingConstraint(name = bc_names_v870, 
                          values = list(lt=matrix(data = rep(1, 365 * 9), 
                                                  ncol = 9)), 
                          timeStep = "daily",
                          operator = "both",
                          opts = opts_test), 
    regexp = "Put right columns dimension : 10 for existing 'group' : default group" 
  ) 
  
  # create new binding constraint with new dimension
  n <- 20
  lt_data <- matrix(data = rep(1, 8760 * n), ncol = n)
  gt_data <- matrix(data = rep(2, 8760 * n), ncol = n)
  eq_data <- matrix(data = rep(3, 8760 * n), ncol = n)
  
  scenar_values_hourly_new <- list(lt= lt_data,
                               gt= gt_data, 
                               eq= eq_data)
  
  createBindingConstraint(
    name = "myconstraint_new",
    values = scenar_values_hourly_new,
    enabled = FALSE,
    timeStep = "hourly",
    operator = "both",
    coefficients = c("al%gr" = 1), 
    group = "new",
    opts = opts_test
  )
  
  # edit group of last bindingConstraint with bad dimension
  testthat::expect_error(
    editBindingConstraint(name = "myconstraint_new", 
                          group = "default group",
                          opts = opts_test), 
    regexp = "Put right columns dimension : 10 for existing 'group' : default group"
    )
  
  # edit param 
  editBindingConstraint(name = "myconstraint_new", 
                        operator = "less",
                        opts = opts_test)
  
  bc_modified <- readBindingConstraints(opts = opts_test)
  new_param <- bc_modified[["myconstraint_new"]]$operator
  
  # test coefs
  testthat::expect_equal(new_param, "less")
  
    
  # remove temporary study
  unlink(x = study_latest_version, recursive = TRUE)
})


