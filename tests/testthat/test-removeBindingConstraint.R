testthat::skip()

# v870 ----

## Global data 
# read / open template study
setup_study_last(dir_path = sourcedir_last_study)
opts_test <- antaresRead::setSimulationPath(study_latest_version, "input")

# areas list
antaresRead::getAreas(opts = opts_test)

# remove BC none v870
names_bc_to_remove <- names(readBindingConstraints(opts = opts_test))

lapply(names_bc_to_remove, 
       removeBindingConstraint,
       opts = simOptions())

# temporary to test with "870"
# force version
opts_test$antaresVersion <- 870

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

test_that("removeBindingConstraint v8.6", {
  
  # create binding constraint (default group value)  
  createBindingConstraint(
    name = "myconstraint",
    values = scenar_values_hourly,
    enabled = FALSE,
    timeStep = "hourly",
    operator = "both",
    coefficients = c("al%gr" = 1), 
    opts = opts_test
  )
  
  createBindingConstraint(
    name = "myconstraint1",
    values = scenar_values_hourly,
    enabled = FALSE,
    timeStep = "hourly",
    operator = "both", 
    group = "group_test",
    coefficients = c("al%gr" = 1), 
    opts = opts_test
  )
  
  createBindingConstraint(
    name = "myconstraint2",
    values = scenar_values_hourly,
    enabled = FALSE,
    timeStep = "hourly",
    operator = "both", 
    group = "group_test",
    coefficients = c("al%gr" = 1), 
    opts = opts_test
  )
  
  bc_names_v870 <- names(readBindingConstraints(opts = opts_test))
  
  removeBindingConstraint(bc_names_v870[1], 
                          opts = opts_test)
  
  bc_in_study <- readBindingConstraints(opts = opts_test)
  
  # test
  if(is.null(bc_in_study))
    testthat::expect_null(names(bc_in_study))
  else
    testthat::expect_false("myconstraint" %in% 
                   names(readBindingConstraints(opts = opts_test)))
  
  # remove temporary study
  unlink(x = study_latest_version, recursive = TRUE)
  
})
