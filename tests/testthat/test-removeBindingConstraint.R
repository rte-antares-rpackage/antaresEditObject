
# v870 ----

## Global data 
# read / open template study
setup_study_850(dir_path = sourcedir850)
opts_v850 <- antaresRead::setSimulationPath(study_temp_path, "input")

# areas list
antaresRead::getAreas(opts = opts_v850)

# remove BC none v870
names_bc_to_remove <- names(readBindingConstraints(opts = opts_v850))

lapply(names_bc_to_remove, 
       removeBindingConstraint,
       opts = simOptions())

# temporary to test with "870"
# force version
opts_v850$antaresVersion <- 870

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
    opts = opts_v850
  )
  
  createBindingConstraint(
    name = "myconstraint1",
    values = scenar_values_hourly,
    enabled = FALSE,
    timeStep = "hourly",
    operator = "both", 
    group = "group_test",
    coefficients = c("al%gr" = 1), 
    opts = opts_v850
  )
  
  createBindingConstraint(
    name = "myconstraint2",
    values = scenar_values_hourly,
    enabled = FALSE,
    timeStep = "hourly",
    operator = "both", 
    group = "group_test",
    coefficients = c("al%gr" = 1), 
    opts = opts_v850
  )
  
  bc_names_v870 <- names(readBindingConstraints(opts = opts_v850))
  
  removeBindingConstraint(bc_names_v870[1], 
                          opts = opts_v850)
  
  bc_in_study <- readBindingConstraints(opts = opts_v850)
  
  # test
  if(is.null(bc_in_study))
    testthat::expect_null(names(bc_in_study))
  else
    testthat::expect_false("myconstraint" %in% 
                   names(readBindingConstraints(opts = opts_v850)))
  
  # remove temporary study
  unlink(x = study_temp_path, recursive = TRUE)
  
})
