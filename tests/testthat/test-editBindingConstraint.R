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

# v860 ----

## Global data 
# read / open template study
setup_study_850(dir_path = sourcedir850)
opts_v850 <- antaresRead::setSimulationPath(study_temp_path, "input")

# areas list
antaresRead::getAreas(opts = opts_v850)

# remove BC none v860
names_bc_to_remove <- names(readBindingConstraints(opts = opts_v850))

lapply(names_bc_to_remove, 
       removeBindingConstraint,
       opts = simOptions())

# temporary to test with "860"
# force version
opts_v850$antaresVersion <- 860

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


test_that("editBindingConstraint v8.6", {
  
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
  
  bc_names_v860 <- names(readBindingConstraints(opts = opts_v850))
    
  # edit BC with NULL VALUES
  editBindingConstraint(name = bc_names_v860, 
                        values = NULL, 
                        timeStep = "daily",
                        operator = "both", 
                        coefficients = c("al%gr"= 7.45),
                        opts = opts_v850)
  
  bc_modified <- readBindingConstraints(opts = opts_v850)
  new_coef <- bc_modified[[bc_names_v860[1]]]$coefs
  
  # test
  testthat::expect_true(all(new_coef %in% c(7.45)))
  
  # edit BC with "daily" VALUES
    # add new coeff
  editBindingConstraint(name = bc_names_v860, 
                        values = scenar_values_daily, 
                        timeStep = "daily",
                        operator = "both", 
                        coefficients = c("00_pump_d%gr" = 12, 
                                         "gr%00_turb_d" = 0, 
                                         "al%gr"= 0.5),
                        opts = opts_v850)
  
  bc_modified <- readBindingConstraints(opts = opts_v850)
  new_coef <- bc_modified[[bc_names_v860[1]]]$coefs
  
  # test coefs
  testthat::expect_true(all(new_coef %in% c(0.5, 12.0, 0.0)))
  
  # test dim values "daily"
    # the length of a standard year is 366 
  res_dim_values <- lapply(bc_modified$myconstraint$values, dim)
  res_dim_values <- lapply(res_dim_values, `[[`, 1)
  res_dim_values <- unlist(res_dim_values)
  testthat::expect_equal(mean(res_dim_values), 366)
    
  # remove temporary study
  unlink(x = study_temp_path, recursive = TRUE)
  
})
