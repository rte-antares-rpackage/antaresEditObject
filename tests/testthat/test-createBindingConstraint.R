
# v7 ----
context("Function createBindingConstraint")


sapply(studies, function(study) {
  
  setup_study(study, sourcedir)
  opts <- antaresRead::setSimulationPath(studyPath, "input")
  
  
  test_that("Create a new binding constraint", {
    
    createBindingConstraint(
      name = "myconstraint", 
      values = matrix(data = rep(0, 8760 * 3), ncol = 3), 
      enabled = FALSE, 
      timeStep = "hourly",
      operator = "both"
    )
    
    expect_true("myconstraint" %in% names(antaresRead::readBindingConstraints()))
  })
  
  
  
  test_that("Create a new binding constraint with named values", {
    
    values_named <- data.frame(
      less = as.integer(rep(0, 365)),
      greater = as.integer(rep(0, 365)),
      equal = as.integer(rep(0, 365))
    )
    
    createBindingConstraint(
      name = "namedvalues", 
      values = values_named, 
      timeStep = "daily"
    )
    
    expect_true("namedvalues" %in% names(antaresRead::readBindingConstraints()))
    expect_equal(
      as.data.frame(rbind(values_named, matrix(rep(0, 3), ncol = 3, dimnames = list(list(), names(values_named))))), 
      as.data.frame(antaresRead::readBindingConstraints()[["namedvalues"]]$values)
    )
  })
  
  
  
  test_that("Create a new binding constraint with BAD named values", {
    
    values_named <- data.frame(
      leeess = as.integer(rep(0, 365)),
      greaaaater = as.integer(rep(0, 365)),
      equaaaal = as.integer(rep(0, 365))
    )
    expect_error(
      createBindingConstraint(
        name = "badnamedvalues", 
        values = values_named, 
        timeStep = "daily"
      )
    )
  })
  
  test_that("Bad values for timestep", {
    
    values_named <- data.frame(
      less = as.integer(rep(0, 365)),
      greater = as.integer(rep(0, 365)),
      equal = as.integer(rep(0, 365))
    )
    
    expect_error(
      createBindingConstraint(
        name = "hourlyvalue", 
        values = values_named, 
        timeStep = "hourly"
      )
    )
  })
  
  
  
  test_that("No values", {
    
    expect_silent(
      createBindingConstraint(
        name = "silent", 
        timeStep = "hourly"
      )
    )
  })
  
  
  
  test_that("Create a new binding constraint with SINGLE named value", {
    
    values_named <- data.frame(
      greater = as.integer(rep(0, 365))
    )
    expect_silent(
      createBindingConstraint(
        name = "singlenamedvalue", 
        values = values_named, 
        timeStep = "daily"
      )
    )
  })
  
  
  
  test_that("Create a new binding constraint with coefficients", {
    
    coefs <- antaresRead::readBindingConstraints()[[1]]$coefs
    
    createBindingConstraint(
      name = "coeffs",
      timeStep = "weekly", 
      values = matrix(data = rep(0, 365 * 3), ncol = 3),
      coefficients = coefs
    )
    
    expect_identical(antaresRead::readBindingConstraints()[["coeffs"]]$coefs, coefs)
  })
  
  
  
  
  test_that("Create a new binding constraint with BAD coefficients", {
    
    expect_error(
      createBindingConstraint(
        name = "badcoeffs",
        timeStep = "weekly", 
        values = matrix(data = rep(0, 365 * 3), ncol = 3),
        coefficients =  c("z%psp in" = 12, "b%null" = 0, "fr%de" = 0.5)
      )
    )
    
  })
  
  test_that("Create a new binding constraint with cluster coefficients (not with %)", {
    
    coefs <- antaresRead::readBindingConstraints()[[1]]$coefs
    coefs <- c(coefs, "at.it" = 1)
    createBindingConstraint(
      name = "coeffs",
      timeStep = "weekly", 
      values = matrix(data = rep(0, 365 * 3), ncol = 3),
      coefficients = coefs, overwrite = TRUE
    )
    
    expect_identical(antaresRead::readBindingConstraints()[["coeffs"]]$coefs, coefs)
  })
  
  
  test_that("Remove a binding constraint", {
    
    removeBindingConstraint(name = "myconstraint")
    
    expect_false("myconstraint" %in% names(antaresRead::readBindingConstraints()))
  })
  
  
  
  test_that("Remove a binding constraint that doesn't exist", {
    expect_warning(removeBindingConstraint(name = "myimaginaryconstraint"))
  })
  
  
  # remove temporary study
  unlink(x = file.path(pathstd, "test_case"), recursive = TRUE)
  
})


# v8.7 ----

## Global data 
# read / open template study
setup_study_last(dir_path = sourcedir_last_study)

opts_test <- antaresRead::setSimulationPath(study_temp_path, "input")

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

# scenarized data hourly
n <- 10
lt_data <- matrix(data = rep(1, 8760 * n), ncol = n)
gt_data <- matrix(data = rep(2, 8760 * n), ncol = n)
eq_data <- matrix(data = rep(3, 8760 * n), ncol = n)

scenar_values <- list(lt= lt_data,
                      gt= gt_data, 
                      eq= eq_data)

# daily
n <- 9
lt_data <- matrix(data = rep(1, 365 * n), ncol = n)
gt_data <- matrix(data = rep(2, 365 * n), ncol = n)
eq_data <- matrix(data = rep(3, 365 * n), ncol = n)

scenar_values_daily <- list(lt= lt_data,
                            gt= gt_data, 
                            eq= eq_data)

test_that("createBindingConstraint (default group value) v8.7", {
  
  # create binding constraint   
  createBindingConstraint(
    name = "myconstraint",
    values = scenar_values,
    enabled = FALSE,
    timeStep = "hourly",
    operator = "both",
    coefficients = c("al%gr" = 1), 
    opts = opts_test
  )
  
  bc <- readBindingConstraints(opts = opts_test)
  
  # tests
  testthat::expect_true("myconstraint" %in% 
                names(bc))
  
  testthat::expect_equal(bc$myconstraint$group, "default group")
  
  
})


test_that("createBindingConstraintBulk v8.7", {
  # Prepare data for constraints 
  bindings_constraints <- lapply(
    X = seq_len(10),
    FUN = function(i) {
      # use arguments of createBindingConstraint()
      # all arguments must be provided !
      list(
        name = paste0("constraints", i), 
        id = paste0("constraints", i), 
        values = scenar_values, 
        enabled = FALSE, 
        timeStep = "hourly",
        operator = "both",
        coefficients = c("al%gr" = 1),
        group= "groupv870",
        overwrite = TRUE
      )
    }
  )
  # create all constraints
  createBindingConstraintBulk(bindings_constraints, opts = opts_test)
  
  # tests
  testthat::expect_true("constraints1" %in% 
                          names(readBindingConstraints(opts = opts_test)))
  testthat::expect_true("constraints10" %in% 
                          names(readBindingConstraints(opts = opts_test)))
  
})


test_that("createBindingConstraint check group values v8.7", {
  
  # create binding constraint (default group value)  
  createBindingConstraint(
    name = "myconstraint_group",
    values = scenar_values,
    enabled = FALSE,
    timeStep = "hourly",
    operator = "both",
    coefficients = c("al%gr" = 1), 
    opts = opts_test
  )
  
  # ADD binding constraint still (default group value)  
  createBindingConstraint(
    name = "myconstraint_group_bis",
    values = scenar_values,
    enabled = FALSE,
    timeStep = "hourly",
    operator = "both",
    coefficients = c("al%gr" = 1), 
    opts = opts_test
  )
  
  # ADD binding constraint with named group
  createBindingConstraint(
    name = "myconstraint_group1",
    values = scenar_values_daily,
    enabled = FALSE,
    timeStep = "daily",
    operator = "both", 
    group = "group_test",
    coefficients = c("al%gr" = 1), 
    opts = opts_test
  )
  
  # tests
  testthat::expect_true(
    all(
      c("myconstraint_group", "myconstraint_group_bis", "myconstraint_group1") %in% 
                          names(readBindingConstraints(opts = opts_test))
    )
    )
  
  # create binding constraint with bad values of existing group (default group) [ERROR] 
  testthat::expect_error(
    createBindingConstraint(
      name = "myconstraint_group_err",
      values = scenar_values_daily,
      enabled = FALSE,
      timeStep = "daily",
      operator = "both",
      coefficients = c("al%gr" = 1), 
      opts = opts_test
    )
  )
  
  # create binding constraint with bad values (NULL) of existing group (default group) [ERROR] 
  testthat::expect_error(
    createBindingConstraint(
      name = "myconstraint_group_err",
      values = NULL,
      enabled = FALSE,
      timeStep = "daily",
      operator = "both",
      coefficients = c("al%gr" = 1), 
      opts = opts_test
    )
  )
  
  # create binding constraint with bad values of existing group [ERROR] 
  testthat::expect_error(
    createBindingConstraint(
      name = "myconstraint_group2",
      values = scenar_values,
      enabled = FALSE,
      timeStep = "hourly",
      operator = "both", 
      group = "group_test",
      coefficients = c("al%gr" = 1), 
      opts = opts_test)
  )
  
  # ADD binding constraint with existing group
  createBindingConstraint(
    name = "myconstraint_group2",
    values = scenar_values_daily,
    enabled = FALSE,
    timeStep = "daily",
    operator = "both", 
    group = "group_test",
    coefficients = c("al%gr" = 1), 
    opts = opts_test)
  
  bc <- readBindingConstraints(opts = opts_test)
  
  testthat::expect_true("myconstraint_group2" %in% 
                          names(readBindingConstraints(opts = opts_test)))
  
  testthat::expect_equal(bc[["myconstraint_group2"]]$group, "group_test")
 
})


test_that("createBindingConstraint check values (NULL) group v8.7", {
  # create binding constraint (NULL value)  
  createBindingConstraint(
    name = "myconstraint_group_NULL",
    values = NULL,
    enabled = FALSE,
    operator = "both", 
    group = "null_values",
    coefficients = c("al%gr" = 1), 
    opts = opts_test
  )
  
  bc <- readBindingConstraints(opts = opts_test)
  
  # check name
  testthat::expect_true("myconstraint_group_NULL" %in% 
                          names(bc))
  
  # check dim
  dim_values <- unlist(lapply(bc$myconstraint_group_NULL$values, 
         function(x) dim(x)[2]))
  
  testthat::expect_equal(sum(dim_values), 0)
  
  # ADD binding constraint (existing group with NULL value) 
  createBindingConstraint(
    name = "myconstraint_group_3",
    values = NULL,
    enabled = FALSE,
    operator = "both", 
    timeStep = "hourly",
    group = "null_values",
    coefficients = c("al%gr" = 1), 
    opts = opts_test
  )
    
  bc <- readBindingConstraints(opts = opts_test)
  
  # check name
  testthat::expect_true("myconstraint_group_3" %in% 
                          names(bc))
  
  # remove temporary study
  unlink(x = study_temp_path, recursive = TRUE)
})
