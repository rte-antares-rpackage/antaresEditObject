
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

opts_test <- antaresRead::setSimulationPath(study_latest_version, "input")

# areas list
antaresRead::getAreas(opts = opts_test)

    # # remove BC none v870
    # names_bc_to_remove <- names(readBindingConstraints(opts = opts_test))
    # 
    # lapply(names_bc_to_remove, 
    #        removeBindingConstraint,
    #        opts = simOptions())
    # 
    # # temporary to test with "870"
    # # force version
    # opts_test$antaresVersion <- 870

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

## parameter group ----

## parameter values ----

## add default bc ----
test_that("createBindingConstraint (default group value) v8.7", {
  ### with no values ----
  createBindingConstraint(
    name = "myconstraint",
    values = NULL,
    enabled = FALSE,
    timeStep = "hourly",
    operator = "both",
    coefficients = c("at%fr" = 1), 
    opts = opts_test
  )
  
  bc <- readBindingConstraints(opts = opts_test)
  
  # tests
  testthat::expect_true("myconstraint" %in% 
                          names(bc))
  testthat::expect_equal(bc$myconstraint$properties$group, "default")
  
  ### with values ----
  createBindingConstraint(
    name = "myconstraint2",
    values = scenar_values,
    enabled = FALSE,
    timeStep = "hourly",
    operator = "both",
    coefficients = c("at%fr" = 1), 
    opts = opts_test
  )
  
  bc <- readBindingConstraints(opts = opts_test)
  
  # tests
  testthat::expect_true("myconstraint2" %in% 
                names(bc))
  testthat::expect_equal(bc$myconstraint2$properties$group, "default")
  
  ### error dim ----
  # add BC with daily values (different columns dimension ERROR) 
  testthat::expect_error(
    createBindingConstraint(
      name = "myconstraint_daily",
      values = scenar_values_daily,
      enabled = FALSE,
      timeStep = "daily",
      operator = "both",
      coefficients = c("at%fr" = 1), 
      opts = opts_test
    ), regexp = "Put right columns dimension"
  )
  
})

## existing named group ----
  # study provide BC with group "group_test"
test_that("createBindingConstraint with existing group v8.7", {
  testthat::skip()
  
  # read to have dimension
  bc <- readBindingConstraints()
  
  # dimension according BC/group
  dim_bc <- lapply(bc, function(x){
    if(x$properties$operator %in% "both")
      list(group = x$properties$group,
           dim_values = dim(x$values$less)[2])
    else
      list(group = x$properties$group,
         dim_values = dim(x$values)[2])
  })
  
  existing_name_group <- dim_bc$bc_1$group
  dim_existing_group <- dim_bc$bc_1$dim_values
  
  # ADD binding constraint with bad dimension
  testthat::expect_error(
    createBindingConstraint(
      name = "myconstraint_group1",
      values = scenar_values_daily,
      enabled = FALSE,
      timeStep = "daily",
      operator = "both", 
      group = existing_name_group,
      coefficients = c("at%fr" = 1), 
      opts = opts_test
    ), regexp = "Put right columns dimension"
  )
  
  # put right dimension
  data_ok <- list(lt=NULL,
                  gt=NULL,
                  eq=NULL)
  data_ok$lt <- scenar_values_daily$lt[,1:dim_existing_group]
  data_ok$gt <- scenar_values_daily$gt[,1:dim_existing_group]
  
  # ADD binding constraint with bad dimension
  createBindingConstraint(
    name = "bc_existing_group",
    values = data_ok,
    enabled = FALSE,
    timeStep = "daily",
    operator = "both", 
    group = existing_name_group,
    coefficients = c("at%fr" = 1), 
    opts = opts_test
  )
  

 
})


## bulk ----
test_that("createBindingConstraintBulk v8.7", {
  testthat::skip()
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


# remove temporary study ----
unlink(x = study_latest_version, recursive = TRUE)
