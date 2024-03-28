
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
        coefficients =  c("psp in%z" = 12, "b%null" = 0, "de%fr" = 0.5)
      ), regexp = "not valid link"
    )
    
  })
  
  test_that("Create a new binding constraint with coefficients not ordered alphabetically", {
    
    expect_error(
      createBindingConstraint(
        name = "not_ordered",
        timeStep = "weekly", 
        values = matrix(data = rep(0, 365 * 3), ncol = 3),
        coefficients =  c("z%psp in" = 12, "fr%de" = 0)
      ), regexp = "The areas are not sorted alphabetically"
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

## parameter values ----
test_that("createBindingConstraint with bad structure values object v8.7", {
  
  # less
  bad_values <- scenar_values_daily[c("eq", "gt")]
  
  testthat::expect_error(
    createBindingConstraint(
      name = "bad_values",
      values = bad_values,
      enabled = FALSE,
      timeStep = "daily",
      operator = "less",
      coefficients = c("at%fr" = 1), 
      opts = opts_test
    ), regexp = "you must provide a list named according your parameter"
  )

  # both
  bad_values <- scenar_values_daily["eq"]
  
  testthat::expect_error(
    createBindingConstraint(
      name = "bad_values",
      values = bad_values,
      enabled = FALSE,
      timeStep = "daily",
      operator = "both",
      coefficients = c("at%fr" = 1), 
      opts = opts_test
    ), regexp = "you must provide a list named according your parameter"
  )
 
})

## add default bc ----
test_that("createBindingConstraint (default group value) v8.7", {
  ### with no values ----
    # /!\/!\/!\ output .txt file has to be empty
  createBindingConstraint(
    name = "myconstraint",
    values = NULL,
    enabled = FALSE,
    timeStep = "hourly",
    operator = "both",
    coefficients = c("at%fr" = 1), 
    overwrite = TRUE,
    opts = opts_test
  )
  
  bc <- readBindingConstraints(opts = opts_test)
  
  # tests
  testthat::expect_true("myconstraint" %in% 
                          names(bc))
  testthat::expect_equal(bc$myconstraint$properties$group, 
                         "default")
  
  # for both
  operator_bc <- c("_lt", "_gt")
  path_bc <- file.path(opts_test$inputPath, "bindingconstraints")
  path_file_bc <- paste0(file.path(path_bc, "myconstraint"), 
                         operator_bc, ".txt")
  
  # read .txt
  res <- lapply(path_file_bc, 
         antaresRead:::fread_antares, 
         opts = opts_test)
  
  res <- unlist(res)
  
  testthat::expect_equal(res, NULL)
  
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
  testthat::expect_equal(dim(scenar_values$lt)[2], 
                         dim(bc$myconstraint2$values$less)[2])
  
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
  data_ok <- list()
  data_ok$lt <- scenar_values_daily$lt[,1:dim_existing_group]
  data_ok$gt <- scenar_values_daily$gt[,1:dim_existing_group]
  
  # ADD binding constraint with good dimension
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
  
  bc <- readBindingConstraints(opts = opts_test)
  
  # tests
  testthat::expect_true("bc_existing_group" %in% 
                          names(bc))
  testthat::expect_equal(bc$bc_existing_group$properties$group, 
                         existing_name_group)
  testthat::expect_equal(dim(data_ok$lt)[2], 
                         dim(bc$bc_existing_group$values$less)[2])
  
})

## add new group ----
testthat::test_that("createBindingConstraint with new group v8.7",{
  
  # add values with the following steps
    # NULL => 1 column => >1 column => 1 column => NULL
    # error case with dimension different
  
  # ADD binding with NULL values
  createBindingConstraint(
    name = "bc_new_group_NULL",
    values = NULL,
    enabled = FALSE,
    timeStep = "hourly",
    operator = "greater", 
    group = "new_group",
    coefficients = c("at%fr" = 1), 
    opts = opts_test
  )
  
  # ADD binding with 1 col
  df_one_col <- scenar_values["lt"]
  df_one_col$lt <- df_one_col$lt[,1, drop = FALSE]
  
  createBindingConstraint(
    name = "bc_new_group_1",
    values = df_one_col,
    enabled = FALSE,
    timeStep = "hourly",
    operator = "less", 
    group = "new_group",
    coefficients = c("at%fr" = 1), 
    opts = opts_test
  )
  
  # ADD binding with multi cols
  df_one_col <- scenar_values["lt"]
  df_one_col$lt <- df_one_col$lt[,1:3, drop = FALSE]
  
  createBindingConstraint(
    name = "bc_new_group_multi",
    values = df_one_col,
    enabled = FALSE,
    timeStep = "hourly",
    operator = "less", 
    group = "new_group",
    coefficients = c("at%fr" = 1), 
    opts = opts_test
  )
  
  # ADD binding with 1 col
  df_one_col <- scenar_values["lt"]
  df_one_col$lt <- df_one_col$lt[,1, drop = FALSE]
  
  createBindingConstraint(
    name = "bc_new_group_1_bis",
    values = df_one_col,
    enabled = FALSE,
    timeStep = "hourly",
    operator = "less", 
    group = "new_group",
    coefficients = c("at%fr" = 1), 
    opts = opts_test
  )
  
  # ADD binding with NULL values
  createBindingConstraint(
    name = "bc_new_group_NULL_bis",
    values = NULL,
    enabled = FALSE,
    timeStep = "hourly",
    operator = "greater", 
    group = "new_group",
    coefficients = c("at%fr" = 1), 
    opts = opts_test
  )
  
  # ADD binding with NULL values
  createBindingConstraint(
    name = "bc_new_group_NULL_bis_both",
    values = NULL,
    enabled = FALSE,
    timeStep = "hourly",
    operator = "both", 
    group = "new_group",
    coefficients = c("at%fr" = 1), 
    opts = opts_test
  )
  
})


## bulk ----
test_that("createBindingConstraintBulk v8.7", {
  # Prepare data for constraints 
  bindings_constraints <- lapply(
    X = seq_len(10),
    FUN = function(i) {
      # use arguments of createBindingConstraint()
      # all arguments must be provided !
      list(
        name = paste0("constraints_bulk", i), 
        id = paste0("constraints_bulk", i), 
        values = scenar_values, 
        enabled = FALSE, 
        timeStep = "hourly",
        operator = "both",
        coefficients = c("at%fr" = 1),
        group= "group_bulk",
        overwrite = TRUE
      )
    }
  )
  # create all constraints
  createBindingConstraintBulk(bindings_constraints, opts = opts_test)
  
  # tests
  testthat::expect_true("constraints_bulk1" %in% 
                          names(readBindingConstraints(opts = opts_test)))
  testthat::expect_true("constraints_bulk10" %in% 
                          names(readBindingConstraints(opts = opts_test)))
  
})


# remove temporary study ----
unlink(x = study_latest_version, recursive = TRUE)
