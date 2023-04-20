
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


# v8.6 ----

## Global data 
# read / open template study
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
n <- 10
lt_data <- matrix(data = rep(1, 8760 * n), ncol = n)
gt_data <- matrix(data = rep(2, 8760 * n), ncol = n)
eq_data <- matrix(data = rep(3, 8760 * n), ncol = n)

scenar_values <- list(lt= lt_data,
                      gt= gt_data, 
                      eq= eq_data)

test_that("createBindingConstraint v8.6", {
  
  # create binding constraint (default group value)  
  createBindingConstraint(
    name = "myconstraint",
    values = scenar_values,
    enabled = FALSE,
    timeStep = "hourly",
    operator = "both",
    coefficients = c("al%gr" = 1), 
    opts = opts_v850
  )
  
  # tests
  testthat::expect_true("myconstraint" %in% 
                names(readBindingConstraints(opts = opts_v850)))
  
  # remove BC
  removeBindingConstraint(name = "myconstraint", 
                          opts = opts_v850)
  
})


test_that("createBindingConstraintBulk v8.6", {
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
        group= "groupv860",
        overwrite = TRUE
      )
    }
  )
  # create all constraints
  createBindingConstraintBulk(bindings_constraints, opts = opts_v850)
  
  # tests
  testthat::expect_true("constraints1" %in% 
                          names(readBindingConstraints(opts = opts_v850)))
  testthat::expect_true("constraints10" %in% 
                          names(readBindingConstraints(opts = opts_v850)))
})

test_that("editBindingConstraint v8.6", {
  
})

test_that("removeBindingConstraint v8.6", {
  
})