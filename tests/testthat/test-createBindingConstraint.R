

context("Function createBindingConstraint")

# v710----
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


