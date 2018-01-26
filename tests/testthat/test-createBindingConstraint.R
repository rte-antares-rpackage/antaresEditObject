#Copyright © 2016 RTE Réseau de transport d’électricité


context("Function createBindingConstraint")



# Setup study -------------------------------------------------------------


path <- tempdir()
# Unzip the study
setup_study(path, sourcedir)
# set simulation path in mode input
opts <- antaresRead::setSimulationPath(studyPath, 'input')





# Tests -------------------------------------------------------------------


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






# End ---------------------------------------------------------------------


# remove temporary study
unlink(x = file.path(path, "test_case"), recursive = TRUE)


