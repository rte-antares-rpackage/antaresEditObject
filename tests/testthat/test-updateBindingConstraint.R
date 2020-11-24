context("Function editBindingConstraint")


sapply(studies, function(study) {
  
  setup_study(study, sourcedir)
  opts <- antaresRead::setSimulationPath(studyPath, "input")
  
  #Create a new binding constraint
  createBindingConstraint(
    name = "myconstraint", 
    values = matrix(data = rep(0, 8760 * 3), ncol = 3), 
    enabled = FALSE, 
    timeStep = "hourly",
    operator = "both"
  )
  
  ###Write params
  bc <- antaresRead::readBindingConstraints()
  bc <- bc[["myconstraint"]]
  editBindingConstraint("myconstraint", enabled = TRUE)
  bc2 <- antaresRead::readBindingConstraints()
  bc2 <- bc2[["myconstraint"]]
  expect_true(bc2$enabled)
  bc2$enabled <- FALSE
  bc$values <- data.frame(bc$values)
  bc2$values <- data.frame(bc2$values)
  expect_true(identical(bc, bc2))
  editBindingConstraint("myconstraint", coefficients = c("a%b" = 10))
  
  ##Write coef
  bc <- antaresRead::readBindingConstraints()
  expect_true(bc$myconstraint$coefs == c("a%b" = 10))
  editBindingConstraint("myconstraint", coefficients = c("a%b" = 100))
  bc <- antaresRead::readBindingConstraints()
  expect_true(bc$myconstraint$coefs == c("a%b" = 100))
  editBindingConstraint("myconstraint", coefficients = c("b%c" = 10))
  bc <- antaresRead::readBindingConstraints()
  expect_true(identical(bc$myconstraint$coefs,c("a%b" = 100, "b%c" = 10)))
  
  
  ##Write values
  
  expect_true(sum(bc$myconstraint$values) == 0)
  bc$myconstraint$timeStep
  editBindingConstraint("myconstraint", values = matrix(data = rep(1, 8760 * 3), ncol = 3))
  bc <- antaresRead::readBindingConstraints()
  expect_true(sum(bc$myconstraint$values) > 0 )
  
  
  # remove temporary study
  unlink(x = file.path(pathstd, "test_case"), recursive = TRUE)
  
})


