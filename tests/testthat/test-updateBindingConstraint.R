context("Function editBindingConstraint")

test_that("editBindingConstraint tests", {
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
    # properties acces
    path_bc_ini <- file.path("input", "bindingconstraints", "bindingconstraints")
    bc <- readIni(pathIni = path_bc_ini)
    bc <- bc[[length(bc)]]
    editBindingConstraint("myconstraint", enabled = TRUE)
    
    # properties acces
    # list .ini files
    bc2 <- readIni(pathIni = path_bc_ini)
    
    bc2 <- bc2[[length(bc2)]]
    expect_true(bc2$enabled)
    
    bc2$enabled <- FALSE
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
    editBindingConstraint("myconstraint", values = matrix(data = rep(1, 8760 * 3), ncol = 3))
    bc <- antaresRead::readBindingConstraints()
    expect_true(sum(bc$myconstraint$values) > 0 )
    
    
    # remove temporary study
    unlink(x = file.path(pathstd, "test_case"), recursive = TRUE)
    
    })
})



