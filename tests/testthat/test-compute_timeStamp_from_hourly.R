context("Function computeTimeStampFromHourly")

sapply(studies, function(study) {
  
  setup_study(study, sourcedir)
  opts <- antaresRead::setSimulationPath(studyPath, 1)
  
  outputd <- antaresRead::readAntares(areas = "all", mcYears = "all", timeStep = "daily")
  outputw <- antaresRead::readAntares(areas = "all", mcYears = "all", timeStep = "weekly")
  outputm <- antaresRead::readAntares(areas = "all", mcYears = "all", timeStep = "monthly")
  outputa <- antaresRead::readAntares(areas = "all", mcYears = "all", timeStep = "annual")
  
  .testDT <- function(dt, dt2, seuil){
    max(unlist(lapply(names(dt), function(X){
      if(is.numeric(dt[[X]])){
        max(dt[[X]] - dt2[[X]])
      }
    })), na.rm = T) < seuil
  }
  
  computeTimeStampFromHourly(opts, nbcl = 1, verbose = 0)
  
  outputdafter <- antaresRead::readAntares(areas = "all", mcYears = "all", timeStep = "daily")
  outputwafter <- antaresRead::readAntares(areas = "all", mcYears = "all", timeStep = "weekly")
  outputmafter <- antaresRead::readAntares(areas = "all", mcYears = "all", timeStep = "monthly")
  outputaafter <- antaresRead::readAntares(areas = "all", mcYears = "all", timeStep = "annual")
  
  
  test_that("daily data", {
    
  testthat::expect_true(identical(names(outputd), names(outputdafter)))
  testthat::expect_true(.testDT(outputd, outputdafter, 15))
  
  })
  
  test_that("weekly data", {
    
  testthat::expect_true(identical(names(outputw), names(outputwafter)))
  testthat::expect_true(.testDT(outputw, outputwafter, 60))
  
  })
  
  test_that("monthly data", {
    
  testthat::expect_true(identical(names(outputm), names(outputmafter)))
  testthat::expect_true(.testDT(outputm, outputmafter, 100))
  
  })
  
  test_that("annual data", {
  
  testthat::expect_true(identical(names(outputa), names(outputaafter)))
  testthat::expect_true(.testDT(outputa, outputaafter, 150))
  
  })
  
})
