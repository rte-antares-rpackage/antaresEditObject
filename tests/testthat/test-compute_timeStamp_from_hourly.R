context("Function computeTimeStampFromHourly")

skip("Deprecated")

sapply(studies, function(study) {
  
  setup_study(study, sourcedir)
  
  opts <- antaresRead::setSimulationPath(studyPath, 1)
  
  outputd <- antaresRead::readAntares(areas = "all", mcYears = "all", timeStep = "daily", showProgress = FALSE)
  outputw <- antaresRead::readAntares(areas = "all", mcYears = "all", timeStep = "weekly", showProgress = FALSE)
  outputm <- antaresRead::readAntares(areas = "all", mcYears = "all", timeStep = "monthly", showProgress = FALSE)
  outputa <- antaresRead::readAntares(areas = "all", mcYears = "all", timeStep = "annual", showProgress = FALSE)
  
  
  outputLd <- antaresRead::readAntares(links = "all", mcYears = "all", timeStep = "daily", showProgress = FALSE)
  outputLw <- antaresRead::readAntares(links = "all", mcYears = "all", timeStep = "weekly", showProgress = FALSE)
  outputLm <- antaresRead::readAntares(links = "all", mcYears = "all", timeStep = "monthly", showProgress = FALSE)
  outputLa <- antaresRead::readAntares(links = "all", mcYears = "all", timeStep = "annual", showProgress = FALSE)
  
  
  .testDT <- function(dt, dt2, seuil){
    max(unlist(lapply(names(dt), function(X){
      if(is.numeric(dt[[X]])){
        max(dt[[X]] - dt2[[X]])
      }
    })), na.rm = T) < seuil
  }
  
  computeTimeStampFromHourly(opts, nbcl = 1, verbose = 0)
  
  outputdafter <- antaresRead::readAntares(areas = "all", mcYears = "all", timeStep = "daily", showProgress = FALSE)
  outputwafter <- antaresRead::readAntares(areas = "all", mcYears = "all", timeStep = "weekly", showProgress = FALSE)
  outputmafter <- antaresRead::readAntares(areas = "all", mcYears = "all", timeStep = "monthly", showProgress = FALSE)
  outputaafter <- antaresRead::readAntares(areas = "all", mcYears = "all", timeStep = "annual", showProgress = FALSE)
  
  outputdafterL <- antaresRead::readAntares(links = "all", mcYears = "all", timeStep = "daily", showProgress = FALSE)
  outputwafterL <- antaresRead::readAntares(links = "all", mcYears = "all", timeStep = "weekly", showProgress = FALSE)
  outputmafterL <- antaresRead::readAntares(links = "all", mcYears = "all", timeStep = "monthly", showProgress = FALSE)
  outputaafterL <- antaresRead::readAntares(links = "all", mcYears = "all", timeStep = "annual", showProgress = FALSE)
  
  
  
  test_that("daily data", {
    
  testthat::expect_true(identical(names(outputd), names(outputdafter)))
  testthat::expect_true(.testDT(outputd, outputdafter, 15))
  
  testthat::expect_true(identical(names(outputLd), names(outputdafterL)))
  testthat::expect_true(.testDT(outputLd, outputdafterL, 15))
  
  
  })
  
  test_that("weekly data", {
    
  testthat::expect_true(identical(names(outputw), names(outputwafter)))
  testthat::expect_true(.testDT(outputw, outputwafter, 60))
  
  testthat::expect_true(identical(names(outputLw), names(outputwafterL)))
  testthat::expect_true(.testDT(outputLw, outputwafterL, 60))
  
  
  })
  
  test_that("monthly data", {
    
  testthat::expect_true(identical(names(outputm), names(outputmafter)))
  testthat::expect_true(.testDT(outputm, outputmafter, 100))
  
  testthat::expect_true(identical(names(outputLm), names(outputmafterL)))
  testthat::expect_true(.testDT(outputLm, outputmafterL, 100))
  
  })
  
  test_that("annual data", {
  
  testthat::expect_true(identical(names(outputa), names(outputaafter)))
  testthat::expect_true(.testDT(outputa, outputaafter, 150))
  
  testthat::expect_true(identical(names(outputLa), names(outputaafterL)))
  testthat::expect_true(.testDT(outputLa, outputaafterL, 150))
  
  })
  
  unlink(x = file.path(pathstd, "test_case"), recursive = TRUE)
  
})
