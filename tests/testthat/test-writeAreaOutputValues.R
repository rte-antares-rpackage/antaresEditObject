context("Function write_area_output_values")

sapply(studies, function(study) {
  setup_study(study, sourcedir)
  
  test_that("Write areas", {
    
    #MCind
    opts <- antaresRead::setSimulationPath(studyPath, 1)
    data <- readAntares(mcYears = "all", showProgress = FALSE)
    write_area_output_values(opts, data)
    data2 <- readAntares(mcYears = "all", showProgress = FALSE)
    expect_true(identical(as.matrix(data), as.matrix(data2)))
    
    
    #MCall
    opts <- antaresRead::setSimulationPath(studyPath, 1)
    data <- readAntares(mcYears = NULL, showProgress = FALSE)
    write_area_output_values(opts, data)
    data2 <- readAntares(mcYears = NULL, showProgress = FALSE)
    expect_true(identical(as.matrix(data), as.matrix(data2)))
    
  })
  
  unlink(x = file.path(pathstd, "test_case"), recursive = TRUE)
  
  
})