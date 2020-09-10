context("Function write_area_output_values")

sapply(studies, function(study) {
  setup_study(study, sourcedir)
  
  test_that("Write areas", {
    
    #MCind
    opts <- antaresRead::setSimulationPath(studyPath)
    data <- readAntares(mcYears = "all", links = "all")
    write_link_output_values(opts, data)
    data2 <- readAntares(mcYears = "all", links = "all")
    expect_true(identical(as.matrix(data), as.matrix(data2)))
    
    #MCall
    opts <- antaresRead::setSimulationPath(studyPath)
    data <- readAntares(mcYears = NULL, links = "all")
    write_link_output_values(opts, data)
    data2 <- readAntares(mcYears = NULL, links = "all")
    expect_true(identical(as.matrix(data), as.matrix(data2)))
  })
  
})