context("Function write_output_values")

sapply(studies, function(study) {
  setup_study(study, sourcedir)
  
  test_that("Write all", {
    
    opts <- antaresRead::setSimulationPath(studyPath, 1)
    
    #MCind
    data <- readAntares(links = "all", areas = "all", clusters = "all", mcYears = "all", showProgress = FALSE)
    data$areas$`OV. COST` <- data$areas$`OV. COST` + 1
    data$links$`FLOW LIN.` <- data$links$`FLOW LIN.` + 1
    data$clusters$production <- data$clusters$production + 1
    write_output_values(data, opts)
    data2 <- readAntares(links = "all", areas = "all", clusters = "all", mcYears = "all", showProgress = FALSE)
    
    expect_true(identical(as.matrix(data$areas), as.matrix(data2$areas)))
    expect_true(identical(as.matrix(data$links), as.matrix(data2$links)))
    expect_true(identical(as.matrix(data$clusters), as.matrix(data2$clusters)))
    
    
    #MCall
    data <- readAntares(links = "all", areas = "all", clusters = "all", showProgress = FALSE)
    data$areas$`OV. COST` <- data$areas$`OV. COST` + 1
    data$links$`FLOW LIN.` <- data$links$`FLOW LIN.` + 1
    data$clusters$production <- data$clusters$production + 1
    write_output_values(data, opts)
    data2 <- readAntares(links = "all", areas = "all", clusters = "all", showProgress = FALSE)
    
    expect_true(identical(as.matrix(data$areas), as.matrix(data2$areas)))
    expect_true(identical(as.matrix(data$links), as.matrix(data2$links)))
    expect_true(identical(as.matrix(data$clusters), as.matrix(data2$clusters)))
    
    
  })
  
  unlink(x = file.path(pathstd, "test_case"), recursive = TRUE)
  
  
})