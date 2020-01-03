
context("Function writeWaterValues")


sapply(studies, function(study) {
  
  setup_study(study, sourcedir)
  opts <- antaresRead::setSimulationPath(studyPath, "input")
  
  test_that("Write new water values", {
    
    area <- sample(x = getOption("antares")$areaList, size = 1)
    
    M <- matrix(rep(0, 365*101), nrow = 365)
    
    writeWaterValues(area = area, data = M, overwrite = FALSE)
    
    values_file <- file.path(path, "test_case", "input", "hydro", "common", "capacity", 
                             paste0("waterValues_", tolower(area), ".txt"))
    
    expect_equal(fread(values_file), as.data.table(M))
    
    M2 <- cbind(
      date = rep(seq(as.Date("2018-01-01"), by = 1, length.out = 365), each = 101),
      level = rep(0:100, times = 365),
      value = rep(0, 365*101)
    )
    
    expect_error(
      writeWaterValues(area = area, data = M2, overwrite = FALSE),
      regexp = "already exist"
    )
    
    writeWaterValues(area = area, data = M2, overwrite = TRUE)
    
    expect_equal(fread(values_file), as.data.table(M))
    
    expect_error(
      writeWaterValues(area = area, data = matrix(1:4), overwrite = TRUE),
      regexp = "a 365\\*101 or \\(365\\*101\\)\\*3 matrix"
    )
    
    expect_error(
      writeWaterValues(area = "fake area", data = M2, overwrite = TRUE),
      regexp = "valid area"
    )
  })
  
  
  # remove temporary study
  unlink(x = file.path(path, "test_case"), recursive = TRUE)
  
})

