context("Function writeHydroValues")

sapply(studies, function(study) {
  
  setup_study(study, sourcedir)
  opts <- antaresRead::setSimulationPath(studyPath, "input")
  
  m_water <- matrix(1,365,101)
  m_reservoir <- matrix(2,365,3)
  m_maxpower <- matrix(3,365,4)
  m_inflowPattern <- matrix(4,365,1)
  m_creditmodulations <- matrix(5,2,101)
  m_mingen <- matrix(6,8760,1)
  
  area <- sample(x = getOption("antares")$areaList, size = 1)
  
  test_that("Write hydro values", {
    
    #waterValues case
    file_type="waterValues"
    writeHydroValues(area = area,type=file_type, data = m_water , overwrite = FALSE)
    
    values_file <- file.path(pathstd, "test_case", "input", "hydro", "common", "capacity", 
                             paste0(file_type,"_", tolower(area), ".txt"))
    
    expect_equal(fread(values_file), as.data.table(m_water))
    
    M2 <- cbind(
      date = rep(seq(as.Date("2018-01-01"), by = 1, length.out = 365), each = 101),
      level = rep(0:100, times = 365),
      value = rep(1, 365*101)
    )
    
    expect_error(
      writeHydroValues(area = area,type=file_type, data = M2, overwrite = FALSE),
      regexp = "already exist"
    )
    
    writeHydroValues(area = area, type=file_type, data = M2, overwrite = TRUE)
    
    expect_equal(fread(values_file), as.data.table(m_water))
    
    expect_error(
      writeHydroValues(area = area,type=file_type, data = matrix(1:4), overwrite = TRUE),
      regexp = "a 365\\*101 or \\(365\\*101\\)\\*3 matrix"
    )
    
    expect_error(
      writeHydroValues(area = "fake area",type=file_type, data = M2, overwrite = TRUE),
      regexp = "valid area"
    )
    
    #reservoir/maxpower/inflowPattern/creditsmodulation
    for (file_type in c("reservoir", "maxpower", "inflowPattern", "creditmodulations","mingen")){
      m_data <- switch(file_type,
                       "reservoir" = m_reservoir,
                       "maxpower" = m_maxpower,
                       "inflowPattern" = m_inflowPattern,
                       "creditmodulations" = m_creditmodulations,
                       "mingen" = m_mingen)
      
      writeHydroValues(area = area, type = file_type, data = m_data , overwrite = TRUE)
      
      values_file <- file.path(pathstd, "test_case", "input", "hydro", "common", "capacity", 
                               paste0(file_type, "_", tolower(area), ".txt"))
      
      expect_equal(fread(values_file), as.data.table(m_data))
      
      expect_error(
        writeHydroValues(area = area, type=file_type, data = matrix(1:4), overwrite = TRUE),
        regexp = "'data' must be"
      )
    }
    
    #unknown type
    expect_error(
      writeHydroValues(area = area, type = "toto", data = matrix(1:4), overwrite = TRUE),
      regexp = "'arg'"
    )
    
  })

  # remove temporary study
  unlink(x = file.path(pathstd, "test_case"), recursive = TRUE)
  
})

