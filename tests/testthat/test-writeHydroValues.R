context("Function writeHydroValues")

#WriteHydroValues does not depend on antaresVersion.
# waterValues ----
# global params for structure v8.5
setup_study_850(sourcedir850)

#Avoid warning related to code writed outside test_that.
suppressWarnings(opts <- antaresRead::setSimulationPath(study_temp_path, "input"))

test_that("Write hydro values, 'waterValues' case", {
  
  #Initialize data for each type of file.
  m_water <- matrix(1,365,101)
  
  area <- sample(x = getOption("antares")$areaList,
                 size = 1)
  
  #waterValues case, there is 2 file formats for waterValues.
  
  writeHydroValues(area = area,type="waterValues",
                   data = m_water ,
                   overwrite = FALSE)
  
  values_file <- file.path(study_temp_path, "input", "hydro", "common", "capacity", 
                           paste0("waterValues_", tolower(area), ".txt"))
  
  expect_equal(antaresRead:::fread_antares(opts = opts,
                                           file = values_file),
               as.data.table(m_water))
  
  M2 <- cbind(
    date = rep(seq(as.Date("2018-01-01"), by = 1, length.out = 365), each = 101),
    level = rep(0:100, times = 365),
    value = rep(1, 365*101)
  )
  
  expect_error(
    writeHydroValues(area = area,
                     type="waterValues",
                     data = M2,
                     overwrite = FALSE),
    regexp = "already exist"
  )
  
  writeHydroValues(area = area,
                   type="waterValues",
                   data = M2,
                   overwrite = TRUE)
  
  expect_equal(antaresRead:::fread_antares(opts = opts, file = values_file),
               as.data.table(m_water))
  
  #Wrong data format
  expect_error(
    writeHydroValues(area = area,
                     type="waterValues",
                     data = matrix(1:4),
                     overwrite = TRUE),
    regexp = "a 365\\*101 or \\(365\\*101\\)\\*3 matrix"
  )
  
  #Wrong area
  expect_error(
    writeHydroValues(area = "fake area",
                     type="waterValues",
                     data = M2,
                     overwrite = TRUE),
    regexp = "valid area"
  )
  
  #unknown type
  expect_error(
    writeHydroValues(area = area,
                     type = "toto",
                     data = matrix(1:4),
                     overwrite = TRUE),
    regexp = "'arg'"
  )
})

# Other cases ----
test_that("writeHydroValues, reservoir/maxpower/inflowPattern/creditmodulations cases", {
  
  #Initialize data
  m_reservoir <- matrix(2,365,3)
  m_maxpower <- matrix(3,365,4)
  m_inflowPattern <- matrix(4,365,1)
  m_creditmodulations <- matrix(5,2,101)
  
  area <- sample(x = getOption("antares")$areaList, size = 1)
  
  #reservoir/maxpower/inflowPattern/creditsmodulation
  for (file_type in c("reservoir", "maxpower", "inflowPattern", "creditmodulations")){
    
    m_data <- switch(file_type,
                     "reservoir" = m_reservoir,
                     "maxpower" = m_maxpower,
                     "inflowPattern" = m_inflowPattern,
                     "creditmodulations" = m_creditmodulations,)
    
    #Create the file
    writeHydroValues(area = area,
                     type = file_type,
                     data = m_data ,
                     overwrite = TRUE)
    
    values_file <- file.path(study_temp_path, "input", "hydro", "common", "capacity", 
                             paste0(file_type, "_", tolower(area), ".txt"))
    
    #Test that the created file respect the matrix.
    expect_equal(antaresRead:::fread_antares(opts = opts,
                                             file = values_file),
                 as.data.table(m_data))
    
    #Expect error when data format does not correspond.
    expect_error(
      writeHydroValues(area = area,
                       type=file_type,
                       data = matrix(1:4),
                       overwrite = TRUE),
      regexp = "'data' must be"
    )
    
    #unknown type
    expect_error(
      writeHydroValues(area = area,
                       type = "toto",
                       data = m_maxpower,
                       overwrite = TRUE),
      regexp = "'arg'"
    )
  }
})


# remove temporary study
unlink(x = opts$studyPath, recursive = TRUE)

