context("Function writeHydroValues")

# v850 ----
# global params for structure v8.5
setup_study_850(sourcedir850)

#Avoid warning related to code writed outside test_that.
suppressWarnings(opts <- antaresRead::setSimulationPath(study_temp_path, "input"))

test_that("Write hydro values", {
  
  #Initialize data for each type of file.
  m_water <- matrix(1,365,101)
  m_reservoir <- matrix(2,365,3)
  m_maxpower <- matrix(3,365,4)
  m_inflowPattern <- matrix(4,365,1)
  m_creditmodulations <- matrix(5,2,101)
  m_mingen <- matrix(6,8760,1)
  
  area <- sample(x = getOption("antares")$areaList, size = 1)
  
  #waterValues case
  file_type="waterValues"
  writeHydroValues(area = area,type=file_type, data = m_water , overwrite = FALSE)
  
  values_file <- file.path(study_temp_path, "input", "hydro", "common", "capacity", 
                           paste0(file_type,"_", tolower(area), ".txt"))
  
  expect_equal(antaresRead:::fread_antares(opts = opts, file = values_file), as.data.table(m_water))
  
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
  
  expect_equal(antaresRead:::fread_antares(opts = opts, file = values_file), as.data.table(m_water))
  
  expect_error(
    writeHydroValues(area = area,type=file_type, data = matrix(1:4), overwrite = TRUE),
    regexp = "a 365\\*101 or \\(365\\*101\\)\\*3 matrix"
  )
  
  expect_error(
    writeHydroValues(area = "fake area",type=file_type, data = M2, overwrite = TRUE),
    regexp = "valid area"
  )
  
  # v860 ----
  # temporary to test with "860"
  # force version
  opts$antaresVersion <- 860
  
  #reservoir/maxpower/inflowPattern/creditsmodulation/mingen
  for (file_type in c("reservoir", "maxpower", "inflowPattern", "creditmodulations","mingen")){
    
    m_data <- switch(file_type,
                     "reservoir" = m_reservoir,
                     "maxpower" = m_maxpower,
                     "inflowPattern" = m_inflowPattern,
                     "creditmodulations" = m_creditmodulations,
                     "mingen" = m_mingen)
    
    #"mingen file can only be writed for antaresVersion >=860. 
    if (!(file_type == "mingen" && opts$antaresVersion < 860)){
      
      writeHydroValues(area = area, type = file_type, data = m_data , overwrite = TRUE, opts = opts)
      
      values_file <- file.path(study_temp_path, "input", "hydro", "common", "capacity", 
                               paste0(file_type, "_", tolower(area), ".txt"))
      
      #Test that the created file respect the matrix.
      expect_equal(antaresRead:::fread_antares(opts = opts, file = values_file), as.data.table(m_data))
      
      #Expect error when data format does not correspond.
      expect_error(
        writeHydroValues(area = area, type=file_type, data = matrix(1:4), overwrite = TRUE, opts = opts),
        regexp = "'data' must be"
      )
      
      #When we are in case mingen and antaresversion <v860. 
    } else {
      expect_error(
        writeHydroValues(area = area, type=file_type, data = m_data, overwrite = TRUE, opts = opts),
        regexp = "antaresVersion should be >= v8.6.0 to write mingen 'data'.")
    }
  }
  
  #unknown type
  expect_error(
    writeHydroValues(area = area, type = "toto", data = matrix(1:4), overwrite = TRUE),
    regexp = "'arg'"
  )
  
})

# remove temporary study
unlink(x = opts$studyPath, recursive = TRUE)

