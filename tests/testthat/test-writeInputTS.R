
context("Function writeInputTS")


sapply(studies, function(study) {
  
  setup_study(study, sourcedir)
  opts <- antaresRead::setSimulationPath(studyPath, "input")
  
  
  test_that("Write new input time series", {
    
    area <- sample(x = getOption("antares")$areaList, size = 1)
    
    M <- matrix(c(rep(8, 8760), rep(5.1, 8760)), nrow = 8760)
    
    writeInputTS(area = area, type = "solar", data = M)
    
    values_file <- file.path(pathstd, "test_case", "input", "solar", "series",
                             paste0("solar_", area, ".txt"))
    
    expect_equal(antaresRead:::fread_antares(opts = opts, file = values_file), as.data.table(M))
    
    #Wrong Area
    expect_error(
      writeInputTS(area = "fake area", type = "solar", data = M),
      regexp = "not a valid area"
    )
    
    #Run a second time the function without overwrite = TRUE.
    expect_error(
      writeInputTS(area = area, type = "solar", data = M, overwrite = FALSE),
      regexp = "already exist"
    )
    
    #Wrong dimension for data.
    expect_error(
      writeInputTS(area = area, type = "solar", data = matrix(1:3)),
      regexp = "8760\\*N matrix"
    )
    
    M_hydrostor <- matrix(c(rep(8, 365), rep(5.1, 365)), nrow = 365)
    
    writeInputTS(area = area, type = "hydroSTOR", data = M_hydrostor)
    
    values_file <- file.path(pathstd, "test_case", "input", "hydro", "series", area, "mod.txt")
    
    expect_equal(antaresRead:::fread_antares(opts = opts, file = values_file), as.data.table(M_hydrostor))
    
    #Wrong area
    expect_error(
      writeInputTS(area = "fake area", type = "hydroSTOR", data = M_hydrostor),
      regexp = "not a valid area"
    )
    
    #Run a second time the function without overwrite = TRUE.
    expect_error(
      writeInputTS(area = area, type = "hydroSTOR", data = M_hydrostor, overwrite = FALSE),
      regexp = "already exist"
    )
    
    #Wrong dimension for data.
    expect_error(
      writeInputTS(area = area, type = "hydroSTOR", data = matrix(1:3)),
      regexp = "365\\*N matrix"
    )
    
  })
  
  test_that("test error format when mingen file exists", {
    
    #Only for antaresVersion >= 860
    opts$antaresVersion <- 860
    
    area <- sample(x = getOption("antares")$areaList, size = 1)
    
    #Initialize mod.txt with 1 column to avoid error when we write mingen
    writeInputTS(area = area, type = "hydroSTOR", data = matrix(1,365,1))
    
    #Initialize mingen data
    M_mingen <- matrix(6,8760,5)
    
    writeHydroValues(area = area, type = "mingen", data = M_mingen , overwrite = TRUE, opts = opts)
    
    #Wrong format of data, here it must be either 1 or 6 columns.
    M_hydrostor <- matrix(c(rep(8, 365), rep(5.1, 365)), nrow = 365)
    
    #warning about the file format
    expect_warning(writeInputTS(area = area, type = "hydroSTOR", data = M_hydrostor),
                   regexp = "mod 'data' must be")
    
    
  })
  
  # remove temporary study
  unlink(x = file.path(pathstd, "test_case"), recursive = TRUE)
  
})

