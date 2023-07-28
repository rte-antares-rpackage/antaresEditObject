context("Function writeHydroValues")

#WriteHydroValues does not depend on antaresVersion.
# waterValues ----
# global params for structure v8.6
setup_study_860(sourcedir860)

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


# hydro.ini
test_that("Write hydro.ini values for the first area, edit leeway up, leeway low and reservoir", {
  
  translate_value <- 23
  
  hydro_ini_path <- file.path("input", "hydro", "hydro.ini")
  hydro_ini_data <- antaresRead::readIni(pathIni = hydro_ini_path, opts = opts)
  
  area_to_edit <- opts$areaList[1]
  new_data <- list("leeway low" = hydro_ini_data[["leeway low"]][[area_to_edit]] + translate_value,
                   "leeway up" = hydro_ini_data[["leeway up"]][[area_to_edit]] + translate_value,
                   "reservoir" = !is.null(hydro_ini_data[["reservoir"]][[area_to_edit]])
  )
  writeIniHydro(area = area_to_edit, params = new_data, mode = "other", opts = opts)
  hydro_ini_after_edit <- antaresRead::readIni(pathIni = hydro_ini_path, opts = opts)
  
  expect_equal(hydro_ini_after_edit[["leeway low"]][[area_to_edit]] - hydro_ini_data[["leeway low"]][[area_to_edit]], translate_value)
  expect_equal(hydro_ini_after_edit[["leeway up"]][[area_to_edit]] - hydro_ini_data[["leeway up"]][[area_to_edit]], translate_value)
  expect_equal(hydro_ini_after_edit[["reservoir"]][[area_to_edit]], !is.null(hydro_ini_data[["reservoir"]][[area_to_edit]]))
  
  # Bad section names
  bad_data <- list("leeway lowwwwwwwwww" = hydro_ini_data[["leeway low"]][[area_to_edit]] + translate_value,
                   "leeway upppppppppp" = hydro_ini_data[["leeway up"]][[area_to_edit]] + translate_value,
                   "reservoirrrrrrrrr" = !is.null(hydro_ini_data[["reservoir"]][[area_to_edit]])
  )
  
  expect_error(
    writeIniHydro(area = area_to_edit, params = bad_data, mode = "other", opts = opts),
    regexp = "Parameter params must be named with the following elements:"
  )
  
  # Bad types
  bad_types <- list("leeway low" = "toto",
                    "leeway up" = "titi",
                    "reservoir" = 35
  )
  
  expect_error(
    writeIniHydro(area = area_to_edit, params = bad_types, mode = "other", opts = opts),
    regexp = "The following parameters have a wrong type:"
  )
  
})


test_that("Write NULL hydro.ini values to ensure its behaviour", {
  
  hydro_ini_path <- file.path("input", "hydro", "hydro.ini")
  hydro_ini_data <- antaresRead::readIni(pathIni = hydro_ini_path, opts = opts)
  
  fname <- names(hydro_ini_data)[1]
  farea <- names(hydro_ini_data[[fname]])[1]
  
  writeIniHydro(area = farea, params = setNames(list(NULL), fname), mode = "other", opts = opts)
  hydro_ini_after_edit <- antaresRead::readIni(pathIni = hydro_ini_path, opts = opts)
  
  expect_true(!is.null(hydro_ini_data[[fname]][[farea]]))
  expect_true(is.null(hydro_ini_after_edit[[fname]][[farea]]))
})



test_that("writeIniHydro(): check if consistency between reservoir and reservoir capacity is preserved", {
  
  custom_reservoir_capa <- 123456
  
  ant_version <- "8.2.0"
  st_test <- paste0("my_study_820_", paste0(sample(letters,5),collapse = ""))
  suppressWarnings(opts <- createStudy(path = pathstd, study_name = st_test, antares_version = ant_version))
  area <- "zone51"
	area2 <- "zone52"
  createArea(area)
	createArea(area2)
	suppressWarnings(opts <- setSimulationPath(opts$studyPath, simulation = "input"))
  
  hydro_ini_path <- file.path("input", "hydro", "hydro.ini")
  
  ## Both params with reservoir TRUE
  # reservoir capacity > 0
  my_params <- list("reservoir" = TRUE, "reservoir capacity" = custom_reservoir_capa)
  writeIniHydro(area = area, params = my_params, opts = opts)
  hydro_ini_data <- antaresRead::readIni(pathIni = hydro_ini_path, opts = opts)
  
  expect_equal(my_params[["reservoir"]], hydro_ini_data[["reservoir"]][[area]])
  expect_equal(my_params[["reservoir capacity"]], hydro_ini_data[["reservoir capacity"]][[area]])
  
  # reservoir capacity = 0
  my_params <- list("reservoir" = TRUE, "reservoir capacity" = 0)
  expect_error(writeIniHydro(area = area, params = my_params, opts = opts),
              regexp = "Invalid reservoir capacity")
  
  
  ## Both params with reservoir FALSE
  # reservoir capacity > 0
  my_params <- list("reservoir" = FALSE, "reservoir capacity" = custom_reservoir_capa)
  writeIniHydro(area = area, params = my_params, opts = opts)
  hydro_ini_data <- antaresRead::readIni(pathIni = hydro_ini_path, opts = opts)
  
  expect_equal(my_params[["reservoir"]], hydro_ini_data[["reservoir"]][[area]])
  expect_true(is.null(hydro_ini_data[["reservoir capacity"]][[area]]))
  
  # reservoir capacity = 0
  my_params <- list("reservoir" = FALSE, "reservoir capacity" = 0)
  writeIniHydro(area = area, params = my_params, opts = opts)
  expect_equal(my_params[["reservoir"]], hydro_ini_data[["reservoir"]][[area]])
  expect_true(is.null(hydro_ini_data[["reservoir capacity"]][[area]]))
  
  
  ## One param with reservoir TRUE
  # init FALSE (no capacity reservoir)
  my_params <- list("reservoir" = FALSE)
  writeIniHydro(area = area, params = my_params, opts = opts)
  
  my_params <- list("reservoir" = TRUE)
  expect_warning(writeIniHydro(area = area, params = my_params, opts = opts),
                 regexp = "Reservoir capacity not defined.") 
  hydro_ini_data <- antaresRead::readIni(pathIni = hydro_ini_path, opts = opts)
  
  expect_equal(my_params[["reservoir"]], hydro_ini_data[["reservoir"]][[area]])
  expect_true(is.null(hydro_ini_data[["reservoir capacity"]][[area]]))

  
  # One param with reservoir FALSE
  # init TRUE (with capacity reservoir)
  my_params <- list("reservoir" = TRUE, "reservoir capacity" = custom_reservoir_capa)
  writeIniHydro(area = area, params = my_params, opts = opts)
  
  my_params <- list("reservoir" = FALSE)
  writeIniHydro(area = area, params = my_params, opts = opts)
  hydro_ini_data <- antaresRead::readIni(pathIni = hydro_ini_path, opts = opts)
  
  expect_equal(my_params[["reservoir"]], hydro_ini_data[["reservoir"]][[area]])
  expect_true(is.null(hydro_ini_data[["reservoir capacity"]][[area]]))
  
  
  # One param with reservoir capacity > 0
  # init reservoir to TRUE (with capacity reservoir)
  my_params <- list("reservoir" = TRUE, "reservoir capacity" = custom_reservoir_capa)
  writeIniHydro(area = area, params = my_params, opts = opts)
  
  my_params <- list("reservoir capacity" = custom_reservoir_capa * 2)
  writeIniHydro(area = area, params = my_params, opts = opts)
  hydro_ini_data <- antaresRead::readIni(pathIni = hydro_ini_path, opts = opts)
  
  expect_equal(my_params[["reservoir capacity"]], hydro_ini_data[["reservoir capacity"]][[area]])
  expect_true(hydro_ini_data[["reservoir"]][[area]])
  
  my_params <- list("reservoir capacity" = 0)
  expect_error(writeIniHydro(area = area, params = my_params, opts = opts),
               regexp = "Invalid reservoir capacity")
  
  
  # One param with reservoir capacity > 0
  # init reservoir to FALSE (no capacity reservoir)
  my_params <- list("reservoir" = FALSE, "reservoir capacity" = custom_reservoir_capa)
  writeIniHydro(area = area, params = my_params, opts = opts)
  
  my_params <- list("reservoir capacity" = custom_reservoir_capa * 2)
  writeIniHydro(area = area, params = my_params, opts = opts)
  hydro_ini_data <- antaresRead::readIni(pathIni = hydro_ini_path, opts = opts)
  
  expect_true(is.null(hydro_ini_data[["reservoir capacity"]][[area]]))
  
  unlink(x = opts$studyPath, recursive = TRUE)
})


# remove temporary study
unlink(x = opts$studyPath, recursive = TRUE)

