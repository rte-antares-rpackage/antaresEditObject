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
  suppressWarnings(writeIniHydro(area = area_to_edit, params = new_data, mode = "other", opts = opts))
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


test_that("fill_empty_hydro_ini_file() : fill specific sections in hydro.ini by default values", {
  
  hydro_ini_path <- file.path("input", "hydro", "hydro.ini")
  hydro_ini_data <- antaresRead::readIni(pathIni = hydro_ini_path, opts = opts)
  
  all_areas <- unique(unlist(lapply(names(hydro_ini_data), function(n) names(hydro_ini_data[[n]]))))
  farea <- all_areas[1]
  
  suppressWarnings(writeIniHydro(farea, list("use heuristic" = NULL, "follow load" = NULL, "reservoir" = NULL), mode = "other", opts = opts))
  
  hydro_ini_data <- antaresRead::readIni(pathIni = hydro_ini_path, opts = opts)
  
  fill_empty_hydro_ini_file(farea, opts)
  
  hydro_ini_data_after_edit <- antaresRead::readIni(pathIni = hydro_ini_path, opts = opts)
  
  expect_true(hydro_ini_data_after_edit[["use heuristic"]][[farea]])
  expect_true(hydro_ini_data_after_edit[["follow load"]][[farea]])
  expect_true(!hydro_ini_data_after_edit[["reservoir"]][[farea]])
})


test_that("get_type_check_mingen_vs_hydrostorage() : type of control to make between mingen.txt and mod.txt", {
  
  # No control
  hydro_params <- list("use heuristic" = FALSE, "follow load" = TRUE, "reservoir" = TRUE)
  expect_true(is.null(get_type_check_mingen_vs_hydrostorage(hydro_params)))
  
  hydro_params <- list("use heuristic" = FALSE, "follow load" = TRUE, "reservoir" = FALSE)
  expect_true(is.null(get_type_check_mingen_vs_hydrostorage(hydro_params)))
  
  hydro_params <- list("use heuristic" = FALSE, "follow load" = FALSE, "reservoir" = TRUE)
  expect_true(is.null(get_type_check_mingen_vs_hydrostorage(hydro_params)))
  
  hydro_params <- list("use heuristic" = FALSE, "follow load" = FALSE, "reservoir" = FALSE)
  expect_true(is.null(get_type_check_mingen_vs_hydrostorage(hydro_params)))
  
  # Control yearly
  hydro_params <- list("use heuristic" = TRUE, "follow load" = TRUE, "reservoir" = TRUE)
  expect_true(get_type_check_mingen_vs_hydrostorage(hydro_params)[["type"]] == "yearly")
  
  # Control monthly
  hydro_params <- list("use heuristic" = TRUE, "follow load" = TRUE, "reservoir" = FALSE)
  expect_true(get_type_check_mingen_vs_hydrostorage(hydro_params)[["type"]] == "monthly")
  
  # Control weekly
  hydro_params <- list("use heuristic" = TRUE, "follow load" = FALSE, "reservoir" = TRUE)
  expect_true(get_type_check_mingen_vs_hydrostorage(hydro_params)[["type"]] == "weekly")
  
  # Control weekly
  hydro_params <- list("use heuristic" = TRUE, "follow load" = FALSE, "reservoir" = FALSE)
  expect_true(get_type_check_mingen_vs_hydrostorage(hydro_params)[["type"]] == "weekly")
})


test_that("check_mingen_vs_hydro_storage() in 8.6.0 : check if the control between mingen.txt and mod.txt is ok or ko", {
  
  ant_version <- "8.6.0"
  st_test <- paste0("my_study_860_", paste0(sample(letters,5),collapse = ""))
  suppressWarnings(opts <- createStudy(path = pathstd, study_name = st_test, antares_version = ant_version))
  area <- "zone51"
  createArea(area)
  suppressWarnings(opts <- setSimulationPath(opts$studyPath, simulation = "input"))
  
  path_mod_file <- file.path(opts$inputPath, "hydro", "series", area, "mod.txt")
  path_mingen_file <- file.path(opts$inputPath, "hydro", "series", area, "mingen.txt")
  
  nb_ts <- 5
  nb_hours_per_day <- 24
  nb_days_per_year <- 365
  nb_hours_per_year <- nb_hours_per_day * nb_days_per_year
  
  mat_mod_false <- matrix(-1, nb_days_per_year, nb_ts)
  mat_mod_true <- matrix(1, nb_days_per_year, nb_ts)
  mat_mod_init <- matrix(0, nb_days_per_year, nb_ts)
  
  mat_mingen_false <- matrix(1, nb_hours_per_year, nb_ts)
  mat_mingen_true <- matrix(-1, nb_hours_per_year, nb_ts)
  mat_mingen_init <- matrix(0, nb_hours_per_year, nb_ts)
  
  lst_yearly <- list("use heuristic" = TRUE, "follow load" = TRUE, "reservoir" = TRUE)
  lst_monthly <- list("use heuristic" = TRUE, "follow load" = TRUE, "reservoir" = FALSE)
  lst_weekly <- list("use heuristic" = TRUE, "follow load" = FALSE)
  
  # YEARLY
  writeIniHydro(area, params = lst_yearly, mode = "other", opts = opts)
  # init mingen
  fwrite(
    x = as.data.table(mat_mingen_init),
    row.names = FALSE,
    col.names = FALSE,
    sep = "\t",
    file = path_mingen_file
  )
  # inconsistent mod
  fwrite(
    x = as.data.table(mat_mod_false),
    row.names = FALSE,
    col.names = FALSE,
    sep = "\t",
    file = path_mod_file
  )
  res_check <- check_mingen_vs_hydro_storage(area, opts)
  expect_true(!res_check$check)
  expect_true(startsWith(res_check$msg, "Data does not respect the yearly condition."))
  # consistent mod
  fwrite(
    x = as.data.table(mat_mod_true),
    row.names = FALSE,
    col.names = FALSE,
    sep = "\t",
    file = path_mod_file
  )
  res_check <- check_mingen_vs_hydro_storage(area, opts)
  expect_true(res_check$check)
  expect_true(identical(res_check$msg,""))
  
  # init mod
  fwrite(
    x = as.data.table(mat_mod_init),
    row.names = FALSE,
    col.names = FALSE,
    sep = "\t",
    file = path_mod_file
  )
  # inconsistent mingen
  fwrite(
    x = as.data.table(mat_mingen_false),
    row.names = FALSE,
    col.names = FALSE,
    sep = "\t",
    file = path_mingen_file
  )
  res_check <- check_mingen_vs_hydro_storage(area, opts)
  expect_true(!res_check$check)
  expect_true(startsWith(res_check$msg, "Data does not respect the yearly condition."))
  # consistent mingen
  fwrite(
    x = as.data.table(mat_mingen_true),
    row.names = FALSE,
    col.names = FALSE,
    sep = "\t",
    file = path_mingen_file
  )
  res_check <- check_mingen_vs_hydro_storage(area, opts)
  expect_true(res_check$check)
  expect_true(identical(res_check$msg,""))
  
  # MONTHLY
  writeIniHydro(area, params = lst_monthly, mode = "other", opts = opts)
  # init mingen
  fwrite(
    x = as.data.table(mat_mingen_init),
    row.names = FALSE,
    col.names = FALSE,
    sep = "\t",
    file = path_mingen_file
  )
  # inconsistent mod
  fwrite(
    x = as.data.table(mat_mod_false),
    row.names = FALSE,
    col.names = FALSE,
    sep = "\t",
    file = path_mod_file
  )
  res_check <- check_mingen_vs_hydro_storage(area, opts)
  expect_true(!res_check$check)
  expect_true(startsWith(res_check$msg, "Data does not respect the monthly condition."))
  # consistent mod
  fwrite(
    x = as.data.table(mat_mod_true),
    row.names = FALSE,
    col.names = FALSE,
    sep = "\t",
    file = path_mod_file
  )
  res_check <- check_mingen_vs_hydro_storage(area, opts)
  expect_true(res_check$check)
  expect_true(identical(res_check$msg,""))
  
  # init mod
  fwrite(
    x = as.data.table(mat_mod_init),
    row.names = FALSE,
    col.names = FALSE,
    sep = "\t",
    file = path_mod_file
  )
  # inconsistent mingen
  fwrite(
    x = as.data.table(mat_mingen_false),
    row.names = FALSE,
    col.names = FALSE,
    sep = "\t",
    file = path_mingen_file
  )
  res_check <- check_mingen_vs_hydro_storage(area, opts)
  expect_true(!res_check$check)
  expect_true(startsWith(res_check$msg, "Data does not respect the monthly condition."))
  # consistent mingen
  fwrite(
    x = as.data.table(mat_mingen_true),
    row.names = FALSE,
    col.names = FALSE,
    sep = "\t",
    file = path_mingen_file
  )
  res_check <- check_mingen_vs_hydro_storage(area, opts)
  expect_true(res_check$check)
  expect_true(identical(res_check$msg,""))
  
  # WEEKLY
  writeIniHydro(area, params = lst_weekly, mode = "other", opts = opts)
  # init mingen
  fwrite(
    x = as.data.table(mat_mingen_init),
    row.names = FALSE,
    col.names = FALSE,
    sep = "\t",
    file = path_mingen_file
  )
  # inconsistent mod
  fwrite(
    x = as.data.table(mat_mod_false),
    row.names = FALSE,
    col.names = FALSE,
    sep = "\t",
    file = path_mod_file
  )
  res_check <- check_mingen_vs_hydro_storage(area, opts)
  expect_true(!res_check$check)
  expect_true(startsWith(res_check$msg, "Data does not respect the weekly condition."))
  # consistent mod
  fwrite(
    x = as.data.table(mat_mod_true),
    row.names = FALSE,
    col.names = FALSE,
    sep = "\t",
    file = path_mod_file
  )
  res_check <- check_mingen_vs_hydro_storage(area, opts)
  expect_true(res_check$check)
  expect_true(identical(res_check$msg,""))
  
  # init mod
  fwrite(
    x = as.data.table(mat_mod_init),
    row.names = FALSE,
    col.names = FALSE,
    sep = "\t",
    file = path_mod_file
  )
  # inconsistent mingen
  fwrite(
    x = as.data.table(mat_mingen_false),
    row.names = FALSE,
    col.names = FALSE,
    sep = "\t",
    file = path_mingen_file
  )
  res_check <- check_mingen_vs_hydro_storage(area, opts)
  expect_true(!res_check$check)
  expect_true(startsWith(res_check$msg, "Data does not respect the weekly condition."))
  # consistent mingen
  fwrite(
    x = as.data.table(mat_mingen_true),
    row.names = FALSE,
    col.names = FALSE,
    sep = "\t",
    file = path_mingen_file
  )
  res_check <- check_mingen_vs_hydro_storage(area, opts)
  expect_true(res_check$check)
  expect_true(identical(res_check$msg,""))
  
  unlink(x = opts$studyPath, recursive = TRUE)
})


test_that("check_mingen_vs_hydro_storage() in 8.6.0 : check if the control is always ok between mingen.txt and mod.txt when there is no control", {
  
  ant_version <- "8.6.0"
  st_test <- paste0("my_study_860_", paste0(sample(letters,5),collapse = ""))
  suppressWarnings(opts <- createStudy(path = pathstd, study_name = st_test, antares_version = ant_version))
  area <- "zone51"
  createArea(area)
  suppressWarnings(opts <- setSimulationPath(opts$studyPath, simulation = "input"))
  
  path_mod_file <- file.path(opts$inputPath, "hydro", "series", area, "mod.txt")
  path_mingen_file <- file.path(opts$inputPath, "hydro", "series", area, "mingen.txt")
  
  nb_ts <- 5
  nb_hours_per_day <- 24
  nb_days_per_year <- 365
  nb_hours_per_year <- nb_hours_per_day * nb_days_per_year
  
  mat_mod_false <- matrix(-1, nb_days_per_year, nb_ts)
  mat_mod_true <- matrix(1, nb_days_per_year, nb_ts)
  mat_mod_init <- matrix(0, nb_days_per_year, nb_ts)
  
  mat_mingen_false <- matrix(1, nb_hours_per_year, nb_ts)
  mat_mingen_true <- matrix(-1, nb_hours_per_year, nb_ts)
  mat_mingen_init <- matrix(0, nb_hours_per_year, nb_ts)
  
  lst_wo_control <- list("use heuristic" = FALSE)
  writeIniHydro(area, params = lst_wo_control, mode = "other", opts = opts)
  
  # YEARLY
  # init mingen
  fwrite(
    x = as.data.table(mat_mingen_init),
    row.names = FALSE,
    col.names = FALSE,
    sep = "\t",
    file = path_mingen_file
  )
  # inconsistent mod
  fwrite(
    x = as.data.table(mat_mod_false),
    row.names = FALSE,
    col.names = FALSE,
    sep = "\t",
    file = path_mod_file
  )
  res_check <- check_mingen_vs_hydro_storage(area, opts)
  expect_true(res_check$check)
  expect_true(identical(res_check$msg,""))
  # consistent mod
  fwrite(
    x = as.data.table(mat_mod_true),
    row.names = FALSE,
    col.names = FALSE,
    sep = "\t",
    file = path_mod_file
  )
  res_check <- check_mingen_vs_hydro_storage(area, opts)
  expect_true(res_check$check)
  expect_true(identical(res_check$msg,""))
  
  # init mod
  fwrite(
    x = as.data.table(mat_mod_init),
    row.names = FALSE,
    col.names = FALSE,
    sep = "\t",
    file = path_mod_file
  )
  # inconsistent mingen
  fwrite(
    x = as.data.table(mat_mingen_false),
    row.names = FALSE,
    col.names = FALSE,
    sep = "\t",
    file = path_mingen_file
  )
  res_check <- check_mingen_vs_hydro_storage(area, opts)
  expect_true(res_check$check)
  expect_true(identical(res_check$msg,""))
  # consistent mingen
  fwrite(
    x = as.data.table(mat_mingen_true),
    row.names = FALSE,
    col.names = FALSE,
    sep = "\t",
    file = path_mingen_file
  )
  res_check <- check_mingen_vs_hydro_storage(area, opts)
  expect_true(res_check$check)
  expect_true(identical(res_check$msg,""))
  
  # MONTHLY
  # init mingen
  fwrite(
    x = as.data.table(mat_mingen_init),
    row.names = FALSE,
    col.names = FALSE,
    sep = "\t",
    file = path_mingen_file
  )
  # inconsistent mod
  fwrite(
    x = as.data.table(mat_mod_false),
    row.names = FALSE,
    col.names = FALSE,
    sep = "\t",
    file = path_mod_file
  )
  res_check <- check_mingen_vs_hydro_storage(area, opts)
  expect_true(res_check$check)
  expect_true(identical(res_check$msg,""))
  # consistent mod
  fwrite(
    x = as.data.table(mat_mod_true),
    row.names = FALSE,
    col.names = FALSE,
    sep = "\t",
    file = path_mod_file
  )
  res_check <- check_mingen_vs_hydro_storage(area, opts)
  expect_true(res_check$check)
  expect_true(identical(res_check$msg,""))
  
  # init mod
  fwrite(
    x = as.data.table(mat_mod_init),
    row.names = FALSE,
    col.names = FALSE,
    sep = "\t",
    file = path_mod_file
  )
  # inconsistent mingen
  fwrite(
    x = as.data.table(mat_mingen_false),
    row.names = FALSE,
    col.names = FALSE,
    sep = "\t",
    file = path_mingen_file
  )
  res_check <- check_mingen_vs_hydro_storage(area, opts)
  expect_true(res_check$check)
  expect_true(identical(res_check$msg,""))
  # consistent mingen
  fwrite(
    x = as.data.table(mat_mingen_true),
    row.names = FALSE,
    col.names = FALSE,
    sep = "\t",
    file = path_mingen_file
  )
  res_check <- check_mingen_vs_hydro_storage(area, opts)
  expect_true(res_check$check)
  expect_true(identical(res_check$msg,""))
  
  # WEEKLY
  # init mingen
  fwrite(
    x = as.data.table(mat_mingen_init),
    row.names = FALSE,
    col.names = FALSE,
    sep = "\t",
    file = path_mingen_file
  )
  # inconsistent mod
  fwrite(
    x = as.data.table(mat_mod_false),
    row.names = FALSE,
    col.names = FALSE,
    sep = "\t",
    file = path_mod_file
  )
  res_check <- check_mingen_vs_hydro_storage(area, opts)
  expect_true(res_check$check)
  expect_true(identical(res_check$msg,""))
  # consistent mod
  fwrite(
    x = as.data.table(mat_mod_true),
    row.names = FALSE,
    col.names = FALSE,
    sep = "\t",
    file = path_mod_file
  )
  res_check <- check_mingen_vs_hydro_storage(area, opts)
  expect_true(res_check$check)
  expect_true(identical(res_check$msg,""))
  
  # init mod
  fwrite(
    x = as.data.table(mat_mod_init),
    row.names = FALSE,
    col.names = FALSE,
    sep = "\t",
    file = path_mod_file
  )
  # inconsistent mingen
  fwrite(
    x = as.data.table(mat_mingen_false),
    row.names = FALSE,
    col.names = FALSE,
    sep = "\t",
    file = path_mingen_file
  )
  res_check <- check_mingen_vs_hydro_storage(area, opts)
  expect_true(res_check$check)
  expect_true(identical(res_check$msg,""))
  # consistent mingen
  fwrite(
    x = as.data.table(mat_mingen_true),
    row.names = FALSE,
    col.names = FALSE,
    sep = "\t",
    file = path_mingen_file
  )
  res_check <- check_mingen_vs_hydro_storage(area, opts)
  expect_true(res_check$check)
  expect_true(identical(res_check$msg,""))
  
  unlink(x = opts$studyPath, recursive = TRUE)
})


test_that("writeHydroValues() in 8.6.0 : check if there is an error when data is inconsistent between mingen data and maxpower data", {
  
  ant_version <- "8.6.0"
  st_test <- paste0("my_study_860_", paste0(sample(letters,5), collapse = ""))
  suppressWarnings(opts <- createStudy(path = pathstd, study_name = st_test, antares_version = ant_version))
  area <- "zone51"
  createArea(area)
  suppressWarnings(opts <- setSimulationPath(opts$studyPath, simulation = "input"))
  
  lst_hourly <- list("reservoir" = FALSE)
  
  nb_hours_per_day <- 24
  nb_days_per_year <- 365
  nb_hours_per_year <- nb_hours_per_day * nb_days_per_year
  # Put more than 1 ts
  nb_ts <- 5
  
  mat_maxpower_false <- matrix(-1, nb_days_per_year, 4)
  mat_mingen_init <- matrix(0, nb_hours_per_year, nb_ts)
  
  # HOURLY
  # hourly data -- enabled check
  writeIniHydro(area, params = lst_hourly, mode = "other", opts = opts)
  # inconsistent maxpower
  writeInputTS(data = mat_mingen_init, area = area, type = "mingen", opts= opts)
  expect_error(writeHydroValues(area = area,
                                type = "maxpower",
                                data = mat_maxpower_false,
                                overwrite = TRUE,
                                opts= opts
                                )
               ,regexp = "can not be updated"
  )
  
  unlink(x = opts$studyPath, recursive = TRUE)
})


test_that("writeHydroValues() in 8.6.0 : check if new data is written when control is enabled and data is consistent between mingen.txt and maxpower.txt", {
  
  ant_version <- "8.6.0"
  st_test <- paste0("my_study_860_", paste0(sample(letters,5),collapse = ""))
  suppressWarnings(opts <- createStudy(path = pathstd, study_name = st_test, antares_version = ant_version))
  area <- "zone51"
  createArea(area)
  suppressWarnings(opts <- setSimulationPath(opts$studyPath, simulation = "input"))
  
  path_maxpower_file <- file.path(opts$inputPath, "hydro", "common", "capacity", paste0("maxpower_", area, ".txt"))
  
  lst_hourly <- list("reservoir" = FALSE)
  
  nb_hours_per_day <- 24
  nb_days_per_year <- 365
  nb_hours_per_year <- nb_hours_per_day * nb_days_per_year
  # Put more than 1 ts
  nb_ts <- 5
  
  mat_maxpower_true <- matrix(12345, nb_days_per_year, 4)
  mat_mingen_init <- matrix(0, nb_hours_per_year, nb_ts)
  
  # HOURLY
  # hourly data -- enabled check
  writeIniHydro(area, params = lst_hourly, mode = "other", opts = opts)
  # consistent maxpower
  writeInputTS(data = mat_mingen_init, area = area, type = "mingen", opts= opts)
  writeHydroValues(area = area,
                   type = "maxpower",
                   data = mat_maxpower_true,
                   overwrite = TRUE,
                   opts= opts
  )
  
  expect_equal(antaresRead:::fread_antares(opts = opts,
                                           file = path_maxpower_file),
               as.data.table(mat_maxpower_true))
  
  unlink(x = opts$studyPath, recursive = TRUE)
})


test_that("writeHydroValues() in 8.6.0 : check if new data is written when there is no control", {
  
  ant_version <- "8.6.0"
  st_test <- paste0("my_study_860_", paste0(sample(letters,5),collapse = ""))
  suppressWarnings(opts <- createStudy(path = pathstd, study_name = st_test, antares_version = ant_version))
  area <- "zone51"
  createArea(area)
  suppressWarnings(opts <- setSimulationPath(opts$studyPath, simulation = "input"))
  
  path_maxpower_file <- file.path(opts$inputPath, "hydro", "common", "capacity", paste0("maxpower_", area, ".txt"))
  
  lst_wo_control <- list("reservoir" = TRUE)
  writeIniHydro(area, params = lst_wo_control, mode = "other", opts = opts)
  
  nb_hours_per_day <- 24
  nb_days_per_year <- 365
  nb_hours_per_year <- nb_hours_per_day * nb_days_per_year
  # Put more than 1 ts
  nb_ts <- 5
  
  mat_maxpower_true <- matrix(12345, nb_days_per_year, 4)
  mat_maxpower_false <- matrix(-12345, nb_days_per_year, 4)
  mat_mingen_init <- matrix(0, nb_hours_per_year, nb_ts)
  
  writeInputTS(data = mat_mingen_init, area = area, type = "mingen", opts= opts)
  
  # HOURLY
  # consistent maxpower
  writeHydroValues(area = area,
                   type = "maxpower",
                   data = mat_maxpower_true,
                   overwrite = TRUE,
                   opts= opts
  )
  
  expect_equal(antaresRead:::fread_antares(opts = opts,
                                           file = path_maxpower_file),
               as.data.table(mat_maxpower_true))
  
  # inconsistent maxpower
  writeHydroValues(area = area,
                   type = "maxpower",
                   data = mat_maxpower_false,
                   overwrite = TRUE,
                   opts= opts
  )
  
  expect_equal(antaresRead:::fread_antares(opts = opts,
                                           file = path_maxpower_file),
               as.data.table(mat_maxpower_false))
  
  unlink(x = opts$studyPath, recursive = TRUE)
})


test_that("replicate_missing_ts() : control if data is replicated if 2 data.tables have not same number of tsId", {
  
  ant_version <- "8.6.0"
  st_test <- paste0("my_study_860_", paste0(sample(letters,5),collapse = ""))
  suppressWarnings(opts <- createStudy(path = pathstd, study_name = st_test, antares_version = ant_version))
  area <- "zone51"
  createArea(area)
  suppressWarnings(opts <- setSimulationPath(opts$studyPath, simulation = "input"))
  
  path_mingen_file <- file.path(opts$inputPath, "hydro", "series", area, "mingen.txt")
  path_mod_file <- file.path(opts$inputPath, "hydro", "series", area, "mod.txt")
  
  nb_hours_per_day <- 24
  nb_days_per_year <- 365
  nb_hours_per_year <- nb_hours_per_day * nb_days_per_year
  
  val <- 2
  values_ts1 <- rep(val, nb_hours_per_year)
  values_ts2 <- rep(val - 0.1, nb_hours_per_year)
  nb_rep_ts_mingen <- 4
  
  mat_mod <- matrix(data = seq(1,nb_days_per_year)*5, nrow = nb_days_per_year)
  fwrite(
       x = as.data.table(mat_mod),
       row.names = FALSE,
       col.names = FALSE,
       sep = "\t",
       file = path_mod_file
  )
  
  mat_mingen <- matrix(data = rep(c(values_ts1, values_ts2), nb_rep_ts_mingen), nrow = nb_hours_per_year)
  fwrite(
    x = as.data.table(mat_mingen),
    row.names = FALSE,
    col.names = FALSE,
    sep = "\t",
    file = path_mingen_file
  )
  
  ts_mingen <- antaresRead::readInputTS(mingen = area, opts = opts)
  ts_mod_before <- antaresRead::readInputTS(hydroStorage = area, opts = opts)
  
  ts_mod_after <- replicate_missing_ts(ts_mod_before, ts_mingen)
  
  expect_equal(max(ts_mod_after$tsId), max(ts_mingen$tsId))
  expect_equal(max(ts_mod_after$tsId), nb_rep_ts_mingen * 2)
  expect_equal(nrow(ts_mod_after), nrow(ts_mingen))
  ts_mod_after_agg <- ts_mod_after[, .N, by = tsId]
  expect_true(all(ts_mod_after_agg$N == opts$timeIdMax) & nrow(ts_mod_after_agg) == nb_rep_ts_mingen * 2)
  
  unlink(x = opts$studyPath, recursive = TRUE)
})


test_that("check_mingen_vs_maxpower() in 8.6.0 : control data consistency between mingen.txt and maxpower.txt", {
  
  ant_version <- "8.6.0"
  st_test <- paste0("my_study_860_", paste0(sample(letters,5),collapse = ""))
  suppressWarnings(opts <- createStudy(path = pathstd, study_name = st_test, antares_version = ant_version))
  area <- "zone51"
  createArea(area)
  suppressWarnings(opts <- setSimulationPath(opts$studyPath, simulation = "input"))
  
  path_maxpower_file <- file.path(opts$inputPath, "hydro", "common", "capacity", paste0("maxpower_", area, ".txt"))
  path_mingen_file <- file.path(opts$inputPath, "hydro", "series", area, "mingen.txt")
  
  nb_hours_per_day <- 24
  nb_days_per_year <- 365
  nb_hours_per_year <- nb_hours_per_day * nb_days_per_year
  # Put more than 1 ts
  nb_ts <- 5
  
  lst_hourly <- list("use heuristic" = FALSE, "reservoir" = FALSE)
  lst_wo_control <- list("use heuristic" = FALSE, "reservoir" = TRUE)
  
  mat_maxpower_false <- matrix(-1, nb_days_per_year, 4)
  mat_maxpower_true <- matrix(1, nb_days_per_year, 4)
  mat_maxpower_init <- matrix(data = rep(c(0, 24, 0, 24), each = 365), ncol = 4)
  
  mat_mingen_false <- matrix(1, nb_hours_per_year, nb_ts)
  mat_mingen_true <- matrix(-1, nb_hours_per_year, nb_ts)
  mat_mingen_init <- matrix(0, nb_hours_per_year, nb_ts)
  
  # HOURLY
  # hourly data -- enabled check
  writeIniHydro(area, params = lst_hourly, mode = "other", opts = opts)
  # inconsistent maxpower
  fwrite(
    x = as.data.table(mat_maxpower_false),
    row.names = FALSE,
    col.names = FALSE,
    sep = "\t",
    file = path_maxpower_file
  )
  fwrite(
    x = as.data.table(mat_mingen_init),
    row.names = FALSE,
    col.names = FALSE,
    sep = "\t",
    file = path_mingen_file
  )
  res_check <- check_mingen_vs_maxpower(area, opts)
  expect_true(!res_check$check)
  expect_true(startsWith(res_check$msg, "Data does not respect the hourly condition."))
  # consistent maxpower
  fwrite(
    x = as.data.table(mat_maxpower_true),
    row.names = FALSE,
    col.names = FALSE,
    sep = "\t",
    file = path_maxpower_file
  )
  res_check <- check_mingen_vs_maxpower(area, opts)
  expect_true(res_check$check)
  expect_true(identical(res_check$msg,""))
  
  # inconsistent mingen
  fwrite(
    x = as.data.table(mat_maxpower_init),
    row.names = FALSE,
    col.names = FALSE,
    sep = "\t",
    file = path_maxpower_file
  )
  fwrite(
    x = as.data.table(mat_mingen_false),
    row.names = FALSE,
    col.names = FALSE,
    sep = "\t",
    file = path_mingen_file
  )
  res_check <- check_mingen_vs_maxpower(area, opts)
  expect_true(!res_check$check)
  expect_true(startsWith(res_check$msg, "Data does not respect the hourly condition."))
  # consistent mingen
  fwrite(
    x = as.data.table(mat_mingen_true),
    row.names = FALSE,
    col.names = FALSE,
    sep = "\t",
    file = path_mingen_file
  )
  res_check <- check_mingen_vs_maxpower(area, opts)
  expect_true(res_check$check)
  expect_true(identical(res_check$msg,""))
  
  # hourly data -- disabled check
  writeIniHydro(area, params = lst_wo_control, mode = "other", opts = opts)
  # inconsistent maxpower
  fwrite(
    x = as.data.table(mat_maxpower_false),
    row.names = FALSE,
    col.names = FALSE,
    sep = "\t",
    file = path_maxpower_file
  )
  fwrite(
    x = as.data.table(mat_mingen_init),
    row.names = FALSE,
    col.names = FALSE,
    sep = "\t",
    file = path_mingen_file
  )
  res_check <- check_mingen_vs_maxpower(area, opts)
  expect_true(res_check$check)
  expect_true(identical(res_check$msg,""))
  # consistent maxpower
  fwrite(
    x = as.data.table(mat_maxpower_true),
    row.names = FALSE,
    col.names = FALSE,
    sep = "\t",
    file = path_maxpower_file
  )
  res_check <- check_mingen_vs_maxpower(area, opts)
  expect_true(res_check$check)
  expect_true(identical(res_check$msg,""))
  
  # inconsistent mingen
  fwrite(
    x = as.data.table(mat_maxpower_init),
    row.names = FALSE,
    col.names = FALSE,
    sep = "\t",
    file = path_maxpower_file
  )
  fwrite(
    x = as.data.table(mat_mingen_false),
    row.names = FALSE,
    col.names = FALSE,
    sep = "\t",
    file = path_mingen_file
  )
  res_check <- check_mingen_vs_maxpower(area, opts)
  expect_true(res_check$check)
  expect_true(identical(res_check$msg,""))
  # consistent mingen
  fwrite(
    x = as.data.table(mat_mingen_true),
    row.names = FALSE,
    col.names = FALSE,
    sep = "\t",
    file = path_mingen_file
  )
  res_check <- check_mingen_vs_maxpower(area, opts)
  expect_true(res_check$check)
  expect_true(identical(res_check$msg,""))
  
  unlink(x = opts$studyPath, recursive = TRUE)
})


# remove temporary study
unlink(x = opts$studyPath, recursive = TRUE)
