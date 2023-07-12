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
  writeIniHydro(area = area_to_edit, params = new_data, with_check_area = TRUE, opts = opts)
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
    writeIniHydro(area = area_to_edit, params = bad_data, with_check_area = TRUE, opts = opts),
    regexp = "Parameter params must be named with the following elements:"
  )
  
  # Bad types
  bad_types <- list("leeway low" = "toto",
                    "leeway up" = "titi",
                    "reservoir" = 35
  )
  
  expect_error(
    writeIniHydro(area = area_to_edit, params = bad_types, with_check_area = TRUE, opts = opts),
    regexp = "The following parameters have a wrong type:"
  )
  
})


test_that("Write NULL hydro.ini values to ensure its behaviour", {
  
  hydro_ini_path <- file.path("input", "hydro", "hydro.ini")
  hydro_ini_data <- antaresRead::readIni(pathIni = hydro_ini_path, opts = opts)
  
  fname <- names(hydro_ini_data)[1]
  farea <- names(hydro_ini_data[[fname]])[1]
  
  writeIniHydro(area = farea, params = setNames(list(NULL), fname), with_check_area = TRUE, opts = opts)
  hydro_ini_after_edit <- antaresRead::readIni(pathIni = hydro_ini_path, opts = opts)
  
  expect_true(!is.null(hydro_ini_data[[fname]][[farea]]))
  expect_true(is.null(hydro_ini_after_edit[[fname]][[farea]]))
})


test_that("fill_empty_hydro_ini_file() : fill specific sections in hydro.ini by default values", {
  
  hydro_ini_path <- file.path("input", "hydro", "hydro.ini")
  hydro_ini_data <- antaresRead::readIni(pathIni = hydro_ini_path, opts = opts)
  
  all_areas <- unique(unlist(lapply(names(hydro_ini_data), function(n) names(hydro_ini_data[[n]]))))
  farea <- all_areas[1]
  
  writeIniHydro(farea, list("use heuristic" = NULL, "follow load" = NULL, "reservoir" = NULL), opts = opts)
  
  hydro_ini_data <- antaresRead::readIni(pathIni = hydro_ini_path, opts = opts)
  
  fill_empty_hydro_ini_file(farea, opts)
  
  hydro_ini_data_after_edit <- antaresRead::readIni(pathIni = hydro_ini_path, opts = opts)
  
  expect_true(is.null(hydro_ini_data[["use heuristic"]][[farea]]) & hydro_ini_data_after_edit[["use heuristic"]][[farea]])
  expect_true(is.null(hydro_ini_data[["follow load"]][[farea]]) & hydro_ini_data_after_edit[["follow load"]][[farea]])
  expect_true(is.null(hydro_ini_data[["reservoir"]][[farea]]) & !hydro_ini_data_after_edit[["reservoir"]][[farea]])
})


test_that("get_type_check_mingen_vs_hydrostorage() : type of control to make between mingen.txt and mod.txt", {
  
  hydro_params <- list("use heuristic" = FALSE, "follow load" = TRUE, "reservoir" = TRUE)
  expect_true(is.null(get_type_check_mingen_vs_hydrostorage(hydro_params)))

  hydro_params <- list("use heuristic" = FALSE, "follow load" = TRUE, "reservoir" = FALSE)
  expect_true(is.null(get_type_check_mingen_vs_hydrostorage(hydro_params)))

  hydro_params <- list("use heuristic" = FALSE, "follow load" = FALSE, "reservoir" = TRUE)
  expect_true(is.null(get_type_check_mingen_vs_hydrostorage(hydro_params)))

  hydro_params <- list("use heuristic" = FALSE, "follow load" = FALSE, "reservoir" = FALSE)
  expect_true(is.null(get_type_check_mingen_vs_hydrostorage(hydro_params)))
  
  hydro_params <- list("use heuristic" = TRUE, "follow load" = TRUE, "reservoir" = TRUE)
  expect_true(get_type_check_mingen_vs_hydrostorage(hydro_params)[["type"]] == "yearly")

  hydro_params <- list("use heuristic" = TRUE, "follow load" = TRUE, "reservoir" = FALSE)
  expect_true(get_type_check_mingen_vs_hydrostorage(hydro_params)[["type"]] == "monthly")

  hydro_params <- list("use heuristic" = TRUE, "follow load" = FALSE, "reservoir" = TRUE)
  expect_true(get_type_check_mingen_vs_hydrostorage(hydro_params)[["type"]] == "weekly")

  hydro_params <- list("use heuristic" = TRUE, "follow load" = FALSE, "reservoir" = FALSE)
  expect_true(get_type_check_mingen_vs_hydrostorage(hydro_params)[["type"]] == "weekly")
})


test_that("check_mingen_vs_hydro_storage() in 8.6.0 : control data consistency between mingen.txt and mod.txt", {
  
  opts <- createStudy(path = pathstd, study_name = "my_study_860", antares_version = "8.6.0")
  area <- "zone51"
  createArea(area)
  opts <- setSimulationPath(opts$studyPath, simulation = "input")
  
  nb_hours_per_year <- 8760
  nb_hours_per_day <- 24
  nb_days_per_year <- 365
  nb_days_per_week <- 7
  
  val <- 2
  values_ts1 <- rep(val, nb_hours_per_year)
  values_ts2 <- rep(val-0.1, nb_hours_per_year)
  writeInputTS(area = area, data = matrix(data = c(values_ts1,values_ts2), nrow = nb_hours_per_year), type = "mingen", opts = opts)
  
  
  mat_mod_false <- matrix(data = c(rep(nb_hours_per_day-1, nb_days_per_year)*val),
                          nrow = nb_days_per_year)
  
  mat_mod_true <- matrix(data = c(rep(nb_hours_per_day+1, nb_days_per_year)*val),
                         nrow = nb_days_per_year)
  
  
  lst_yearly <- list("use heuristic" = TRUE, "follow load" = TRUE, "reservoir" = TRUE)
  lst_monthly <- list("use heuristic" = TRUE, "follow load" = TRUE, "reservoir" = FALSE)
  lst_weekly <- list("use heuristic" = TRUE, "follow load" = FALSE, "reservoir" = TRUE)
  lst_wo_control <- list("use heuristic" = FALSE, "follow load" = FALSE, "reservoir" = TRUE)
  
  
  # YEARLY
  # yearly data -- enabled check
  writeIniHydro(area, params = lst_yearly, opts = opts)
  # inconsistent
  writeInputTS(area = area, data = mat_mod_false, type = "hydroSTOR", opts = opts)
  res_check <- check_mingen_vs_hydro_storage(area, opts)
  expect_true(!res_check$check)
  expect_true(startsWith(res_check$msg, "Data does not respect the yearly condition."))
  # consistent
  writeInputTS(area = area, data = mat_mod_true, type = "hydroSTOR", opts = opts)
  res_check <- check_mingen_vs_hydro_storage(area, opts)
  expect_true(res_check$check)
  expect_true(identical(res_check$msg,""))
  
  
  # yearly data -- disabled check
  writeIniHydro(area, params = lst_wo_control, opts = opts)
  # inconsistent
  writeInputTS(area = area, data = mat_mod_false, type = "hydroSTOR", opts = opts)
  res_check <- check_mingen_vs_hydro_storage(area, opts)
  expect_true(res_check$check)
  expect_true(identical(res_check$msg,""))
  # consistent
  writeInputTS(area = area, data = mat_mod_true, type = "hydroSTOR", opts = opts)
  res_check <- check_mingen_vs_hydro_storage(area, opts)
  expect_true(res_check$check)
  expect_true(identical(res_check$msg,""))
  
  
  # MONTHLY
  # monthly data -- enabled check
  writeIniHydro(area, params = lst_monthly, opts = opts)
  # inconsistent
  writeInputTS(area = area, data = mat_mod_false, type = "hydroSTOR", opts = opts)
  res_check <- check_mingen_vs_hydro_storage(area, opts)
  expect_true(!res_check$check)
  expect_true(startsWith(res_check$msg, "Data does not respect the monthly condition."))
  # consistent
  writeInputTS(area = area, data = mat_mod_true, type = "hydroSTOR", opts = opts)
  res_check <- check_mingen_vs_hydro_storage(area, opts)
  expect_true(res_check$check)
  expect_true(identical(res_check$msg,""))
  
  
  # monthly data -- disabled check
  writeIniHydro(area, params = lst_wo_control, opts = opts)
  # inconsistent
  writeInputTS(area = area, data = mat_mod_false, type = "hydroSTOR", opts = opts)
  res_check <- check_mingen_vs_hydro_storage(area, opts)
  expect_true(res_check$check)
  expect_true(identical(res_check$msg,""))
  # consistent
  writeInputTS(area = area, data = mat_mod_true, type = "hydroSTOR", opts = opts)
  res_check <- check_mingen_vs_hydro_storage(area, opts)
  expect_true(res_check$check)
  expect_true(identical(res_check$msg,""))
  
  
  # WEEKLY
  # weekly data -- enabled check
  mat_mod_false_weekly <- matrix(data = c(rep(nb_hours_per_day - 1, nb_days_per_week)*val, rep(nb_hours_per_day, nb_days_per_year - nb_days_per_week)*val),
                                 nrow = nb_days_per_year)
  mat_mod_true_weekly <- matrix(data = rep(nb_hours_per_day + 1, nb_days_per_year) * val,
                                nrow = nb_days_per_year)
  writeIniHydro(area, params = lst_weekly, opts = opts)
  # inconsistent
  writeInputTS(area = area, data = mat_mod_false_weekly, type = "hydroSTOR", opts = opts)
  res_check <- check_mingen_vs_hydro_storage(area, opts)
  expect_true(!res_check$check)
  expect_true(startsWith(res_check$msg, "Data does not respect the weekly condition."))
  # consistent
  writeInputTS(area = area, data = mat_mod_true_weekly, type = "hydroSTOR", opts = opts)
  res_check <- check_mingen_vs_hydro_storage(area, opts)
  expect_true(res_check$check)
  expect_true(identical(res_check$msg,""))
  
  # weekly data -- disabled check
  writeIniHydro(area, params = lst_wo_control, opts = opts)
  # inconsistent
  writeInputTS(area = area, data = mat_mod_false_weekly, type = "hydroSTOR", opts = opts)
  res_check <- check_mingen_vs_hydro_storage(area, opts)
  expect_true(res_check$check)
  expect_true(identical(res_check$msg,""))
  # consistent
  writeInputTS(area = area, data = mat_mod_true_weekly, type = "hydroSTOR", opts = opts)
  res_check <- check_mingen_vs_hydro_storage(area, opts)
  expect_true(res_check$check)
  expect_true(identical(res_check$msg,""))
  
  unlink(x = opts$studyPath, recursive = TRUE)
})


test_that("replicate_missing_ts() : control if data is replicated if 2 data.tables have not same number of tsId", {
  
  opts <- createStudy(path = pathstd, study_name = "my_study_860", antares_version = "8.6.0")
  area <- "zone51"
  createArea(area)
  opts <- setSimulationPath(opts$studyPath, simulation = "input")
  
  nb_hours_per_year <- 8760
  nb_days_per_year <- 365
  
  val <- 2
  values_ts1 <- rep(val, nb_hours_per_year)
  values_ts2 <- rep(val - 0.1, nb_hours_per_year)
  nb_rep_ts_mingen <- 4
  mat_mingen <- matrix(data = rep(c(values_ts1, values_ts2), nb_rep_ts_mingen), nrow = nb_hours_per_year)
  writeInputTS(area = area, data = mat_mingen, type = "mingen", opts = opts)
  
  mat_mod <- matrix(data = seq(1,nb_days_per_year), nrow = nb_days_per_year)
  writeInputTS(area = area, data = mat_mod, type = "hydroSTOR", opts = opts)
  
  ts_mingen <- antaresRead::readInputTS(mingen = area, opts = opts)
  ts_mod_before <- antaresRead::readInputTS(hydroStorage = area, opts = opts)
  
  ts_mod_after <- replicate_missing_ts(ts_mod_before, ts_mingen)
  
  expect_equal(max(ts_mod_after$tsId), max(ts_mingen$tsId))
  expect_equal(max(ts_mod_after$tsId), nb_rep_ts_mingen * 2)
  expect_equal(nrow(ts_mod_after), nrow(ts_mingen))
  ts_mod_after_agg <- ts_mod_after[,.N,by = tsId]
  expect_true(all(ts_mod_after_agg$N == opts$timeIdMax) & nrow(ts_mod_after_agg) == nb_rep_ts_mingen * 2)
  
  unlink(x = opts$studyPath, recursive = TRUE)
})


# remove temporary study
unlink(x = opts$studyPath, recursive = TRUE)

