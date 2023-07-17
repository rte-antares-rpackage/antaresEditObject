
context("Function writeInputTS")

# v710 ----
sapply(studies, function(study) {
  
  setup_study(study, sourcedir)
  opts <- antaresRead::setSimulationPath(studyPath, "input")
  
  
  test_that("Write new input time series", {
    # Classic cases ----
    
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
    
    #unknown type
    expect_error(
      writeInputTS(area = area,
                   type = "toto",
                   data = M,
                   overwrite = TRUE),
      regexp = "'arg'"
    )
    
    
    # hydroSTOR case ----
    
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
    
    #unknown type
    expect_error(
      writeInputTS(area = area,
                   type = "toto",
                   data = M_hydrostor,
                   overwrite = TRUE,
                   opts = opts),
      regexp = "'arg'"
    )
  })
  
  # remove temporary study
  unlink(x = file.path(pathstd, "test_case"), recursive = TRUE)
  
})


# v860 ----

setup_study_860(sourcedir860)

#Avoid warning related to code writed outside test_that.
suppressWarnings(opts <- antaresRead::setSimulationPath(study_temp_path, "input"))

test_that("create mingen file data v860", {
  
  #Initialize mingen data
  M_mingen = matrix(0,8760,5)
  
  
  # [management rules] for mingen data : 
    # file mod.txt (in /series) have to be same column dimension 
    # or column dimension of 1 or NULL (empty file)
  
  # check dimensions of mod.txt for every areas
  path_file_mod <- file.path(opts$inputPath, "hydro", "series", 
                             getAreas(), 
                             "mod.txt")
  
  list_dim <- lapply(path_file_mod, function(x){
    # read
    file <- fread(file = x)
    dim_file <- dim(file)[2]
  })
  
  names(list_dim) <- getAreas()
  
  ## trivial case 
    # mod.txt column dimension == 1
  area_1 <- getAreas()[list_dim==1][1]
  
  # write for an area with file mod.txt NULL or nb columns == 1
  writeInputTS(area = area_1, type = "mingen", 
               data = M_mingen , overwrite = TRUE, opts = opts)
  
  # use antaresRead to test
  read_ts_file <- readInputTS(mingen = "all", opts = opts)
  
  # tests correct reading data
    # check col name "mingen"
  testthat::expect_true("mingen" %in% names(read_ts_file))
    # check your area
  testthat::expect_true(area_1 %in% unique(read_ts_file$area))
    # check dimension data for your area
  testthat::expect_equal(dim(M_mingen)[2], max(read_ts_file[area %in% area_1, tsId]))
  
  
    # mod.txt column dimension == 0 (empty file)
  area_0 <- getAreas()[list_dim==0][1]
  
  # write for an area with file mod.txt empty columns == 0
  writeInputTS(area = area_0, type = "mingen", 
               data = M_mingen , overwrite = TRUE, opts = opts)
  
  # use antaresRead to test
  read_ts_file <- readInputTS(mingen = "all", opts = opts)
  
  # check your area
  testthat::expect_true(area_0 %in% unique(read_ts_file$area))
  
  
  ## multi columns cas for mod.txt file
    # mod.txt column dimension >= 1 
  area_mult <- getAreas()[list_dim>1][1]
  
  # write for an area with file mod.txt >1 columns
    # error case cause mod.txt dimension
  testthat::expect_error(writeInputTS(area = area_mult, type = "mingen", 
               data = M_mingen , overwrite = TRUE, opts = opts), 
               regexp = 'mingen \'data\' must be either a 8760\\*1 or 8760\\*3 matrix.')
  
  # you can write only mingen file with dimension 1 
  writeInputTS(area = area_mult, type = "mingen", 
               data = as.matrix(M_mingen[,1]) , 
               overwrite = TRUE, opts = opts)
  
  # use antaresRead to test
  read_ts_file <- readInputTS(mingen = "all", opts = opts)
  
  # check your area
  testthat::expect_true(area_mult %in% unique(read_ts_file$area))
  # check dimension data for your area
  testthat::expect_equal(1, max(read_ts_file[area %in% area_mult, tsId]))
  
  
  
  
  
  ## display warning message with type= "hydroSTOR" (minor update function v860)
  
  # Wrong format of data, here it must be either 1 or 5 columns.
  M_hydrostor <- matrix(c(rep(8, 365), rep(5.1, 365)), nrow = 365)
  
  # warning about the file format
  expect_warning(writeInputTS(area = area_1, type = "hydroSTOR", data = M_hydrostor, opts = opts),
                 regexp = "mod 'data' must be")
  
})



test_that("create mingen file data v860", {

  ant_version <- "8.6.0"
  st_test <- paste0("my_study_860",paste0(sample(letters,5),collapse = ""))
  suppressWarnings(opts <- createStudy(path = pathstd, study_name = st_test, antares_version = ant_version))
  area <- "zone51"
  createArea(area)
  opts <- setSimulationPath(opts$studyPath, simulation = "input")

  nb_hours_per_day <- 24
  nb_days_per_year <- 365
  nb_hours_per_year <- nb_hours_per_day * nb_days_per_year
  nb_days_per_month <- 31
  nb_hours_per_month <- nb_days_per_month * nb_hours_per_day

  val <- 3
  writeInputTS(area = area, data = matrix(data = rep(val, nb_days_per_year), nrow = nb_days_per_year), type = "hydroSTOR", opts = opts)
  
  mat_maxpower_true <- matrix(data = rep(c(10000, 24, 0, 24), each = 365), ncol = 4)
  writeHydroValues(area= area, type = "maxpower", data = mat_maxpower_true, opts = opts)
  # division by nb_hours_per_month + 1 => writing OK
  val_mingen_ok <- val * nb_days_per_month / (nb_hours_per_month + 1)
  writeInputTS(area = area, data = matrix(data = rep(val_mingen_ok, nb_hours_per_year), nrow = nb_hours_per_year), type = "mingen", opts = opts)

  path_mingen <- file.path(opts$inputPath, "hydro", "series", area, "mingen.txt")
  data_mingen <- antaresRead:::fread_antares(opts = opts, file = path_mingen)

  expect_equal(val_mingen_ok, as.numeric(unique(data_mingen)))

  # try to increase val_mingen
  # division by nb_hours_per_month - 1 => writing KO
  val_mingen_ko <- val * nb_days_per_month / (nb_hours_per_month - 1)

  data_mingen <- antaresRead:::fread_antares(opts = opts, file = path_mingen)

  expect_error(writeInputTS(area = area,
                            data = matrix(data = rep(val_mingen_ko, nb_hours_per_year),
                                          nrow = nb_hours_per_year),
                            type = "mingen",
                            opts = opts)
               ,regexp = "can not be updated")
  expect_equal(val_mingen_ok, as.numeric(unique(data_mingen)))
  expect_true(! val_mingen_ok == val_mingen_ko)


  # YEARLY
  writeIniHydro(area, params = list("use heuristic" = TRUE, "follow load" = TRUE, "reservoir" = TRUE), mode = "other", opts = opts)
  expect_error(writeInputTS(area = area,
                              data = matrix(data = rep(val_mingen_ko, nb_hours_per_year),
                                            nrow = nb_hours_per_year),
                              type = "mingen",
                              opts = opts)
                 ,regexp = "can not be updated")

  # WEEKLY
  writeIniHydro(area, params = list("use heuristic" = TRUE, "follow load" = FALSE), mode = "other", opts = opts)
  expect_error(writeInputTS(area = area,
                            data = matrix(data = rep(val_mingen_ko, nb_hours_per_year),
                                          nrow = nb_hours_per_year),
                            type = "mingen",
                            opts = opts)
               ,regexp = "can not be updated")

  # try to decrease val_mod
  # MONTHLY
  writeIniHydro(area, params = list("use heuristic" = TRUE, "follow load" = TRUE, "reservoir" = FALSE), mode = "other", opts = opts)
  val_mod <- 2
  expect_error(writeInputTS(area = area,
                              data = matrix(data = rep(val_mod, nb_days_per_year), nrow = nb_days_per_year),
                              type = "hydroSTOR",
                              opts = opts)
               ,regexp = "can not be updated")

  # YEARLY
  writeIniHydro(area, params = list("use heuristic" = TRUE, "follow load" = TRUE, "reservoir" = TRUE), mode = "other", opts = opts)
  expect_error(writeInputTS(area = area,
                            data = matrix(data = rep(val_mod, nb_days_per_year), nrow = nb_days_per_year),
                            type = "hydroSTOR",
                            opts = opts)
               ,regexp = "can not be updated")

  # WEEKLY
  writeIniHydro(area, params = list("use heuristic" = TRUE, "follow load" = FALSE), mode = "other", opts = opts)
  expect_error(writeInputTS(area = area,
                            data = matrix(data = rep(val_mod, nb_days_per_year), nrow = nb_days_per_year),
                            type = "hydroSTOR",
                            opts = opts)
               ,regexp = "can not be updated")


  unlink(x = opts$studyPath, recursive = TRUE)

})


