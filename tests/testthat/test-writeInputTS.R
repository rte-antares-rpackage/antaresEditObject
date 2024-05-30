
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


# >= 820 ----
## Alphabetical order links ----
test_that("Check if writeInputTS() writes time series link regardless alphabetical order", {

  ant_version <- "8.2.0"
  st_test <- paste0("my_study_820_", paste0(sample(letters,5),collapse = ""))
  suppressWarnings(opts <- createStudy(path = pathstd, study_name = st_test, antares_version = ant_version))
  area <- "aa"
  area2 <- "zz"
  createArea(area)
  createArea(area2)
  suppressWarnings(opts <- setSimulationPath(opts$studyPath, simulation = "input"))

  createLink(from = area, to = area2, opts = opts)
  suppressWarnings(opts <- setSimulationPath(opts$studyPath, simulation = "input"))

  path_direct_link_file <- file.path(opts$inputPath, "links", area, "capacities", paste0(area2,"_direct.txt"))
  path_indirect_link_file <- file.path(opts$inputPath, "links", area, "capacities", paste0(area2,"_indirect.txt"))

  dat_mat <- c(1,3,2,4)
  dat_mat_inv <- c(4,2,3,1)
  nb_cols <- length(dat_mat)

  # alphabetical order
  mat_multi_scen <- matrix(data = rep(dat_mat, each = 8760), ncol = nb_cols)
  writeInputTS(data = mat_multi_scen, link = paste0(area,"%",area2), type = "tsLink", opts = opts)

  expect_equal(antaresRead:::fread_antares(opts = opts,
                                           file = path_direct_link_file),
               as.data.table(mat_multi_scen[,seq(1, nb_cols/2)]))

  expect_equal(antaresRead:::fread_antares(opts = opts,
                                           file = path_indirect_link_file),
               as.data.table(mat_multi_scen[,seq((nb_cols/2)+1, nb_cols)]))

  # no alphabetical order
  mat_multi_scen_inv <- matrix(data = rep(dat_mat_inv, each = 8760), ncol = nb_cols)
  writeInputTS(data = mat_multi_scen_inv, link = paste0(area2,"%",area), type = "tsLink", opts = opts)

  expect_equal(antaresRead:::fread_antares(opts = opts,
                                           file = path_indirect_link_file),
               as.data.table(mat_multi_scen_inv[,seq(1, nb_cols/2)]))

  expect_equal(antaresRead:::fread_antares(opts = opts,
                                           file = path_direct_link_file),
               as.data.table(mat_multi_scen_inv[,seq((nb_cols/2)+1, nb_cols)]))

})


## Separator link type ----
test_that("Check if writeInputTS() writes links time series with argument link 'area1 - area2' or 'area1%area2'", {

  ant_version <- "8.2.0"
  st_test <- paste0("my_study_820_", paste0(sample(letters,5),collapse = ""))
  suppressWarnings(opts <- createStudy(path = pathstd, study_name = st_test, antares_version = ant_version))
  area <- "aa"
  area2 <- "zz"
  createArea(area)
  createArea(area2)
  suppressWarnings(opts <- setSimulationPath(opts$studyPath, simulation = "input"))

  createLink(from = area, to = area2, opts = opts)
  suppressWarnings(opts <- setSimulationPath(opts$studyPath, simulation = "input"))

  path_direct_link_file <- file.path(opts$inputPath, "links", area, "capacities", paste0(area2,"_direct.txt"))
  path_indirect_link_file <- file.path(opts$inputPath, "links", area, "capacities", paste0(area2,"_indirect.txt"))

  dat_mat_sep_1 <- c(1,3,2,4)
  nb_cols <- length(dat_mat_sep_1)
  mat_ts_sep_1 <- matrix(data = rep(dat_mat_sep_1, each = 8760), ncol = nb_cols)

  dat_mat_sep_2 <- c(5,7,6,8)
  nb_cols <- length(dat_mat_sep_2)
  mat_ts_sep_2 <- matrix(data = rep(dat_mat_sep_2, each = 8760), ncol = nb_cols)

  # link separator '%'
  writeInputTS(data = mat_ts_sep_1, link = paste0(area,"%",area2), type = "tsLink", opts = opts)

  expect_equal(antaresRead:::fread_antares(opts = opts,
                                           file = path_direct_link_file),
               as.data.table(mat_ts_sep_1[,seq(1, nb_cols/2)]))

  expect_equal(antaresRead:::fread_antares(opts = opts,
                                           file = path_indirect_link_file),
               as.data.table(mat_ts_sep_1[,seq((nb_cols/2)+1, nb_cols)]))

  # link separator ' - '
  writeInputTS(data = mat_ts_sep_2, link = paste0(area," - ",area2), type = "tsLink", opts = opts)

  expect_equal(antaresRead:::fread_antares(opts = opts,
                                           file = path_direct_link_file),
               as.data.table(mat_ts_sep_2[,seq(1, nb_cols/2)]))

  expect_equal(antaresRead:::fread_antares(opts = opts,
                                           file = path_indirect_link_file),
               as.data.table(mat_ts_sep_2[,seq((nb_cols/2)+1, nb_cols)]))

})



# >= v860 ----

## write mingen file ----
  # write mingen file depend of dimension of mod.txt file

test_that("create mingen file with one or empty column dimension of mod.txt file", {
  # create study to have mod empty and mod with one column
  createStudy(path = tempdir(), antares_version = "8.6.0")
  area <- "zone51"
  createArea(area)
  
  #Initialize mingen data
  M_mingen = matrix(0,8760,5)
  
  # [management rules] for mingen data : 
  # file mod.txt (in /series) have to be same column dimension 
  # or column dimension of 1 or NULL (empty file)
  
  opts <- simOptions()
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
  
  # mod.txt column dimension == 0 (empty file)
  area_0 <- getAreas()[list_dim==0][1]
  
  # write for an area with file mod.txt empty columns == 0
  writeInputTS(area = area_0, 
               type = "mingen", 
               data = M_mingen , 
               overwrite = TRUE, 
               opts = opts)
  
  # use antaresRead to test
  read_ts_file <- readInputTS(mingen = "all", opts = opts)
  
  # check your area
  testthat::expect_true(area_0 %in% unique(read_ts_file$area))
  
  
  # mod.txt column dimension == 1
  
  list_dim <- lapply(path_file_mod, function(x){
    # read
    file <- fread(file = x)
    dim_file <- dim(file)[2]
  })
  
  names(list_dim) <- getAreas()
  
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
  
  
  unlink(x = opts, recursive = TRUE)
})


test_that("create mingen file with multi dimension mod.txt file", {
  
  suppressWarnings(
    createStudy(path = tempdir(), 
                study_name = "write-input-ts", 
                antares_version = "8.6.0"))
  
  # default area with st cluster
  area_1 = "fr"
  area_2 = "be"
  area_3 = "ge"
  opts<-createArea(name = area_1)
  opts<-createArea(name = area_2)
  opts<-createArea(name = area_3)
  
  #Avoid warning related to code writed outside test_that.
  #suppressWarnings(opts <- antaresRead::setSimulationPath(study_temp_path, "input"))

## Check column dimension ----
test_that("create mingen file data v860", {

  #Initialize mingen data
  M_mingen = matrix(0,8760,5)
  # write TS with 3 columns for area_3 et file mod.txt
  writeInputTS(data = matrix(12,365,3), type = "hydroSTOR", area = area_3, overwrite = TRUE, opts = opts)

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
# <<<<<<< HEAD
#   
#   # PS : study v8.7.0 have only mod files with 5 columns dimension
#   
#   
#   ## multi columns cas for mod.txt file
#   # mod.txt column dimension >= 1 
#   area_mult <- getAreas()[list_dim>1][1]
#   
#   # rewrite with less columns
#   mod_data = matrix(60,365,4)
#   
#   writeInputTS(area = area_mult, 
#                type = "hydroSTOR", 
#                data = mod_data, 
#                overwrite = TRUE)
#   
#   # write for an area with file mod.txt >1 columns
#   # error case cause mod.txt dimension
#   testthat::expect_error(writeInputTS(area = area_mult, type = "mingen", 
#                                       data = M_mingen , overwrite = TRUE, opts = opts), 
#                          regexp = 'mingen \'data\' must be either a 8760\\*1 or 8760\\*4 matrix.')
#   
#   # you can write only mingen file with dimension 1 
#   writeInputTS(area = area_mult, type = "mingen", 
#                data = as.matrix(M_mingen[,1]) , 
# =======

  ## trivial case
  # mod.txt column dimension == 1
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
  area_2 <- getAreas()[list_dim==0][1]

  # write for an area with file mod.txt empty columns == 0
  writeInputTS(area = area_2, type = "mingen",
               data = M_mingen , overwrite = TRUE, opts = opts)

  # use antaresRead to test
  read_ts_file <- readInputTS(mingen = "all", opts = opts)

  # check your area
  testthat::expect_true(area_2 %in% unique(read_ts_file$area))


  ## multi columns cas for mod.txt file
  # mod.txt column dimension >= 1
 

  # write for an area with file mod.txt >1 columns
  # error case cause mod.txt dimension

  area_mult <- getAreas()[list_dim>1][1]
  testthat::expect_error(writeInputTS(area = area_mult, type = "mingen",
                                      data = matrix(0,8760,5) , overwrite = TRUE, opts = opts),
                        regexp = 'mingen \'data\' must be either a 8760\\*1 or 8760\\*3 matrix.')

  # you can write only mingen file with dimension 1
  writeInputTS(area = area_2, type = "mingen",
               data = as.matrix(M_mingen[,1]) ,
               overwrite = TRUE, opts = opts)

  # use antaresRead to test
  read_ts_file <- readInputTS(mingen = "all", opts = opts)

  # check your area
  testthat::expect_true(area_2 %in% unique(read_ts_file$area))
  # check dimension data for your area
  testthat::expect_equal(1, max(read_ts_file[area %in% area_2, tsId]))





  ## display warning message with type= "hydroSTOR" (minor update function v860)

  # Wrong format of data, here it must be either 1 or 5 columns.
  M_hydrostor <- matrix(c(rep(8, 365), rep(5.1, 365)), nrow = 365)

  # warning about the file format
  expect_warning(writeInputTS(area = getAreas()[2], 
                              type = "hydroSTOR", 
                              data = M_hydrostor, 
                              opts = opts),
                 regexp = "mod 'data' must be")
  })
  unlink(x = opts$studyPath, recursive = TRUE)
  testthat::expect_true(TRUE)
})


# Rollback to empty file ----
test_that("writeInputTS() in 8.6.0 : rollback to an empty file", {

  ant_version <- "8.6.0"
  st_test <- paste0("my_study_860_", paste0(sample(letters,5),collapse = ""))
  suppressWarnings(opts <- createStudy(path = tempdir(), study_name = st_test, antares_version = ant_version))
  area <- "zone51"
  opts <- createArea(area)
  opts <- setSimulationPath(opts$studyPath, simulation = "input")

  path_mingen <- file.path(opts$inputPath, "hydro", "series", area, "mingen.txt")
  mat_mingen <- matrix(6,8760,5)
  expect_error(writeInputTS(area = area,
                            data = mat_mingen,
                            type = "mingen",
                            opts = opts
  )
  ,regexp = "can not be updated"
  )
  expect_true(file.size(path_mingen) == 0)

  unlink(x = opts$studyPath, recursive = TRUE)
})


# Error mingen.txt vs mod.txt ----
test_that("writeInputTS() in 8.6.0 : check if there is an error when control is enabled and data is inconsistent between mingen.txt and mod.txt", {

  ant_version <- "8.6.0"
  st_test <- paste0("my_study_860_", paste0(sample(letters,5),collapse = ""))
  suppressWarnings(opts <- createStudy(path = tempdir(), study_name = st_test, antares_version = ant_version))
  area <- "zone51"
  opts <- createArea(area)
  opts <- setSimulationPath(opts$studyPath, simulation = "input")

  lst_yearly <- list("use heuristic" = TRUE, "follow load" = TRUE, "reservoir" = TRUE)
  lst_monthly <- list("use heuristic" = TRUE, "follow load" = TRUE, "reservoir" = FALSE)
  lst_weekly <- list("use heuristic" = TRUE, "follow load" = FALSE)

  nb_hours_per_day <- 24
  nb_days_per_year <- 365
  nb_hours_per_year <- nb_hours_per_day * nb_days_per_year
  # Put more than 1 ts
  nb_ts <- 5

  mat_maxpower_init <- matrix(data = rep(c(10000, 24, 0, 24), each = 365), ncol = 4)

  mat_mingen_false <- matrix(1,nb_hours_per_year,nb_ts)
  mat_mingen_true <- matrix(-1,nb_hours_per_year,nb_ts)
  mat_mingen_init <- matrix(0,nb_hours_per_year,nb_ts)

  mat_mod_false <- matrix(-1,nb_days_per_year,nb_ts)
  mat_mod_true <- matrix(1,nb_days_per_year,nb_ts)
  mat_mod_init <- matrix(0,nb_days_per_year,nb_ts)

  writeHydroValues(area= area, type = "maxpower", data = mat_maxpower_init, opts = opts)

  # YEARLY
  writeIniHydro(area, params = lst_yearly, mode = "other", opts = opts)
  # ref mod
  writeInputTS(area = area, data = mat_mod_init, type = "hydroSTOR", opts = opts)
  expect_error(writeInputTS(area = area,
                            data = mat_mingen_false,
                            type = "mingen",
                            opts = opts
  )
  ,regexp = "can not be updated"
  )
  # ref mingen
  writeInputTS(area = area, data = mat_mingen_init, type = "mingen", opts = opts)
  expect_error(writeInputTS(area = area,
                            data = mat_mod_false,
                            type = "hydroSTOR",
                            opts = opts
  )
  ,regexp = "can not be updated"
  )

  # MONTHLY
  writeIniHydro(area, params = lst_monthly, mode = "other", opts = opts)
  # ref mod
  writeInputTS(area = area, data = mat_mod_init, type = "hydroSTOR", opts = opts)
  expect_error(writeInputTS(area = area,
                            data = mat_mingen_false,
                            type = "mingen",
                            opts = opts
  )
  ,regexp = "can not be updated"
  )
  # ref mingen
  writeInputTS(area = area, data = mat_mingen_init, type = "mingen", opts = opts)
  expect_error(writeInputTS(area = area,
                            data = mat_mod_false,
                            type = "hydroSTOR",
                            opts = opts
  )
  ,regexp = "can not be updated"
  )

  # WEEKLY
  writeIniHydro(area, params = lst_weekly, mode = "other", opts = opts)
  # ref mod
  writeInputTS(area = area, data = mat_mod_init, type = "hydroSTOR", opts = opts)
  expect_error(writeInputTS(area = area,
                            data = mat_mingen_false,
                            type = "mingen",
                            opts = opts
  )
  ,regexp = "can not be updated"
  )
  # ref mingen
  writeInputTS(area = area, data = mat_mingen_init, type = "mingen", opts = opts)
  expect_error(writeInputTS(area = area,
                            data = mat_mod_false,
                            type = "hydroSTOR",
                            opts = opts
  )
  ,regexp = "can not be updated"
  )

  unlink(x = opts$studyPath, recursive = TRUE)

})


# Success mingen.txt vs mod.txt ----
test_that("writeInputTS() in 8.6.0 : check if new data is written when control is enabled and data is consistent between mingen.txt and mod.txt", {

  ant_version <- "8.6.0"
  st_test <- paste0("my_study_860_", paste0(sample(letters,5),collapse = ""))
  suppressWarnings(opts <- createStudy(path = tempdir(), study_name = st_test, antares_version = ant_version))
  area <- "zone51"
  opts <- createArea(area)
  opts <- setSimulationPath(opts$studyPath, simulation = "input")

  path_mod_file <- file.path(opts$inputPath, "hydro", "series", area, "mod.txt")
  path_mingen_file <- file.path(opts$inputPath, "hydro", "series", area, "mingen.txt")

  lst_yearly <- list("use heuristic" = TRUE, "follow load" = TRUE, "reservoir" = TRUE)
  lst_monthly <- list("use heuristic" = TRUE, "follow load" = TRUE, "reservoir" = FALSE)
  lst_weekly <- list("use heuristic" = TRUE, "follow load" = FALSE)

  nb_hours_per_day <- 24
  nb_days_per_year <- 365
  nb_hours_per_year <- nb_hours_per_day * nb_days_per_year
  # Put more than 1 ts
  nb_ts <- 5

  mat_maxpower_init <- matrix(data = rep(c(10000, 24, 0, 24), each = 365), ncol = 4)

  mat_mingen_false <- matrix(1,nb_hours_per_year,nb_ts)
  mat_mingen_true <- matrix(-1,nb_hours_per_year,nb_ts)
  mat_mingen_init <- matrix(0,nb_hours_per_year,nb_ts)

  mat_mod_false <- matrix(-1,nb_days_per_year,nb_ts)
  mat_mod_true <- matrix(1,nb_days_per_year,nb_ts)
  mat_mod_init <- matrix(0,nb_days_per_year,nb_ts)

  writeHydroValues(area= area, type = "maxpower", data = mat_maxpower_init, opts = opts)

  # YEARLY
  writeIniHydro(area, params = lst_yearly, mode = "other", opts = opts)
  # ref mod
  writeInputTS(area = area, data = mat_mod_init, type = "hydroSTOR", opts = opts)
  writeInputTS(area = area, data = mat_mingen_true, type = "mingen", opts = opts)
  expect_equal(antaresRead:::fread_antares(opts = opts,
                                           file = path_mingen_file),
               as.data.table(mat_mingen_true))
  # ref mingen
  writeInputTS(area = area, data = mat_mingen_init, type = "mingen", opts = opts)
  writeInputTS(area = area, data = mat_mod_true, type = "hydroSTOR", opts = opts)
  expect_equal(antaresRead:::fread_antares(opts = opts,
                                           file = path_mod_file),
               as.data.table(mat_mod_true))

  # MONTHLY
  writeIniHydro(area, params = lst_monthly, mode = "other", opts = opts)
  # ref mod
  writeInputTS(area = area, data = mat_mod_init, type = "hydroSTOR", opts = opts)
  writeInputTS(area = area, data = mat_mingen_true, type = "mingen", opts = opts)
  expect_equal(antaresRead:::fread_antares(opts = opts,
                                           file = path_mingen_file),
               as.data.table(mat_mingen_true))
  # ref mingen
  writeInputTS(area = area, data = mat_mingen_init, type = "mingen", opts = opts)
  writeInputTS(area = area, data = mat_mod_true, type = "hydroSTOR", opts = opts)
  expect_equal(antaresRead:::fread_antares(opts = opts,
                                           file = path_mod_file),
               as.data.table(mat_mod_true))

  # WEEKLY
  writeIniHydro(area, params = lst_weekly, mode = "other", opts = opts)
  # ref mod
  writeInputTS(area = area, data = mat_mod_init, type = "hydroSTOR", opts = opts)
  writeInputTS(area = area, data = mat_mingen_true, type = "mingen", opts = opts)
  expect_equal(antaresRead:::fread_antares(opts = opts,
                                           file = path_mingen_file),
               as.data.table(mat_mingen_true))
  # ref mingen
  writeInputTS(area = area, data = mat_mingen_init, type = "mingen", opts = opts)
  writeInputTS(area = area, data = mat_mod_true, type = "hydroSTOR", opts = opts)
  expect_equal(antaresRead:::fread_antares(opts = opts,
                                           file = path_mod_file),
               as.data.table(mat_mod_true))

  unlink(x = opts$studyPath, recursive = TRUE)

})


# Success when disabled control ----
test_that("writeInputTS() in 8.6.0 : check if new data is written when control is disabled", {

  ant_version <- "8.6.0"
  st_test <- paste0("my_study_860_", paste0(sample(letters,5),collapse = ""))
  suppressWarnings(opts <- createStudy(path = tempdir(), study_name = st_test, antares_version = ant_version))
  area <- "zone51"
  opts <- createArea(area)
  opts <- setSimulationPath(opts$studyPath, simulation = "input")

  path_mod_file <- file.path(opts$inputPath, "hydro", "series", area, "mod.txt")
  path_mingen_file <- file.path(opts$inputPath, "hydro", "series", area, "mingen.txt")

  lst_wo_control <- list("use heuristic" = FALSE)

  nb_hours_per_day <- 24
  nb_days_per_year <- 365
  nb_hours_per_year <- nb_hours_per_day * nb_days_per_year
  # Put more than 1 ts
  nb_ts <- 5

  mat_maxpower_init <- matrix(data = rep(c(10000, 24, 0, 24), each = 365), ncol = 4)

  mat_mingen_false <- matrix(1,nb_hours_per_year,nb_ts)
  mat_mingen_true <- matrix(-1,nb_hours_per_year,nb_ts)
  mat_mingen_init <- matrix(0,nb_hours_per_year,nb_ts)

  mat_mod_false <- matrix(-1,nb_days_per_year,nb_ts)
  mat_mod_true <- matrix(1,nb_days_per_year,nb_ts)
  mat_mod_init <- matrix(0,nb_days_per_year,nb_ts)

  writeIniHydro(area, params = lst_wo_control, mode = "other", opts = opts)
  writeHydroValues(area= area, type = "maxpower", data = mat_maxpower_init, opts = opts)

  # ref mod
  writeInputTS(area = area, data = mat_mod_init, type = "hydroSTOR", opts = opts)
  writeInputTS(area = area, data = mat_mingen_true, type = "mingen", opts = opts)
  expect_equal(antaresRead:::fread_antares(opts = opts,
                                           file = path_mingen_file),
               as.data.table(mat_mingen_true))
  writeInputTS(area = area, data = mat_mingen_false, type = "mingen", opts = opts)
  expect_equal(antaresRead:::fread_antares(opts = opts,
                                           file = path_mingen_file),
               as.data.table(mat_mingen_false))
  writeInputTS(area = area, data = mat_mingen_init, type = "mingen", opts = opts)
  expect_equal(antaresRead:::fread_antares(opts = opts,
                                           file = path_mingen_file),
               as.data.table(mat_mingen_init))
  # ref mingen
  writeInputTS(area = area, data = mat_mingen_init, type = "mingen", opts = opts)
  writeInputTS(area = area, data = mat_mod_true, type = "hydroSTOR", opts = opts)
  expect_equal(antaresRead:::fread_antares(opts = opts,
                                           file = path_mod_file),
               as.data.table(mat_mod_true))
  writeInputTS(area = area, data = mat_mod_false, type = "hydroSTOR", opts = opts)
  expect_equal(antaresRead:::fread_antares(opts = opts,
                                           file = path_mod_file),
               as.data.table(mat_mod_false))
  writeInputTS(area = area, data = mat_mod_init, type = "hydroSTOR", opts = opts)
  expect_equal(antaresRead:::fread_antares(opts = opts,
                                           file = path_mod_file),
               as.data.table(mat_mod_init))

  unlink(x = opts$studyPath, recursive = TRUE)
})


