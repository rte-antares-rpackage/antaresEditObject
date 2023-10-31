

context("Function createArea")


sapply(studies, function(study) {
  
  setup_study(study, sourcedir)
  opts <- antaresRead::setSimulationPath(studyPath, 1)
  
  
  test_that("Cannot initialize a new area if not in 'Input' mode", {
    expect_error(createArea(name = "myarea"))
  })
  
  
  # set simulation path in mode input
  opts <- antaresRead::setSimulationPath(studyPath, 'input')
  
  
  
  
  
  test_that("Backup study/input", {
    backupStudy(what = "study", extension = ".zip")
    backupStudy(what = "input", extension = ".tar.gz")
    expect_true(file.exists(paste0(opts$studyPath, ".zip")))
    expect_true(file.exists(paste0(opts$inputPath, ".tar.gz")))
  })
  
  
  
  test_that("Initialize a new area", {
    n_before <- length(getOption("antares")$areaList)
    createArea(name = "myarea")
    n_after <- length(getOption("antares")$areaList)
    expect_equal(n_before + 1, n_after)
    expect_true("myarea" %in% getOption("antares")$areaList)
  })
  
  
  test_that("nodal optimization options are properly written", {
    createArea(
      name = "testarea",
      nodalOptimization = nodalOptimizationOptions(
        non_dispatchable_power = FALSE,
        dispatchable_hydro_power = TRUE,
        other_dispatchable_power = FALSE,
        spread_unsupplied_energy_cost = 10,
        spread_spilled_energy_cost = 3.14,
        average_unsupplied_energy_cost = 239,
        average_spilled_energy_cost = 1000
      )
    )
    
    optim_testarea <- readIniFile(file.path(opts$inputPath, "areas", "testarea", "optimization.ini"))
    expect_equal(optim_testarea$`nodal optimization`$`dispatchable-hydro-power`, TRUE)
    expect_equal(optim_testarea$`nodal optimization`$`spread-unsupplied-energy-cost`, 10)
    expect_equal(optim_testarea$`nodal optimization`$`non-dispatchable-power`, FALSE)
    expect_equal(optim_testarea$`nodal optimization`$`other-dispatchable-power`, FALSE)
    expect_equal(optim_testarea$`nodal optimization`$`spread-spilled-energy-cost`, 3.14)
    
    thermal_areas <- readIniFile(file.path(opts$inputPath, "thermal", "areas.ini"))
    expect_equal(thermal_areas$spilledenergycost$testarea, 1000)
    expect_equal(thermal_areas$unserverdenergycost$testarea, 239)

    
  })
  
  # Initialize hydro.ini values for a new area 
  test_that("Check if hydro.ini default values are initialized when creating an area", {
    
    default_hydro_params <- get_default_hydro_ini_values()
    
    hydro_ini_path <- file.path("input", "hydro", "hydro.ini")
    hydro_ini_data <- readIni(pathIni = hydro_ini_path, opts = opts)
    
    new_area <- "area_check_hydro"
    createArea(new_area)
    
    hydro_ini_data_after_area_creation <- readIni(pathIni = hydro_ini_path, opts = opts)
    
    # Check if each section is in the hydro.ini file
    name_ref <- setdiff(names(default_hydro_params), "reservoir capacity")
    name_ref <- name_ref[order(name_ref)]
    name_ <- names(hydro_ini_data_after_area_creation)
    name_ <- name_[order(name_)]
    
    expect_equal(name_, name_ref)
    
    # Check if new_area has an entry for each section
    check_by_section <- sapply(names(hydro_ini_data_after_area_creation),
                               FUN = function(name) new_area %in% names(hydro_ini_data_after_area_creation[[name]])
    )
    expect_true(all(check_by_section))
    
    # Check if the value for new_area is the default value
    value_by_section <- sapply(names(hydro_ini_data_after_area_creation),
                               FUN = function(name) hydro_ini_data_after_area_creation[[name]][[new_area]],
                               simplify = FALSE
    )
    
    for(key in name_ref){
      expect_equal(default_hydro_params[[key]], value_by_section[[key]])
    }
    
  })
  
  
  test_that("Remove an area", {
    area2remove <- "myareatoremove"
    createArea(name = area2remove)
    
    ra <- checkRemovedArea(area = area2remove)
    expect_true(length(ra$areaResiduFiles) > 0)
    expect_true(length(ra$areaResidus) > 0)
    
    removeArea(name = area2remove)
    ra <- checkRemovedArea(area = area2remove)
    expect_length(ra$areaResiduFiles, 0)
    expect_length(ra$areaResidus, 0)
  })
  
  # remove temporary study
  unlink(x = file.path(pathstd, "test_case"), recursive = TRUE)
  
})


test_that("adequacy patch options are properly written", {
  
  tmp <- tempfile()
  suppressWarnings({
    createStudy(path = tmp, antares_version = "8.3.0")
    opts <- antaresRead::setSimulationPath(tmp)
  })
  
  #activateRES(quietly = TRUE)
  createArea(
    name = "testarea_adq",
    adequacy = adequacyOptions(
      adequacy_patch_mode = "inside"
    )
  )
  
  adq_testarea <- readIniFile(file.path(opts$inputPath, "areas", "testarea_adq", "adequacy_patch.ini"))
  expect_equal(adq_testarea$`adequacy-patch`$`adequacy-patch-mode`, "inside")
  
  unlink(tmp, recursive = TRUE)
})


# v860 ----
test_that("create area / st-storage in 8.6.0", {
  
  tmp <- tempfile()
  suppressWarnings({
    temp_study_860 <- createStudy(path = tmp, antares_version = "8.6.0")
    opts <- antaresRead::setSimulationPath(tmp)
  })
  
  # check area 
  createArea(name = "myarea", 
             opts = temp_study_860)
  
  areas_created <- getAreas()
  expect_true("myarea" %in% areas_created)
  
  expect_true(dir.exists(file.path(tmp,"input","st-storage","clusters","myarea")))
  
  # check ini file
  ini_path_file <- file.path(temp_study_860$inputPath, 
                             "st-storage","clusters","myarea", "list.ini")
  expect_true(file.exists(ini_path_file))
  
  # check mingen.txt file (hydro)
  mingen_path_file <- file.path(temp_study_860$inputPath, 
                             "hydro", "series","myarea", "mingen.txt")
  expect_true(file.exists(mingen_path_file))
  
  # REMOVE TESTS
  test_that("Remove an area in 8.6.0", {
    area2remove <- "myareatoremove"
    createArea(name = area2remove, 
               opts = temp_study_860)
    
    ra <- checkRemovedArea(area = area2remove)
    expect_true(length(ra$areaResiduFiles) > 0)
    expect_true(length(ra$areaResidus) > 0)
    
    removeArea(name = area2remove)
    ra <- checkRemovedArea(area = area2remove)
    expect_length(ra$areaResiduFiles, 0)
    expect_length(ra$areaResidus, 0)
    
    expect_equal(getAreas(select = area2remove), character(0))
  })
  
  # remove temp study
  unlink(tmp, recursive = TRUE)
})


test_that("removeArea() in 8.2.0 : check that properties.ini are all there", {
  
  ## V8
  ant_version <- "8.2.0"
  st_test <- paste0("my_study_820_", paste0(sample(letters,5),collapse = ""))
  suppressWarnings(opts <- createStudy(path = pathstd, study_name = st_test, antares_version = ant_version))
  
  # prop is taken cause it is a substring included in the char properties.ini
  area <- "prop"
  area2 <- "zone51"
  area3 <- "zone52"
  
  createArea(name = area, opts = opts)
  createArea(name = area2, opts = opts)
  createArea(name = area3, opts = opts)
  
  suppressWarnings(opts <- setSimulationPath(opts$studyPath, simulation = "input"))
  
  createLink(from = area, to = area2, opts = opts)
  createLink(from = area, to = area3, opts = opts)
  createLink(from = area2, to = area3, opts = opts)
  
  opts <- setSimulationPath(opts$studyPath, simulation = "input")
  
  removeArea(name = area, opts = opts)
  
  links_path <- file.path(opts$inputPath, "links")
  dirs_links <- list.dirs(path = links_path, full.names = TRUE, recursive = FALSE)
  files_properties <- list.files(path = links_path, full.names = TRUE, recursive = TRUE, pattern = "properties.ini$")
  
  expect_true(length(dirs_links) == length(files_properties))
  expect_true(all(file.exists(file.path(dirs_links, "properties.ini"))))
  
  unlink(opts$studyPath, recursive = TRUE)
  
  ## V7
  ant_version <- "7.1.0"
  st_test <- paste0("my_study_710_", paste0(sample(letters,5),collapse = ""))
  suppressWarnings(opts <- createStudy(path = pathstd, study_name = st_test, antares_version = ant_version))
  
  # prop is taken cause it is a substring included in the char properties.ini
  area <- "prop"
  area2 <- "zone51"
  area3 <- "zone52"
  
  createArea(name = area, opts = opts)
  createArea(name = area2, opts = opts)
  createArea(name = area3, opts = opts)
  
  suppressWarnings(opts <- setSimulationPath(opts$studyPath, simulation = "input"))
  
  createLink(from = area, to = area2, opts = opts)
  createLink(from = area, to = area3, opts = opts)
  createLink(from = area2, to = area3, opts = opts)
  
  opts <- setSimulationPath(opts$studyPath, simulation = "input")
  
  removeArea(name = area, opts = opts)
  
  links_path <- file.path(opts$inputPath, "links")
  dirs_links <- list.dirs(path = links_path, full.names = TRUE, recursive = FALSE)
  files_properties <- list.files(path = links_path, full.names = TRUE, recursive = TRUE, pattern = "properties.ini$")
  
  expect_true(length(dirs_links) == length(files_properties))
  expect_true(all(file.exists(file.path(dirs_links, "properties.ini"))))
  
  unlink(opts$studyPath, recursive = TRUE)
})




