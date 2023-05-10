

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

test_that("create area in 8.6.0", {
  
  tmp <- tempfile()
  suppressWarnings({
    createStudy(path = tmp, antares_version = "8.6.0")
    opts <- antaresRead::setSimulationPath(tmp)
  })
  
  expect_equal(getAreas(),character(0))
  createArea(name = "myarea")
  expect_equal(getAreas(),"myarea")
  
  unlink(tmp, recursive = TRUE)
})
