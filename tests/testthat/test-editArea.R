context("Function editArea")


sapply(studies, function(study) {
  
  setup_study(study, sourcedir)
  opts <- antaresRead::setSimulationPath(studyPath, "input")
  
  expect_error(editArea("azerty", opts = opts))
  
  
  editArea("a", opts = opts)

  editArea("a", localization = c(1, 1), opts = opts)

  color_loc_ini <- readIniFile(file.path(studyPath, "input", "areas", "a", "ui.ini"))
  
  
  ##Edit position
  expect_true(color_loc_ini$layerX$`0` == 1)
  expect_true(color_loc_ini$layerY$`0` == 1)
  
  expect_true(color_loc_ini$ui$x == 1)
  expect_true(color_loc_ini$ui$y == 1)
  
  
  ## Edit color
  editArea("a", color = grDevices::rgb(230, 108, 44, max = 255), opts = opts)
  
  color_loc_ini <- readIniFile(file.path(studyPath, "input", "areas", "a", "ui.ini"))
  expect_true(color_loc_ini$layerColor$`0` == "230, 108,  44")
    
  expect_true(color_loc_ini$ui$color_r == 230)
  expect_true(color_loc_ini$ui$color_g == 108)
  expect_true(color_loc_ini$ui$color_b == 44)
  expect_true(color_loc_ini$layerColor$`0` == "230, 108,  44")
  
  # remove temporary study
  unlink(x = file.path(pathstd, "test_case"), recursive = TRUE)
  
})


# Edit spilledenergycost and unserverdenergycost ----
test_that("Edit spilledenergycost and unserverdenergycost", {
  
  ant_version <- "8.2.0"
  st_test <- paste0("my_study_820_", paste0(sample(letters,5),collapse = ""))
  suppressWarnings(opts <- createStudy(path = pathstd, study_name = st_test, antares_version = ant_version))

  nb_areas <- 5
  ids_areas <- seq(1,nb_areas)
  my_areas <- paste0("zone",ids_areas)
  lapply(my_areas, FUN = function(area){createArea(name = area, opts = simOptions())})
  
  zone_test <- "zone1"
  inputPath <- opts$inputPath
  thermal_areas_path <- file.path(inputPath, "thermal", "areas.ini")
  area_optimization_path <- file.path(inputPath, "areas", zone_test, "optimization.ini")
  
  # spilledenergycost - unserverdenergycost
  new_spilledenergycost <- 123
  new_unserverdenergycost <- 456
  editArea(name = zone_test,
           nodalOptimization = list("spilledenergycost" = new_spilledenergycost,
                                    "unserverdenergycost" = new_unserverdenergycost
                                   ),
           opts = antaresRead::simOptions()
  )
  final_content <- readIniFile(file = thermal_areas_path)
  expect_equal(final_content[["spilledenergycost"]][[zone_test]], new_spilledenergycost)
  expect_equal(final_content[["unserverdenergycost"]][[zone_test]], new_unserverdenergycost)
  
  # spilledenergycost
  new_spilledenergycost <- 789
  editArea(name = zone_test,
           nodalOptimization = list("spilledenergycost" = new_spilledenergycost),
           opts = antaresRead::simOptions()
  )
  final_content <- readIniFile(file = thermal_areas_path)
  expect_equal(final_content[["spilledenergycost"]][[zone_test]], new_spilledenergycost)
  expect_equal(final_content[["unserverdenergycost"]][[zone_test]], new_unserverdenergycost)

  # unserverdenergycost
  new_unserverdenergycost <- 695
  editArea(name = zone_test,
           nodalOptimization = list("unserverdenergycost" = new_unserverdenergycost),
           opts = antaresRead::simOptions()
  )
  final_content <- readIniFile(file = thermal_areas_path)
  expect_equal(final_content[["spilledenergycost"]][[zone_test]], new_spilledenergycost)
  expect_equal(final_content[["unserverdenergycost"]][[zone_test]], new_unserverdenergycost)
  
  # spilledenergycost - unserverdenergycost - non-dispatchable-power
  new_spilledenergycost <- 145
  new_unserverdenergycost <- 638
  new_non_dispatchable_power <- FALSE
  editArea(name = zone_test,
         nodalOptimization = list("non-dispatchable-power" = new_non_dispatchable_power,
                                  "unserverdenergycost" = new_unserverdenergycost,
                                  "spilledenergycost" = new_spilledenergycost
                                  ),
         opts = antaresRead::simOptions()
  )
  final_content_areas <- readIniFile(file = thermal_areas_path)
  final_content_optimization <- readIniFile(file = area_optimization_path)
  expect_equal(final_content_areas[["spilledenergycost"]][[zone_test]], new_spilledenergycost)
  expect_equal(final_content_areas[["unserverdenergycost"]][[zone_test]], new_unserverdenergycost)
  expect_equal(final_content_optimization[["nodal optimization"]][["non-dispatchable-power"]], new_non_dispatchable_power)
  
  # error without input/thermal/areas.ini
  unlink(x = thermal_areas_path)
  new_spilledenergycost <- 14
  expect_error(editArea(name = zone_test,
                        nodalOptimization = list("spilledenergycost" = new_spilledenergycost),
                        opts = antaresRead::simOptions()
                        ),
               regexp = "File input/thermal/areas.ini does not exist."
  )
  
  unlink(x = opts$studyPath, recursive = TRUE)
})
