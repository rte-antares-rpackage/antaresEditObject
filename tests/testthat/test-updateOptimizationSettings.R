
context("Function updateOptimizationSettings")


sapply(studies, function(study) {
  
  setup_study(study, sourcedir)
  opts <- antaresRead::setSimulationPath(studyPath, "input")
  
  
  
  
  test_that("Update an optimization parameter", {
    
    
    updateOptimizationSettings(include.hurdlecosts = "false")
    expect_false(getOption("antares")$parameters$optimization$`include-hurdlecosts`)
    
    updateOptimizationSettings(power.fluctuations = "minimize excursions")
    expect_equal(getOption("antares")$parameters$`other preferences`$`power-fluctuations`, "minimize excursions")
    
    expect_error(updateOptimizationSettings(unit.commitment.mode = "unknown"), 
                 regexp = " is not an authorized value"
                 )
    
  })
  
  # remove temporary study
  unlink(x = file.path(pathstd, "test_case"), recursive = TRUE)
  
})


test_that("solver.log parameter available only if version >= 8.8, update multiple properties and check log message if value is not authorized", {
  
  ant_version <- "8.2.0"
  st_test <- paste0("my_study_820_", paste0(sample(letters,5),collapse = ""))
  suppressWarnings(opts <- createStudy(path = pathstd, study_name = st_test, antares_version = ant_version))
  expect_error(updateOptimizationSettings(solver.log = "true"),
               regexp = "updateOptimizationSettings: solver.log parameter is only available if using Antares >= 8.8.0"
               )
  unlink(x = opts$studyPath, recursive = TRUE)
  
  ant_version <- "8.8.0"
  st_test <- paste0("my_study_880_", paste0(sample(letters,5),collapse = ""))
  suppressWarnings(opts <- createStudy(path = pathstd, study_name = st_test, antares_version = ant_version))
  updateOptimizationSettings(solver.log = "true", opts = opts)
  expect_true(getOption("antares")$parameters$optimization$`solver-log`)
  
  expect_error(updateOptimizationSettings(include.unfeasible.problem.behavior = "unauthorized-value", opts = opts),
               regexp = " is not an authorized value"
              )
  updateOptimizationSettings(include.unfeasible.problem.behavior = "error-dry", opts = opts)
  expect_equal(getOption("antares")$parameters$optimization$`include-unfeasible-problem-behavior`, "error-dry")
  
  updateOptimizationSettings(simplex.range = "day", include.tc.min.stable.power = "true", opts = opts)
  expect_equal(getOption("antares")$parameters$optimization$`simplex-range`, "day")
  expect_true(getOption("antares")$parameters$optimization$`include-tc-minstablepower`)
  
  unlink(x = opts$studyPath, recursive = TRUE)
})


test_that("Check consistency of dictionary of the optimization settings", {
  
  keys <- c(  "simplex.range", "transmission.capacities"                        
            , "include.constraints", "include.hurdlecosts"                            
            , "include.tc.min.stable.power", "include.tc.min.up.down.time"                    
            , "include.dayahead", "include.strategicreserve"                       
            , "include.spinningreserve", "include.primaryreserve"                         
            , "include.exportmps", "power.fluctuations"                             
            , "shedding.strategy", "shedding.policy"                                
            , "unit.commitment.mode", "number.of.cores.mode"                           
            , "renewable.generation.modelling", "day.ahead.reserve.management"                   
            , "solver.log", "include.exportstructure"                        
            , "include.unfeasible.problem.behavior", "hydro.heuristic.policy"                         
            , "hydro.pricing.mode", "accurate.shave.peaks.include.short.term.storage"
  )
  
  values <- sapply(keys, FUN = dicoOptimizationSettings, simplify = FALSE)
  properties <- sapply(values, "[[", "property")
  expect_false(any(sapply(properties, is.null)))
})

