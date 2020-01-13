

context("Function writeEconomicOptions")


sapply(studies, function(study) {
  
  setup_study(study, sourcedir)
  opts <- antaresRead::setSimulationPath(studyPath)
  
  
  test_that("Error when no 'area' column", {
    expect_error(
      writeEconomicOptions(data.frame(
        V1 = c("a", "b", "c"),
        dispatchable_hydro_power = c(TRUE, FALSE, FALSE),
        spread_unsupplied_energy_cost = 1:3,
        average_spilled_energy_cost = 7:9,
        stringsAsFactors = FALSE
      )),
      regexp = "must contain an 'area' column"
    )
  })  

  
  test_that("Error when unknown area", {
    expect_error(
      writeEconomicOptions(data.frame(
        area = c("a", "b", "wqsx"),
        dispatchable_hydro_power = c(TRUE, FALSE, FALSE),
        spread_unsupplied_energy_cost = 1:3,
        average_spilled_energy_cost = 7:9,
        stringsAsFactors = FALSE
      )),
      regexp = "Unknown area"
    )
  })
  
  
  test_that("Error when dispatchable_hydro_power is not logical", {
    expect_error(
      writeEconomicOptions(data.frame(
        area = c("a", "b", "c"),
        dispatchable_hydro_power = c(8, 5, 6),
        spread_unsupplied_energy_cost = 1:3,
        average_spilled_energy_cost = 7:9,
        stringsAsFactors = FALSE
      )),
      regexp = "logical"
    )
  })
  
  
  test_that("Error when average_spilled_energy_cost is not numeric", {
    expect_error(
      writeEconomicOptions(data.frame(
        area = c("a", "b", "c"),
        dispatchable_hydro_power = c(TRUE, FALSE, FALSE),
        spread_unsupplied_energy_cost = 1:3,
        average_spilled_energy_cost = c("a", "b", "c"),
        stringsAsFactors = FALSE
      )),
      regexp = "numeric"
    )
  })
  
  
  test_that("writeEconomicOptions works", {
    
    writeEconomicOptions(data.frame(
      area = letters[1:3],
      non_dispatchable_power = c(TRUE, FALSE, TRUE),
      dispatchable_hydro_power = c(TRUE, FALSE, FALSE),
      other_dispatchable_power = c(TRUE, TRUE, TRUE),
      spread_unsupplied_energy_cost = 1:3,
      spread_spilled_energy_cost = c(20, 10, 100),
      average_unsupplied_energy_cost = c(0, 8, 5),
      average_spilled_energy_cost = 7:9,
      stringsAsFactors = FALSE
    ))
    
    optim_c <- readIniFile(file.path(opts$inputPath, "areas", "c", "optimization.ini"))
    expect_equal(optim_c$`nodal optimization`$`dispatchable-hydro-power`, FALSE)
    expect_equal(optim_c$`nodal optimization`$`spread-unsupplied-energy-cost`, 3)
    
    optim_b <- readIniFile(file.path(opts$inputPath, "areas", "b", "optimization.ini"))
    expect_equal(optim_b$`nodal optimization`$`non-dispatchable-power`, FALSE)
    expect_equal(optim_b$`nodal optimization`$`other-dispatchable-power`, TRUE)
    expect_equal(optim_b$`nodal optimization`$`spread-spilled-energy-cost`, 10)
    
    thermal_areas <- readIniFile(file.path(opts$inputPath, "thermal", "areas.ini"))
    expect_equal(thermal_areas$spilledenergycost$b, 8)
    expect_equal(thermal_areas$unserverdenergycost$c, 5)
    
  })
  
  
  # remove temporary study
  unlink(x = file.path(path, "test_case"), recursive = TRUE)
  
})


