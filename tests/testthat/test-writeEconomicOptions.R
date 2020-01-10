

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
    expect_error(
      writeEconomicOptions(data.frame(
        area = letters[1:3],
        dispatchable_hydro_power = c(TRUE, FALSE, FALSE),
        spread_unsupplied_energy_cost = 1:3,
        average_spilled_energy_cost = 7:9,
        stringsAsFactors = FALSE
      )),
      NA
    )
  })
  
  
  # remove temporary study
  unlink(x = file.path(path, "test_case"), recursive = TRUE)
  
})


