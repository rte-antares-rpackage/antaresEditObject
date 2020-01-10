

context("Function writeEconomicOptions")


sapply(studies, function(study) {
  
  setup_study(study, sourcedir)
  opts <- antaresRead::setSimulationPath(studyPath)
  
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


