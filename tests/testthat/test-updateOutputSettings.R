
context("Function updateOuputSettings")


sapply(studies, function(study) {
  
  setup_study(study, sourcedir)
  opts <- antaresRead::setSimulationPath(studyPath, "input")
  
  test_that("Update an output parameter", {
    
    # synthesis
    current_value <- getOption("antares")[["parameters"]][["output"]][["synthesis"]]
    opts <- updateOutputSettings(synthesis = !current_value, opts = opts)
    new_value <- getOption("antares")[["parameters"]][["output"]][["synthesis"]]
    
    if (current_value) {
      expect_false(new_value)
    } else {
      expect_true(new_value)
    }
    
    # storenewset
    current_value <- getOption("antares")[["parameters"]][["output"]][["storenewset"]]
    opts <- updateOutputSettings(storenewset = !current_value, opts = opts)
    new_value <- getOption("antares")[["parameters"]][["output"]][["storenewset"]]
    
    if (current_value) {
      expect_false(new_value)
    } else {
      expect_true(new_value)
    }
  
    # archives
    current_value <- getOption("antares")[["parameters"]][["output"]][["archives"]]
    opts <- updateOutputSettings(archives = c("load", "wind"), opts = opts)
    new_value <- getOption("antares")[["parameters"]][["output"]][["archives"]]
    
    expect_true(current_value != new_value)
    expect_true(new_value == .format_ini_rhs(value = c("load", "wind")))
  })
  
  # remove temporary study
  unlink(x = file.path(pathstd, "test_case"), recursive = TRUE)
  
})