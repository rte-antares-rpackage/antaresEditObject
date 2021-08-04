
context("Function updateInputSettings")


sapply(studies, function(study) {
  
  setup_study(study, sourcedir)
  opts <- antaresRead::setSimulationPath(studyPath, "input")
  
  
  
  test_that("Update input settings", {
    
    updateInputSettings(import = c("hydro", "thermal"))
    
    expect_identical(getOption("antares")$parameters$input$import, c("hydro, thermal"))
    
  })
  
  test_that("Dont update input settings if renewables", {
    
    expect_warning(updateInputSettings(import = "renewables"))
    
    expect_identical(getOption("antares")$parameters$input$import, NA)
    
  })
  
  # remove temporary study
  unlink(x = file.path(pathstd, "test_case"), recursive = TRUE)
  
})


