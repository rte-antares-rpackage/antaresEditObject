
context("Function updateInputSettings")


sapply(studies, function(study) {
  
  setup_study(study, sourcedir)
  opts <- antaresRead::setSimulationPath(studyPath, "input")
  
  
  
  test_that("Update input settings", {
    
    updateInputSettings(import = c("hydro", "thermal"))
    
    expect_identical(getOption("antares")$parameters$input$import, c("hydro, thermal"))
    
  })
  
  # remove temporary study
  unlink(x = file.path(pathstd, "test_case"), recursive = TRUE)
  
})


