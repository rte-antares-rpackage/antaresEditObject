
context("Function updateAdequacySettings")


sapply(studies, function(study) {
  
  setup_study(study, sourcedir)
  opts <- antaresRead::setSimulationPath(studyPath, "input")
  
  
  
  test_that("Update an adequacy parameter", {
    
    updateAdequacySettings(include_adq_patch = TRUE)
    updateAdequacySettings(set_to_null_ntc_from_physical_out_to_physical_in_for_first_step = FALSE)
    updateAdequacySettings(set_to_null_ntc_between_physical_out_for_first_step = FALSE)
    
    expect_true(getOption("antares")$parameters$`adequacy patch`$`include-adq-patch`)
    expect_false(getOption("antares")$parameters$`adequacy patch`$`set-to-null-ntc-from-physical-out-to-physical-in-for-first-step`)
    expect_false(getOption("antares")$parameters$`adequacy patch`$`set-to-null-ntc-between-physical-out-for-first-step`)
    
  })
  
  # remove temporary study
  unlink(x = file.path(pathstd, "test_case"), recursive = TRUE)
  
})