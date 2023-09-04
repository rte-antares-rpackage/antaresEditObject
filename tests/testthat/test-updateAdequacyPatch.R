
context("Function updateAdequacySettings")


test_that("Update an adequacy parameter for an Antares version 860", {
	
	ant_version <- "8.6.0"
	st_test <- paste0("my_study_860_", paste0(sample(letters,5),collapse = ""))
	suppressWarnings(opts <- createStudy(path = pathstd, study_name = st_test, antares_version = ant_version))
	area <- "aa"
	area2 <- "zz"
	createArea(area)
	createArea(area2)
	suppressWarnings(opts <- setSimulationPath(opts$studyPath, simulation = "input"))
  
	if (opts$antaresVersion >= 850) {
    updateAdequacySettings(include_adq_patch = TRUE)
    updateAdequacySettings(set_to_null_ntc_from_physical_out_to_physical_in_for_first_step = FALSE)
    updateAdequacySettings(set_to_null_ntc_between_physical_out_for_first_step = FALSE)
    updateAdequacySettings(check_csr_cost_function = TRUE)
      
    expect_true(getOption("antares")$parameters$`adequacy patch`$`include-adq-patch`)
    expect_false(getOption("antares")$parameters$`adequacy patch`$`set-to-null-ntc-from-physical-out-to-physical-in-for-first-step`)
    expect_false(getOption("antares")$parameters$`adequacy patch`$`set-to-null-ntc-between-physical-out-for-first-step`)
    expect_true(getOption("antares")$parameters$`adequacy patch`$`check-csr-cost-function`)
  }
	
	if (opts$antaresVersion >= 860) {
    updateAdequacySettings(enable_first_step = FALSE)
	  
    expect_false(getOption("antares")$parameters$`adequacy patch`$`enable-first-step`)
  }

	unlink(x = opts$studyPath, recursive = TRUE)
})
