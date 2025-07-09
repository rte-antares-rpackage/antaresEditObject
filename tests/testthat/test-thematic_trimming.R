skip()
# for all version of study
  # untar studies in temp files
setup_study_860(sourcedir860)
setup_study(studies, sourcedir)

# v710 ----
  
test_that("set thematic trimming v710", {
  # read study / load meta parameters
  antaresRead::setSimulationPath(studyPath, "input")
  
  # list of variables
  vect_select_vars <- antaresRead:::pkgEnv$thematic
  
  # setThematicTrimming() not available for version <800
  testthat::expect_error(
    setThematicTrimming(selection_variables = vect_select_vars$variable), 
    regexp = "calls is only available for study version >= v8.0"
  )
  
  # delete study
  unlink(x = studyPath, recursive = TRUE)
})
  

# v860 ----
## add variables ----
test_that("set thematic trimming version >= 800 (ADD VAR)", {
  # read study / load meta parameters
  antaresRead::setSimulationPath(study_temp_path, "input")
  
  # list of variables
  vect_select_vars <- antaresRead:::pkgEnv$thematic
  
  ##
  # set all variables
  ##
  setThematicTrimming(selection_variables = vect_select_vars$variable)
  
    # check variables names according to antares version
  opts_study_test <- simOptions()
  antares_version <- opts_study_test$antaresVersion
  filter_vars_version <- vect_select_vars[version<=antares_version,]
  
  res_read <- getThematicTrimming()
  
  # test if variables are all in output
  testthat::expect_true(all(filter_vars_version$variable%in%
                              res_read$variables))
  # test status values
  testthat::expect_equal(object = unique(res_read$status_selection), 
                         expected = "active")
  
  ##
  # set few variables
  ##
  setThematicTrimming(selection_variables = vect_select_vars$variable[1:10])
  res_read <- getThematicTrimming()
  
    # test if vars are activated
  res_read_active <- res_read[res_read$status_selection %in% "active",]
  testthat::expect_true(all(
    vect_select_vars$variable[1:10]%in%res_read_active$variables
  ))
  
    # test opts updated
  opts_study <- simOptions()
  thematic_values <- opts_study$parameters$`variables selection`
  testthat::expect_true(!is.null(thematic_values))
  testthat::expect_true(all(
    c("selected_vars_reset", "select_var +") %in% 
      names(thematic_values)
    ))
  thematic_values <- unlist(thematic_values, use.names = FALSE)
  testthat::expect_true(all(
    c("FALSE", vect_select_vars$variable[1:10])%in%
      thematic_values
    ))
  
  # set more than 50% of variables
    # Opposite case with ADD columns but write suppression columns
  nb_vars <- length(vect_select_vars$variable)
  setThematicTrimming(selection_variables = vect_select_vars$variable[1:(nb_vars-10)])
  
  res_read <- getThematicTrimming()
  
    # test if vars are activated
  res_read_active <- res_read[res_read$status_selection %in% "active",]
  testthat::expect_true(all(
    vect_select_vars$variable[1:(nb_vars-10)]%in%res_read_active$variables
  ))
  
    # test opts updated
  opts_study <- simOptions()
  thematic_values <- opts_study$parameters$`variables selection`
  testthat::expect_true(!is.null(thematic_values))
  testthat::expect_true(all(
    c("selected_vars_reset", "select_var -") %in% 
      names(thematic_values)
  ))
    # control values
  thematic_values <- unlist(thematic_values, use.names = FALSE)
  res_read_skip <- res_read[res_read$status_selection %in% "skip",]
  testthat::expect_true(all(
    c("TRUE", res_read_skip$variables)%in%
      thematic_values
  ))
 
})

## suppr variables ----
test_that("set thematic trimming version >= 800 (SUPPR VAR)", {
  # read study / load meta parameters
  antaresRead::setSimulationPath(study_temp_path, "input")
  
  # list of variables
  vect_select_vars <- antaresRead:::pkgEnv$thematic
  
  ##
  # set all variables
  ##
  setThematicTrimming(selection_variables = vect_select_vars$variable, 
                      type_select = "suppr")
  
  # read
  res_read <- getThematicTrimming()
  
  # test status values
  testthat::expect_equal(object = unique(res_read$status_selection), 
                         expected = "skip")
  
  ##
  # set few variables
  ##
  setThematicTrimming(selection_variables = vect_select_vars$variable[1:10], 
                      type_select = "suppr")
  
  # read
  res_read <- getThematicTrimming()
  
  # test if vars are activated
  res_read_active <- res_read[res_read$status_selection %in% "skip",]
  testthat::expect_true(all(
    vect_select_vars$variable[1:10]%in%res_read_active$variables
  ))
  
  # test opts updated
  opts_study <- simOptions()
  thematic_values <- opts_study$parameters$`variables selection`
  testthat::expect_true(!is.null(thematic_values))
  testthat::expect_true(all(
    c("selected_vars_reset", "select_var -") %in% 
      names(thematic_values)
  ))
  
  thematic_values <- unlist(thematic_values, use.names = FALSE)
  testthat::expect_true(all(
    c("TRUE", vect_select_vars$variable[1:10])%in%
      thematic_values
  ))
  
  # set more than 50% of variables
  # Opposite case with "suppr" columns but write "add" columns
  nb_vars <- length(vect_select_vars$variable)
  setThematicTrimming(selection_variables = vect_select_vars$variable[1:(nb_vars-10)], 
                      type_select = "suppr")
  
  res_read <- getThematicTrimming()
  
  # test if vars are skiped
  res_read_skip <- res_read[res_read$status_selection %in% "skip",]
  testthat::expect_true(all(
    vect_select_vars$variable[1:(nb_vars-10)]%in%
      res_read_skip$variables
  ))
  
  # test opts updated
  opts_study <- simOptions()
  thematic_values <- opts_study$parameters$`variables selection`
  testthat::expect_true(!is.null(thematic_values))
  testthat::expect_true(all(
    c("selected_vars_reset", "select_var +") %in% 
      names(thematic_values)
  ))
  # control values
  thematic_values <- unlist(thematic_values, use.names = FALSE)
  res_read_active <- res_read[res_read$status_selection %in% "active",]
  testthat::expect_true(all(
    c("FALSE", res_read_active$variables)%in%
      thematic_values
  ))
})

# delete study ----
unlink(x = study_temp_path, recursive = TRUE)
