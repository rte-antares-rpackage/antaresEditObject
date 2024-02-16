

# v870 ----

##  one name ----
test_that("removeBindingConstraint v8.7.0 by name", {
  # read / open template study
  setup_study_last(dir_path = sourcedir_last_study)
  opts_test <- antaresRead::setSimulationPath(study_latest_version, "input")
  
  # areas list
  antaresRead::getAreas(opts = opts_test)
  
  # read
  bc_names_v870 <- names(readBindingConstraints(opts = opts_test))
  
  # delete
  removeBindingConstraint(bc_names_v870[1], 
                          opts = opts_test)
  
  # read
  bc_in_study <- readBindingConstraints(opts = opts_test)
  
  # test
  if(is.null(bc_in_study))
    testthat::expect_null(names(bc_in_study))
  else
    testthat::expect_false(bc_names_v870[1] %in% 
                   names(readBindingConstraints(opts = opts_test)))
  
  # again with "both" binding constraint
  bc_names_v870 <- names(readBindingConstraints(opts = opts_test))
  
  removeBindingConstraint(bc_names_v870[3], 
                          opts = opts_test)
  
  bc_in_study <- readBindingConstraints(opts = opts_test)
  
  # test
  if(is.null(bc_in_study))
    testthat::expect_null(names(bc_in_study))
  else
    testthat::expect_false(bc_names_v870[3] %in% 
                             names(readBindingConstraints(opts = opts_test)))
  
  
  
  # remove temporary study
  unlink(x = study_latest_version, recursive = TRUE)
  
})

## multi names ----
test_that("removeBindingConstraint v8.7.0 by names", {
  # read / open template study
  setup_study_last(dir_path = sourcedir_last_study)
  opts_test <- antaresRead::setSimulationPath(study_latest_version, "input")
  
  # areas list
  antaresRead::getAreas(opts = opts_test)
  
  # read
  bc_names_v870 <- names(readBindingConstraints(opts = opts_test))
  
  # delete
  name_to_delete <- bc_names_v870[-1]
  removeBindingConstraint(name_to_delete, 
                          opts = opts_test)
  
  # read
  bc_in_study <- readBindingConstraints(opts = opts_test)
  
  # test
  if(is.null(bc_in_study))
    testthat::expect_null(names(bc_in_study))
  else
    testthat::expect_false(all(name_to_delete %in% 
                             names(readBindingConstraints(opts = opts_test))))
  
  # remove temporary study
  unlink(x = study_latest_version, recursive = TRUE)
})


## one group ----
test_that("removeBindingConstraint v8.7.0 by group", {
  # read / open template study
  setup_study_last(dir_path = sourcedir_last_study)
  opts_test <- antaresRead::setSimulationPath(study_latest_version, "input")
  
  # areas list
  antaresRead::getAreas(opts = opts_test)
  
  # read
  bc <- readBindingConstraints(opts = opts_test)
  
  # delete
  group_to_delete <- bc$bc_2$properties$group
  
  removeBindingConstraint(group = group_to_delete, 
                          opts = opts_test)
  
  # read
  bc_in_study <- readBindingConstraints(opts = opts_test)
  
  # test
  if(is.null(bc_in_study))
    testthat::expect_null(names(bc_in_study))
  else
    testthat::expect_false(all(group_to_delete %in% 
                                 names(readBindingConstraints(opts = opts_test))))
  # remove temporary study
  unlink(x = study_latest_version, recursive = TRUE)
})

## multi group ----
test_that("removeBindingConstraint v8.7.0 by group", {
  # read / open template study
  setup_study_last(dir_path = sourcedir_last_study)
  opts_test <- antaresRead::setSimulationPath(study_latest_version, "input")
  
  # areas list
  antaresRead::getAreas(opts = opts_test)
  
  # read
  bc <- readBindingConstraints(opts = opts_test)
  
  # select all groups
  group_to_delete <- sapply(bc, function(x){
    x$properties$group
  })
  
  # delete all groups
  removeBindingConstraint(group = group_to_delete, 
                          opts = opts_test)
  
  # read
  bc_in_study <- readBindingConstraints(opts = opts_test)
  
  # test
  if(is.null(bc_in_study))
    testthat::expect_null(names(bc_in_study))
  else
    testthat::expect_false(all(group_to_delete %in% 
                             names(readBindingConstraints(opts = opts_test))))
  # remove temporary study
  unlink(x = study_latest_version, recursive = TRUE)
})
