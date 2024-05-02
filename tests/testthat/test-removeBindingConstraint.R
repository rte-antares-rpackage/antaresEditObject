# v710 ----
## error function calls with bc group ----
test_that("removeBindingConstraint with name group", {
  # read / open template study
  setup_study(studies, sourcedir)
  opts <- antaresRead::setSimulationPath(studyPath, 1)
  
  # delete
  testthat::expect_error(
    removeBindingConstraint(name = "fake",  
                            group = "group_fake",
                            opts = opts), 
    regexp = "Parameter 'group' is only for Antares study version >= v8.7.0"
  )
  
  # remove temporary study
  unlink(x = studyPath, recursive = TRUE)
})

# v870 ----
# study test creation ----
# read script to generate study v8.7.0
sourcedir_last_study <- system.file("study_test_generator/generate_test_study_870.R", 
                                    package = "antaresEditObject")

# create study
source(file = sourcedir_last_study)
opts_test <- simOptions()

## Error parameters ----
test_that("removeBindingConstraint v8.7.0 error call", {
   # try to delete BC with name + group
  testthat::expect_error(
    removeBindingConstraint(name = "whereAreYou",  
                            group = "where_is_group"), 
    regexp = "You can only delete binding constraint by"
  )
})

## error unknown group ----
test_that("removeBindingConstraint v8.7.0 warning message", {
  # try to delete BC with name + group
  testthat::expect_error(
    removeBindingConstraint(group = "where_is_group"), 
    regexp = "No binding constraint with group 'where_is_group'"
  )
})

##  one name ----
test_that("removeBindingConstraint v8.7.0 by name", {
  # INIT with creation BC 
    # multi properties
  data_terms <- list("at%fr" = "1%10",
                     "at%fr" = "1%11",
                     "fr%it" = "1%-5",
                     "at.at_gas" = "1%10")
  
  name_bc <- "bc_group_multi_offset"
  name_group <- "group_test"
  
  createBindingConstraint(
    name = name_bc,
    enabled = TRUE,
    timeStep = "hourly",
    operator = "both",
    group = name_group,
    overwrite = TRUE,
    coefficients = data_terms)
  
  createBindingConstraint(
    name = "bc_test_default_group",
    enabled = TRUE,
    timeStep = "hourly",
    operator = "both",
    overwrite = TRUE,
    coefficients = data_terms)
  
  # read
  bc_names_v870 <- names(readBindingConstraints())
  
  # delete
  removeBindingConstraint(name = bc_names_v870[1])
  
  # read
  bc_in_study <- readBindingConstraints()
  
  # test
  testthat::expect_false(bc_names_v870[1] %in% 
                   names(readBindingConstraints()))
  
  # again with "both" binding constraint
  bc_names_v870 <- names(readBindingConstraints())
  
  removeBindingConstraint(name = bc_names_v870[1])
  
  bc_in_study <- readBindingConstraints()
  
  # test
  testthat::expect_false(bc_names_v870[1] %in% 
                           names(readBindingConstraints()))
})

## multi names ----
test_that("removeBindingConstraint v8.7.0 by names", {
  # INIT with creation BC 
    # multi properties
  data_terms <- list("at%fr" = "1%10",
                     "at%fr" = "1%11",
                     "fr%it" = "1%-5",
                     "at.at_gas" = "1%10")
  
  name_bc <- "bc_group_multi_offset"
  name_group <- "group_test"
  
  createBindingConstraint(
    name = name_bc,
    enabled = TRUE,
    timeStep = "hourly",
    operator = "both",
    group = name_group,
    overwrite = TRUE,
    coefficients = data_terms)
  
  createBindingConstraint(
    name = "bc_test_default_group",
    enabled = TRUE,
    timeStep = "hourly",
    operator = "both",
    overwrite = TRUE,
    coefficients = data_terms)
  
  # read
  bc_names_v870 <- names(readBindingConstraints())
  
  # delete
  name_to_delete <- bc_names_v870
  removeBindingConstraint(name_to_delete)
  
  # read
  bc_in_study <- readBindingConstraints()
  
  # test
  testthat::expect_false(all(name_to_delete %in% 
                               bc_in_study))
})


## one group ----
test_that("removeBindingConstraint v8.7.0 by group", {
  # INIT with creation BC 
  # multi properties
  data_terms <- list("at%fr" = "1%10",
                     "at%fr" = "1%11",
                     "fr%it" = "1%-5",
                     "at.at_gas" = "1%10")
  
  name_bc <- "bc_group_multi_offset"
  name_group <- "group_test"
  
  createBindingConstraint(
    name = name_bc,
    enabled = TRUE,
    timeStep = "hourly",
    operator = "both",
    group = name_group,
    overwrite = TRUE,
    coefficients = data_terms)
  
  # read
  bc <- readBindingConstraints(opts = opts_test)
  
  # delete
  group_to_delete <- bc$bc_group_multi_offset$properties$group
  
  removeBindingConstraint(group = group_to_delete)
  
  # read
  bc_in_study <- readBindingConstraints()
  
  # test
  testthat::expect_false(all(group_to_delete %in% 
                               bc_in_study))
})

## multi group ----
test_that("removeBindingConstraint v8.7.0 by group", {
  # INIT with creation BC 
    # multi properties
  data_terms <- list("at%fr" = "1%10",
                     "at%fr" = "1%11",
                     "fr%it" = "1%-5",
                     "at.at_gas" = "1%10")
  
  name_bc1 <- "bc_group_multi_offset"
  name_group1 <- "group_test"
  
  name_bc2 <- "bc_group_multi_offset2"
  name_group2 <- "group_test2"
  
  createBindingConstraint(
    name = name_bc1,
    enabled = TRUE,
    timeStep = "hourly",
    operator = "both",
    group = name_group1,
    overwrite = TRUE,
    coefficients = data_terms)
  
  createBindingConstraint(
    name = name_bc2,
    enabled = TRUE,
    timeStep = "hourly",
    operator = "both",
    group = name_group2,
    overwrite = TRUE,
    coefficients = data_terms)
  
  # read
  bc <- readBindingConstraints()
  
  # select all groups
  group_to_delete <- sapply(bc, function(x){
    x$properties$group
  })
  
  # delete all groups
  removeBindingConstraint(group = group_to_delete)
  
  # read
  bc_in_study <- readBindingConstraints()
  
  # test
  testthat::expect_false(all(group_to_delete %in% 
                               bc_in_study))
})

## remove temporary study ----
deleteStudy()
