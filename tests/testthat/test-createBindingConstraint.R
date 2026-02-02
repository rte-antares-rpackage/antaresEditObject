
# v7 ----
context("Function createBindingConstraint")

# v710----
sapply(studies, function(study) {
  
  setup_study(study, sourcedir)
  opts <- antaresRead::setSimulationPath(studyPath, "input")
  
  
  test_that("Create a new binding constraint", {
    
    createBindingConstraint(
      name = "myconstraint", 
      values = matrix(data = rep(0, 8760 * 3), ncol = 3), 
      enabled = FALSE, 
      timeStep = "hourly",
      operator = "both"
    )
    
    expect_true("myconstraint" %in% names(antaresRead::readBindingConstraints()))
    
    createBindingConstraint(
      name = "myconstraint_coef_no_lower", 
      values = matrix(data = rep(0, 8760 * 3), ncol = 3), 
      enabled = FALSE,
      coefficients =  c("a.BaSe" = 0.1, "c.PeaK" = 0.3),      
      timeStep = "hourly",
      operator = "both"
    )
    
    expect_true("myconstraint_coef_no_lower" %in% names(antaresRead::readBindingConstraints()))
  })
  
  
  
  test_that("Create a new binding constraint with named values", {
    
    values_named <- data.frame(
      less = as.integer(rep(0, 365)),
      greater = as.integer(rep(0, 365)),
      equal = as.integer(rep(0, 365))
    )
    
    createBindingConstraint(
      name = "namedvalues", 
      values = values_named, 
      timeStep = "daily"
    )
    
    expect_true("namedvalues" %in% names(antaresRead::readBindingConstraints()))
    expect_equal(
      as.data.frame(rbind(values_named, matrix(rep(0, 3), ncol = 3, dimnames = list(list(), names(values_named))))), 
      as.data.frame(antaresRead::readBindingConstraints()[["namedvalues"]]$values)
    )
  })
  
  
  
  test_that("Create a new binding constraint with BAD named values", {
    
    values_named <- data.frame(
      leeess = as.integer(rep(0, 365)),
      greaaaater = as.integer(rep(0, 365)),
      equaaaal = as.integer(rep(0, 365))
    )
    expect_error(
      createBindingConstraint(
        name = "badnamedvalues", 
        values = values_named, 
        timeStep = "daily"
      )
    )
  })
  
  test_that("Bad values for timestep", {
    
    values_named <- data.frame(
      less = as.integer(rep(0, 365)),
      greater = as.integer(rep(0, 365)),
      equal = as.integer(rep(0, 365))
    )
    
    expect_error(
      createBindingConstraint(
        name = "hourlyvalue", 
        values = values_named, 
        timeStep = "hourly"
      )
    )
  })
  
  
  
  test_that("No values", {
    
    expect_silent(
      createBindingConstraint(
        name = "silent", 
        timeStep = "hourly"
      )
    )
  })
  
  
  
  test_that("Create a new binding constraint with SINGLE named value", {
    
    values_named <- data.frame(
      greater = as.integer(rep(0, 365))
    )
    expect_silent(
      createBindingConstraint(
        name = "singlenamedvalue", 
        values = values_named, 
        timeStep = "daily"
      )
    )
  })
  
  
  ## coeffs ----
  test_that("Create a new binding constraint with coefficients", {
    
    coefs <- antaresRead::readBindingConstraints()[[1]]$coefs
    
    createBindingConstraint(
      name = "coeffs",
      timeStep = "weekly", 
      values = matrix(data = rep(0, 365 * 3), ncol = 3),
      coefficients = coefs
    )
    
    expect_identical(antaresRead::readBindingConstraints()[["coeffs"]]$coefs, coefs)
  })
  
  ## multi coeffs ----
  test_that("Create new bc with multi coefficients values", {
    links_available <- getLinks()[1:3]
    names_links <- gsub(pattern = " - ", replacement = "%", x = links_available)
    
    list_coeffs_values <- list(a=1, b=2, c=3)
    names(list_coeffs_values) <- names_links
    
    createBindingConstraint(
      name = "multi_coeffs",
      timeStep = "weekly", 
      values = matrix(data = rep(0, 365 * 3), ncol = 3),
      coefficients = list_coeffs_values
    )
    
    path_bc_ini <- file.path("input", "bindingconstraints", "bindingconstraints")
    
    read_bc <- antaresRead::readIni(path_bc_ini)
    bc_to_test <- read_bc[length(read_bc)]
    
    testthat::expect_true(all(names_links %in% names(bc_to_test[[1]]))) 
  })
  
  ## multi coeffs + offset ----
  test_that("Create new bc with multi coefficients values + offset", {
    links_available <- getLinks()[1:3]
    names_links <- gsub(pattern = " - ", replacement = "%", x = links_available)
    
    list_coeffs_values <- list(a="1%8", b="2%7", c="3%9")
    names(list_coeffs_values) <- names_links
    
    createBindingConstraint(
      name = "multi_coeffs_offset",
      timeStep = "weekly", 
      values = matrix(data = rep(0, 365 * 3), ncol = 3),
      coefficients = list_coeffs_values
    )
    
    path_bc_ini <- file.path("input", "bindingconstraints", "bindingconstraints")
    
    read_bc <- antaresRead::readIni(path_bc_ini)
    bc_to_test <- read_bc[length(read_bc)]
    
    offset_values <- unlist(bc_to_test[[1]][names_links])
    
    testthat::expect_equal(offset_values, unlist(list_coeffs_values))
  })
  
  test_that("Create a new binding constraint with BAD coefficients", {
    
    expect_error(
      createBindingConstraint(
        name = "badcoeffs_links",
        timeStep = "weekly", 
        values = matrix(data = rep(0, 365 * 3), ncol = 3),
        coefficients =  c("psp in%z" = 12, "b%null" = 0, "de%fr" = 0.5)
      ), regexp = "not valid link"
    )

    expect_error(
      createBindingConstraint(
        name = "badcoeffs_clusters",
        timeStep = "weekly", 
        values = matrix(data = rep(0, 365 * 3), ncol = 3),
        coefficients =  c("a.fake_cluster" = 0.1, "b.other_cluster" = 0.2, "c.my_cluster" = 0.3)
      ), regexp = "not valid cluster"
    )
    
    # Links are controlled first
    expect_error(
      createBindingConstraint(
        name = "badcoeffs_links_clusters",
        timeStep = "weekly", 
        values = matrix(data = rep(0, 365 * 3), ncol = 3),
        coefficients =  c("a.fake_cluster" = 0.1, "b.other_cluster" = 0.2, "c.my_cluster" = 0.3, "psp in%z" = 12, "b%null" = 0, "de%fr" = 0.5)
      ), regexp = "not valid link"
    )
    
    val_cstr1 <- matrix(data = rep(22, 365 * 3), ncol = 3)
    val_cstr2 <- matrix(data = rep(33, 365 * 3), ncol = 3)
    
    lst_cstr <- list(
      list(
        name = "cstr1", 
        id = "cstr1",
        values = val_cstr1, 
        enabled = TRUE, 
        timeStep = "hourly",
        operator = "greater",
        coefficients = list("a.base" = 1),
        overwrite = TRUE
      ),
      list(
        name = "cstr2", 
        id = "cstr2",
        values = val_cstr2, 
        enabled = TRUE, 
        timeStep = "hourly",
        operator = "greater",
        coefficients = list("b.fake_nuclear" = 1), # Not a cluster
        overwrite = TRUE
      )
    )  
    
    expect_error(
      createBindingConstraintBulk(constraints = lst_cstr, opts = simOptions())
      , regexp = "not valid cluster"
    )
    
    lst_cstr <- list(
      list(
        name = "cstr1", 
        id = "cstr1",
        values = val_cstr1, 
        enabled = TRUE, 
        timeStep = "hourly",
        operator = "greater",
        coefficients = list("a%c" = 1), # Not a link
        overwrite = TRUE
      ),
      list(
        name = "cstr2", 
        id = "cstr2",
        values = val_cstr2, 
        enabled = TRUE, 
        timeStep = "hourly",
        operator = "greater",
        coefficients = list("b%c" = 1),
        overwrite = TRUE
      )
    )  
    
    expect_error(
      createBindingConstraintBulk(constraints = lst_cstr, opts = simOptions())
      , regexp = "not valid link"
    )
  })
  
  test_that("Create a new binding constraint with coefficients not ordered alphabetically", {
    
    expect_error(
      createBindingConstraint(
        name = "not_ordered",
        timeStep = "weekly", 
        values = matrix(data = rep(0, 365 * 3), ncol = 3),
        coefficients =  c("z%psp in" = 12, "fr%de" = 0)
      ), regexp = "The areas are not sorted alphabetically"
    )
    
  })
  
  test_that("Create a new binding constraint with cluster coefficients (not with %)", {
    
    coefs <- antaresRead::readBindingConstraints()[[1]]$coefs
    coefs <- c(coefs, "a.base" = 1)
    createBindingConstraint(
      name = "coeffs",
      timeStep = "weekly", 
      values = matrix(data = rep(0, 365 * 3), ncol = 3),
      coefficients = coefs, overwrite = TRUE
    )
    
    expect_identical(antaresRead::readBindingConstraints()[["coeffs"]]$coefs, coefs)
  })
  
  
  test_that("Remove a binding constraint", {
    
    removeBindingConstraint(name = "myconstraint")
    
    expect_false("myconstraint" %in% names(antaresRead::readBindingConstraints()))
  })
  
  
  
  test_that("Remove a binding constraint that doesn't exist", {
    expect_warning(removeBindingConstraint(name = "myimaginaryconstraint"))
  })
  
  ## bulk ----
  test_that("createBindingConstraintBulk v710", {
    # Prepare data for constraints 
    bindings_constraints <- lapply(
      X = seq_len(5),
      FUN = function(i) {
        # use arguments of createBindingConstraint()
        # all arguments must be provided !
        list(
          name = paste0("constraints_bulk", i), 
          id = paste0("constraints_bulk", i), 
          values = matrix(data = rep(1, 8760 * 3), ncol = 3), 
          enabled = FALSE, 
          timeStep = "hourly",
          operator = "both",
          coefficients = list("a%b" = 1),
          overwrite = TRUE
        )
      }
    )
    # create all constraints
    createBindingConstraintBulk(bindings_constraints)
    
    # tests
    testthat::expect_true("constraints_bulk1" %in% 
                            names(readBindingConstraints()))
    testthat::expect_true("constraints_bulk5" %in% 
                            names(readBindingConstraints()))
  })
  
  
  # remove temporary study
  unlink(x = file.path(pathstd, "test_case"), recursive = TRUE)
  
})


# v870 ----

# read script to generate study v8.7.0
sourcedir_last_study <- system.file("study_test_generator/generate_test_study_870.R", 
                                    package = "antaresEditObject")

# create study
source(file = sourcedir_last_study)
opts_test <- simOptions()

## Global data----
# scenarized data hourly
n <- 10
lt_data <- matrix(data = rep(1, 8760 * n), ncol = n)
gt_data <- matrix(data = rep(2, 8760 * n), ncol = n)
eq_data <- matrix(data = rep(3, 8760 * n), ncol = n)

scenar_values <- list(lt= lt_data,
                      gt= gt_data, 
                      eq= eq_data)

# daily
n <- 9
lt_data <- matrix(data = rep(1, 365 * n), ncol = n)
gt_data <- matrix(data = rep(2, 365 * n), ncol = n)
eq_data <- matrix(data = rep(3, 365 * n), ncol = n)

scenar_values_daily <- list(lt= lt_data,
                            gt= gt_data, 
                            eq= eq_data)

## ERROR CASE ----
test_that("createBindingConstraint with bad structure values object v8.7", {
  
  # less
  bad_values <- scenar_values_daily[c("eq", "gt")]
  
  testthat::expect_error(
    createBindingConstraint(
      name = "bad_values",
      values = bad_values,
      enabled = FALSE,
      timeStep = "daily",
      operator = "less",
      coefficients = list("at%fr" = 1)), 
    regexp = "you must provide a list named according your parameter"
  )

  # both
  bad_values <- scenar_values_daily["eq"]
  
  testthat::expect_error(
    createBindingConstraint(
      name = "bad_values",
      values = bad_values,
      enabled = FALSE,
      timeStep = "daily",
      operator = "both",
      coefficients = list("at%fr" = 1)), 
    regexp = "you must provide a list named according your parameter"
  )
 
})

## add default bc ----
test_that("createBindingConstraint (default group value) v8.7", {
  ### with no values ----
    # /!\/!\/!\ output .txt file has to be empty
  createBindingConstraint(
    name = "myconstraint",
    values = NULL,
    enabled = FALSE,
    timeStep = "hourly",
    operator = "both",
    coefficients = list("at%fr" = 1), 
    overwrite = TRUE)
  
  bc <- readBindingConstraints()
  
  # tests
  testthat::expect_true("myconstraint" %in% 
                          names(bc))
  testthat::expect_equal(bc$myconstraint$properties$group, 
                         "default")
  
  # for both
  operator_bc <- c("_lt", "_gt")
  path_bc <- file.path(opts_test$inputPath, "bindingconstraints")
  path_file_bc <- paste0(file.path(path_bc, "myconstraint"), 
                         operator_bc, ".txt")
  
  # read .txt (test values)
  res <- lapply(path_file_bc, 
         antaresRead:::fread_antares, 
         opts = opts_test)
  
  res <- unlist(res)
  
  # txt files are empty
  testthat::expect_equal(res, NULL)
  
  ### with values ----
  createBindingConstraint(
    name = "myconstraint2",
    values = scenar_values,
    enabled = FALSE,
    timeStep = "hourly",
    operator = "both",
    coefficients = c("at%fr" = 1))
  
  bc <- readBindingConstraints()
  
  # tests
  testthat::expect_true("myconstraint2" %in% 
                names(bc))
  testthat::expect_equal(bc$myconstraint2$properties$group, "default")
  testthat::expect_equal(dim(scenar_values$lt)[2], 
                         dim(bc$myconstraint2$values$less)[2])
  
  # for both
  operator_bc <- c("_lt", "_gt")
  path_bc <- file.path(opts_test$inputPath, "bindingconstraints")
  path_file_bc <- paste0(file.path(path_bc, "myconstraint2"), 
                         operator_bc, ".txt")
  
  # read .txt (test values)
  res <- lapply(path_file_bc, 
                antaresRead:::fread_antares, 
                opts = opts_test)
  
  # txt files (test real value)
    # test just first values cause code convert 8760 to 8784 with 0
  testthat::expect_equal(head(res[[1]]), 
                         head(data.table::as.data.table(scenar_values$lt)))
  testthat::expect_equal(head(res[[2]]), 
                         head(data.table::as.data.table(scenar_values$gt)))
  
  # for greater 
  createBindingConstraint(
    name = "myconstraint_gr8ter",
    values = scenar_values,
    enabled = FALSE,
    timeStep = "hourly",
    operator = "greater",
    coefficients = c("at%fr" = 1))
  
  bc <- readBindingConstraints()
  
  operator_bc <- c("_gt")
  path_bc <- file.path(opts_test$inputPath, "bindingconstraints")
  path_file_bc <- paste0(file.path(path_bc, "myconstraint_gr8ter"), 
                         operator_bc, ".txt")
  
  # read .txt (test values)
  res <- lapply(path_file_bc, 
                antaresRead:::fread_antares, 
                opts = opts_test)
  
  # txt files (test real value)
  # test just first values cause code convert 8760 to 8784 with 0
  testthat::expect_equal(head(res[[1]]), 
                         head(data.table::as.data.table(scenar_values$gt)))
  
  # for equal 
  createBindingConstraint(
    name = "myconstraint_equal",
    values = scenar_values,
    enabled = FALSE,
    timeStep = "hourly",
    operator = "equal",
    coefficients = c("at%fr" = 1))
  
  bc <- readBindingConstraints()
  
  operator_bc <- c("_eq")
  path_bc <- file.path(opts_test$inputPath, "bindingconstraints")
  path_file_bc <- paste0(file.path(path_bc, "myconstraint_equal"), 
                         operator_bc, ".txt")
  
  # read .txt (test values)
  res <- lapply(path_file_bc, 
                antaresRead:::fread_antares, 
                opts = opts_test)
  
  # txt files (test real value)
  # test just first values cause code convert 8760 to 8784 with 0
  testthat::expect_equal(head(res[[1]]), 
                         head(data.table::as.data.table(scenar_values$eq)))
  
  
  ### error dim ----
    # add BC with daily values (different columns dimension ERROR) 
  testthat::expect_error(
    createBindingConstraint(
      name = "myconstraint_daily",
      values = scenar_values_daily,
      enabled = FALSE,
      timeStep = "daily",
      operator = "both",
      coefficients = c("at%fr" = 1)), 
    regexp = "Put right columns dimension"
  )
  
})


## add new group ----
testthat::test_that("createBindingConstraint with new group v8.7",{
  
  # add values with the following steps
  # NULL => 1 column => >1 column => 1 column => NULL
  # error case with dimension different
  
  name_group <- "new_group"
  
  # ADD binding with NULL values
  createBindingConstraint(
    name = "bc_new_group_NULL",
    values = NULL,
    enabled = FALSE,
    timeStep = "hourly",
    operator = "greater", 
    group = name_group,
    coefficients = list("at%fr" = 1))
  
  # ADD binding with 1 col
  df_one_col <- scenar_values["lt"]
  df_one_col$lt <- df_one_col$lt[,1, drop = FALSE]
  
  createBindingConstraint(
    name = "bc_new_group_1",
    values = df_one_col,
    enabled = FALSE,
    timeStep = "hourly",
    operator = "less", 
    group = name_group,
    coefficients = c("at%fr" = 1))
  
  # ADD binding with multi cols
  df_multi_col <- scenar_values["lt"]
  df_multi_col$lt <- df_multi_col$lt[,1:3, drop = FALSE]
  
  # now, group will keep this dimension
  createBindingConstraint(
    name = "bc_new_group_multi",
    values = df_multi_col,
    enabled = FALSE,
    timeStep = "hourly",
    operator = "less", 
    group = name_group,
    coefficients = list("at%fr" = 1))
  
  # ADD binding with 1 col
  createBindingConstraint(
    name = "bc_new_group_1_bis",
    values = df_one_col,
    enabled = FALSE,
    timeStep = "hourly",
    operator = "less", 
    group = name_group,
    coefficients = list("at%fr" = 1))
  
  # ADD binding with NULL values
  createBindingConstraint(
    name = "bc_new_group_NULL_bis",
    values = NULL,
    enabled = FALSE,
    timeStep = "hourly",
    operator = "greater", 
    group = name_group,
    coefficients = list("at%fr" = 1))
  
  # ADD binding with NULL values (both case)
  createBindingConstraint(
    name = "bc_new_group_NULL_bis_both",
    values = NULL,
    enabled = FALSE,
    timeStep = "hourly",
    operator = "both", 
    group = name_group,
    coefficients = list("at%fr" = 1))
  
  # test dimension of group "new_group"
  path_bc_value_file <- file.path(opts_test$inputPath, 
                                  "bindingconstraints", 
                                  "bc_new_group_multi_lt.txt")
  
  # read value
  dim_new_group <- dim(data.table::fread(file = path_bc_value_file))
  testthat::expect_equal(3, dim_new_group[2])
  
})



## existing named group ----
  # study provide BC with group "group_test"
test_that("createBindingConstraint with existing group v8.7", {
  
  # create "group_test"
  name_group <- "group_test"
  createBindingConstraint(
    name = "bc_with_group",
    values = scenar_values,
    enabled = FALSE,
    timeStep = "hourly",
    operator = "both",
    group = name_group,
    coefficients = list("at%fr" = 1))
  
  # ADD binding constraint with bad dimension
  testthat::expect_error(
    createBindingConstraint(
      name = "bc_with_group_error",
      values = scenar_values_daily,
      enabled = FALSE,
      timeStep = "daily",
      operator = "both", 
      group = name_group,
      coefficients = list("at%fr" = 1)), 
    regexp = "Put right columns dimension"
  )
  
  n <- 10
  ts_data <- matrix(data = rep(1, 365 * n), ncol = n)
  data_ok <- list()
  data_ok$lt <- ts_data
  data_ok$gt <- ts_data
  
  # ADD binding constraint with good dimension
  createBindingConstraint(
    name = "bc_existing_group",
    values = data_ok,
    enabled = FALSE,
    timeStep = "daily",
    operator = "both", 
    group = name_group,
    coefficients = list("at%fr" = 1))
  
  bc <- readBindingConstraints(opts = opts_test)
  
  # tests
  testthat::expect_true("bc_existing_group" %in% 
                          names(bc))
  testthat::expect_equal(bc$bc_existing_group$properties$group, 
                         name_group)
  testthat::expect_equal(dim(data_ok$lt)[2], 
                         dim(bc$bc_existing_group$values$less)[2])
  
})

## multi terms properties ----
test_that("bc with multi weight + offset properties", {
  
  # multi properties
  data_terms <- list("at%fr" = "1%10",
                     "at%fr" = "1%11",
                     "fr%it" = "1%-5",
                     "at.at_gas" = "1%10")
  
  createBindingConstraint(
    name = "bc_multi_offset",
    values = scenar_values,
    enabled = TRUE,
    timeStep = "hourly",
    operator = "both",
    coefficients = data_terms)
  
  path_bc_ini <- file.path("input", "bindingconstraints", "bindingconstraints")
  
  read_bc <- antaresRead::readIni(path_bc_ini)
  bc_id <- sapply(read_bc, `[[`, "id")
  index_my_bc <- which(bc_id%in%"bc_multi_offset")
  
  # test if all terms are created
  testthat::expect_true(all(
    names(data_terms)%in%names(read_bc[[index_my_bc]])))
  
})

## bulk ----
test_that("createBindingConstraintBulk v8.7", {
  # Prepare data for constraints 
  bindings_constraints <- lapply(
    X = seq_len(10),
    FUN = function(i) {
      # use arguments of createBindingConstraint()
      # all arguments must be provided !
      list(
        name = paste0("constraints_bulk", i), 
        id = paste0("constraints_bulk", i), 
        values = scenar_values, 
        enabled = FALSE, 
        timeStep = "hourly",
        operator = "both",
        coefficients = list("at%fr" = 1),
        group= "group_bulk",
        overwrite = TRUE
      )
    }
  )
  # create all constraints
  createBindingConstraintBulk(bindings_constraints)
  
  # tests
  testthat::expect_true("constraints_bulk1" %in% 
                          names(readBindingConstraints()))
  testthat::expect_true("constraints_bulk10" %in% 
                          names(readBindingConstraints()))
  
  
  
  test_that("test bad dimension object INPUT v8.7", {
    bad_object <-  list(
      name = paste0("constraints_bulkBAD"),
      id = paste0("constraints_bulkBAD"), 
      values = scenar_values_daily, 
      enabled = FALSE, 
      timeStep = "hourly",
      operator = "both",
      coefficients = list("at%fr" = 1),
      group= "group_bulk",
      overwrite = TRUE
    )
    
    bad_object <- append(list(bad_object), bindings_constraints)
    
    expect_error(
      createBindingConstraintBulk(bad_object), 
      regexp = "Problem dimension with group"
    )
    
  })
  
})



test_that("test bad dimension object with existing object in study v8.7", {
  bad_object <-  list(
    name = paste0("constraints_bulkBAD"), 
    id = paste0("constraints_bulkBAD"),
    values = scenar_values_daily, 
    enabled = FALSE, 
    timeStep = "hourly",
    operator = "both",
    coefficients = list("at%fr" = 1),
    group= "group_bulk",
    overwrite = TRUE
  )
  
  expect_error(
    createBindingConstraintBulk(list(bad_object)), 
    regexp = "Problem dimension with group"
  )
  
})

test_that("test NULL VALUES in study v8.7", {
  BC_NULL_VALUES <-  list(
    name = paste0("constraints_bulkNULL"), 
    id = paste0("constraints_bulkNULL"),
    values = NULL, 
    enabled = FALSE, 
    timeStep = "hourly",
    operator = "both",
    coefficients = list("at%fr" = 1),
    group= "group_bulk",
    overwrite = TRUE
  )
  
  createBindingConstraintBulk(list(BC_NULL_VALUES))
  
  # tests
  testthat::expect_true("constraints_bulkNULL" %in% 
                          names(readBindingConstraints()))
  
  # read real value
  operator_bc <- c("_lt", "_gt")
  path_bc <- file.path(opts_test$inputPath, "bindingconstraints")
  path_file_bc <- paste0(file.path(path_bc, "constraints_bulkNULL"), 
                         operator_bc, ".txt")
  
  # read .txt (test values)
  res <- lapply(path_file_bc, 
                antaresRead:::fread_antares, 
                opts = opts_test)
  
  res <- unlist(res)
  
  # txt files are empty
  testthat::expect_equal(res, NULL)
  
})

test_that("test mixed VALUES in study v8.7", {
  BC_MIX_VALUES <-  list(
    list(
      name = paste0("constraints_bulkNULL"), 
      id = paste0("constraints_bulkNULL"),
      values = NULL, 
      enabled = FALSE, 
      timeStep = "hourly",
      operator = "both",
      coefficients = list("at%fr" = 1),
      group= "group_bulk",
      overwrite = TRUE
      ),
    list(
      name = paste0("constraints_bulk_value"), 
      id = paste0("constraints_bulk_value"),
      values = scenar_values, 
      enabled = FALSE, 
      timeStep = "hourly",
      operator = "greater",
      coefficients = list("at%fr" = 1),
      group= "group_bulk",
      overwrite = TRUE
      ))
  
  createBindingConstraintBulk(BC_MIX_VALUES)
  
  # tests
  testthat::expect_true(all(
    c("constraints_bulkNULL", "constraints_bulk_value") %in% 
      names(readBindingConstraints())))
  
  # read real value
    # NULL
  operator_bc <- c("_lt", "_gt")
  path_bc <- file.path(opts_test$inputPath, "bindingconstraints")
  path_file_bc <- paste0(file.path(path_bc, "constraints_bulkNULL"), 
                         operator_bc, ".txt")
  
  # read .txt (test values)
  res <- lapply(path_file_bc, 
                antaresRead:::fread_antares, 
                opts = opts_test)
  
  res <- unlist(res)
  
  # txt files are empty
  testthat::expect_equal(res, NULL)
  
    # VALUE
  operator_bc <- c("_gt")
  path_bc <- file.path(opts_test$inputPath, "bindingconstraints")
  path_file_bc <- paste0(file.path(path_bc, "constraints_bulk_value"), 
                         operator_bc, ".txt")
  
  # read .txt (test values)
  res <- lapply(path_file_bc, 
                antaresRead:::fread_antares, 
                opts = opts_test)
  
  # txt files 
  testthat::expect_equal(head(res[[1]]), 
                         head(data.table::as.data.table(scenar_values$gt)))
  
  
  
})


test_that("Control of matrix dimension is not dependent of the order in the list of the values", {

  val_cstr1 <- list("lt" = matrix(data = rep(0, 8760 * 1), ncol = 1),
                    "gt" = matrix(data = rep(555, 8760 * 3), ncol = 3),
                    "eq" = matrix(data = rep(0, 8760 * 1), ncol = 1)
                    )
  val_cstr2 <- list("lt" = matrix(data = rep(0, 8760 * 1), ncol = 1),
                    "eq" = matrix(data = rep(0, 8760 * 1), ncol = 1),
                    "gt" = matrix(data = rep(777, 8760 * 5), ncol = 5)
                    )
  lst_cstr <- list(
    list(
      name = "cstr1", 
      id = "cstr1",
      values = val_cstr1, 
      enabled = TRUE, 
      timeStep = "hourly",
      operator = "greater",
      coefficients = list("at%fr" = 1),
      group= "group_bulk_123",
      overwrite = TRUE
      ),
    list(
      name = "cstr2", 
      id = "cstr2",
      values = val_cstr2, 
      enabled = TRUE, 
      timeStep = "hourly",
      operator = "greater",
      coefficients = list("at%fr" = 1),
      group= "group_bulk_123",
      overwrite = TRUE
      )
  )
  expect_error(
    createBindingConstraintBulk(constraints = lst_cstr, opts = simOptions()), 
    regexp = "Problem dimension with group"
  )

  val_cstr1 <- list("lt" = matrix(data = rep(444, 8760 * 2), ncol = 2),
                    "gt" = matrix(data = rep(555, 8760 * 3), ncol = 3),
                    "eq" = matrix(data = rep(0, 8760 * 1), ncol = 1)
                    )
  val_cstr2 <- list("lt" = matrix(data = rep(0, 8760 * 1), ncol = 1),
                    "eq" = matrix(data = rep(0, 8760 * 1), ncol = 1),
                    "gt" = matrix(data = rep(777, 8760 * 5), ncol = 5)
                    )
  lst_cstr <- list(
    list(
      name = "cstr1", 
      id = "cstr1",
      values = val_cstr1, 
      enabled = TRUE, 
      timeStep = "hourly",
      operator = "both",
      coefficients = list("at%fr" = 1),
      group= "group_bulk_both",
      overwrite = TRUE
      ),
    list(
      name = "cstr2", 
      id = "cstr2",
      values = val_cstr2, 
      enabled = TRUE, 
      timeStep = "hourly",
      operator = "greater",
      coefficients = list("at%fr" = 1),
      group= "group_bulk_123",
      overwrite = TRUE
      )
  )
  expect_error(
    createBindingConstraintBulk(constraints = lst_cstr, opts = simOptions()), 
    regexp = "Problem dimension with group"
  )
  
  val_cstr1 <- list("gt" = NULL,
                    "lt" = matrix(data = rep(555, 8760 * 3), ncol = 3),
                    "eq" = matrix(data = rep(0, 8760 * 1), ncol = 1)
                    )
  val_cstr2 <- list("lt" = matrix(data = rep(0, 8760 * 1), ncol = 1),
                    "eq" = matrix(data = rep(0, 8760 * 1), ncol = 1),
                    "gt" = matrix(data = rep(777, 8760 * 5), ncol = 5)
                    )
  lst_cstr <- list(
    list(
      name = "cstr1", 
      id = "cstr1",
      values = val_cstr1, 
      enabled = TRUE, 
      timeStep = "hourly",
      operator = "both",
      coefficients = list("at%fr" = 1),
      group= "group_bulk_123",
      overwrite = TRUE
      ),
    list(
      name = "cstr2", 
      id = "cstr2",
      values = val_cstr2, 
      enabled = TRUE, 
      timeStep = "hourly",
      operator = "greater",
      coefficients = list("at%fr" = 1),
      group= "group_bulk_123",
      overwrite = TRUE
      )
  )
  expect_error(
    createBindingConstraintBulk(constraints = lst_cstr, opts = simOptions()), 
    regexp = "Problem dimension with group"
  )

})

# remove temporary study ----
deleteStudy()


test_that("Control that you can not create a binding constraint link/cluster with no link/cluster in your study", {
  
  suppressWarnings(opts <- createStudy(path = pathstd, study_name = "study_no_link_no_cluster", antares_version = "8.8.0"))
  
  createArea(name = "zone1", opts = simOptions())
  createArea(name = "zone2", opts = simOptions())
  createArea(name = "zone3", opts = simOptions())
  
  expect_error(
    createBindingConstraint(name = "myconstraint_link", 
                            enabled = TRUE, 
                            timeStep = "hourly",
                            operator = "less",
                            coefficients = list("area1%area2" = 1, "area1%area3" = 2),
                            opts = simOptions()
                            ),
    regexp = "You are trying to create a binding constraint with a link coefficient but you have no link in your study."                                  
  )

  expect_error(
    createBindingConstraint(name = "myconstraint_cluster", 
                            enabled = TRUE, 
                            timeStep = "hourly",
                            operator = "less",
                            coefficients = list("area1.cluster1" = 1, "area2.cluster2" = 2),
                            opts = simOptions()
                            ),
    regexp = "You are trying to create a binding constraint with a cluster coefficient but you have no cluster in your study."                                  
  )
  
  val_cstr1 <- list("lt" = matrix(data = rep(0, 8760 * 1), ncol = 1),
                    "gt" = matrix(data = rep(555, 8760 * 1), ncol = 1),
                    "eq" = matrix(data = rep(0, 8760 * 1), ncol = 1)
                    )
  val_cstr2 <- list("lt" = matrix(data = rep(0, 8760 * 1), ncol = 1),
                    "eq" = matrix(data = rep(0, 8760 * 1), ncol = 1),
                    "gt" = matrix(data = rep(777, 8760 * 1), ncol = 1)
                    )
  lst_cstr <- list(
    list(
      name = "cstr1", 
      id = "cstr1",
      values = val_cstr1, 
      enabled = TRUE, 
      timeStep = "hourly",
      operator = "greater",
      coefficients = list("zone1%zone2" = 1),
      group= "group_bulk_123",
      overwrite = TRUE
      ),
    list(
      name = "cstr2", 
      id = "cstr2",
      values = val_cstr2, 
      enabled = TRUE, 
      timeStep = "hourly",
      operator = "greater",
      coefficients = list("zone1%zone2" = 1),
      group= "group_bulk_123",
      overwrite = TRUE
      )
  )  
  
  expect_error(
    createBindingConstraintBulk(constraints = lst_cstr, opts = simOptions()),
    regexp = "You are trying to create a binding constraint with a link coefficient but you have no link in your study."
  )
  
  lst_cstr <- list(
    list(
      name = "cstr1", 
      id = "cstr1",
      values = val_cstr1, 
      enabled = TRUE, 
      timeStep = "hourly",
      operator = "greater",
      coefficients = list("zone1.nuclear" = 1),
      group= "group_bulk_123",
      overwrite = TRUE
      ),
    list(
      name = "cstr2", 
      id = "cstr2",
      values = val_cstr2, 
      enabled = TRUE, 
      timeStep = "hourly",
      operator = "greater",
      coefficients = list("zone2.nuclear" = 1),
      group= "group_bulk_123",
      overwrite = TRUE
      )
  )  
  
  expect_error(
    createBindingConstraintBulk(constraints = lst_cstr, opts = simOptions()),
    regexp = "You are trying to create a binding constraint with a cluster coefficient but you have no cluster in your study."
  )
  
})
