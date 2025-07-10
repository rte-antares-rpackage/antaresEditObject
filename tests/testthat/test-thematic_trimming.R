
test_that("Minimum version v8.8", {
  # given
  areas <- c("fr", "be")
  study_path <- tempdir()
  study_name <- "add_all"
  opts <- list(
    "inputPath" = study_path,
    "typeLoad"= "txt",
    "areaList" = areas,
    "antaresVersion" = 870,
    "studyPath" = file.path(study_path, study_name),
    "studyName" = study_name
  )
  class(opts) <- c("simOptions")
  
  # when/then
  # message is displayed
  expect_error(
    setThematicTrimming(selection_variables = "NULL", 
                        opts = opts), 
    regexp = "'setThematicTrimming\\(\\)' calls is only available"
  )
})

# v8.8 ----
test_that("Add ALL variables", {
  # given
  areas <- c("fr", "be")
  study_path <- tempdir()
  study_name <- "add_all"
  opts <- list(
    "inputPath" = study_path,
    "typeLoad"= "txt",
    "areaList" = areas,
    "antaresVersion" = 880,
    "studyPath" = file.path(study_path, study_name),
    "studyName" = study_name
  )
  class(opts) <- c("simOptions")
  
  # create dir with properties
  dir_path <- file.path(study_path, study_name, "settings")
  lapply(dir_path, dir.create, recursive = TRUE, showWarnings = FALSE)
  
  # write generaldata
  writeLines(text = "", con = file.path(dir_path, "generaldata.ini"))
  
  # list of variables
  vect_select_vars <- antaresRead::list_thematic_variables(opts = opts)
  
  # when/then
    # message is displayed
  expect_message(
    setThematicTrimming(selection_variables = vect_select_vars$col_name, 
                        type_select = "add",
                        opts = opts), 
    regexp = "All variables are selected, by default"
  )
  
  # section variable selection is deleted 
  # only custom mode is written 
  generaldata_file <- readIni(pathIni = "settings/generaldata", 
                              opts = opts)
  expect_true(generaldata_file$general$`thematic-trimming`)
  expect_null(generaldata_file$`variables selection`)
})


test_that("Skip ALL variables", {
  # given
  areas <- c("fr", "be")
  study_path <- tempdir()
  study_name <- "add_all"
  opts <- list(
    "inputPath" = study_path,
    "typeLoad"= "txt",
    "areaList" = areas,
    "antaresVersion" = 880,
    "studyPath" = file.path(study_path, study_name),
    "studyName" = study_name
  )
  class(opts) <- c("simOptions")
  
  # create dir with properties
  dir_path <- file.path(study_path, study_name, "settings")
  lapply(dir_path, dir.create, recursive = TRUE, showWarnings = FALSE)
  
  # write generaldata
  writeLines(text = "", con = file.path(dir_path, "generaldata.ini"))
  
  # list of variables
  vect_select_vars <- antaresRead::list_thematic_variables(opts = opts)
  
  # when/then
  # message is displayed
  expect_message(
    setThematicTrimming(selection_variables = vect_select_vars$col_name, 
                        type_select = "suppr",
                        opts = opts), 
    regexp = "All variables will be skiped"
  )
  
  # check write part
  generaldata_file <- readIni(pathIni = "settings/generaldata", 
                              opts = opts)
  expect_true(generaldata_file$general$`thematic-trimming`)
  expect_false(generaldata_file$`variables selection`$selected_vars_reset)
})


test_that("select var+ less 50%", {
  # given
  areas <- c("fr", "be")
  study_path <- tempdir()
  study_name <- "add_all"
  opts <- list(
    "inputPath" = study_path,
    "typeLoad"= "txt",
    "areaList" = areas,
    "antaresVersion" = 880,
    "studyPath" = file.path(study_path, study_name),
    "studyName" = study_name
  )
  class(opts) <- c("simOptions")
  
  # create dir with properties
  dir_path <- file.path(study_path, study_name, "settings")
  lapply(dir_path, dir.create, recursive = TRUE, showWarnings = FALSE)
  
  # write generaldata
  writeLines(text = "", con = file.path(dir_path, "generaldata.ini"))
  
  # list of variables
  vect_select_vars <- antaresRead::list_thematic_variables(opts = opts)
  vect_select_vars <- vect_select_vars$col_name[1:10]
  
  # when
  setThematicTrimming(selection_variables = vect_select_vars, 
                      opts = opts)
  
  # then
  # check write part
  generaldata_file <- readIni(pathIni = "settings/generaldata", 
                              opts = opts)
  
  # expected params
  expect_true(generaldata_file$general$`thematic-trimming`)
  expect_false(generaldata_file$`variables selection`$selected_vars_reset)
  
  # expected values
  index <- names(generaldata_file$`variables selection`)%in%"select_var +"  
  
  values_expected <- unlist(generaldata_file$`variables selection`[index], 
                            use.names = FALSE)
  
  expect_equal(vect_select_vars, values_expected)
})

test_that("select var+ more 50%", {
  # given
  areas <- c("fr", "be")
  study_path <- tempdir()
  study_name <- "add_all"
  opts <- list(
    "inputPath" = study_path,
    "typeLoad"= "txt",
    "areaList" = areas,
    "antaresVersion" = 880,
    "studyPath" = file.path(study_path, study_name),
    "studyName" = study_name
  )
  class(opts) <- c("simOptions")
  
  # create dir with properties
  dir_path <- file.path(study_path, study_name, "settings")
  lapply(dir_path, dir.create, recursive = TRUE, showWarnings = FALSE)
  
  # write generaldata
  writeLines(text = "", con = file.path(dir_path, "generaldata.ini"))
  
  # list of variables
  vect_select_vars <- antaresRead::list_thematic_variables(opts = opts)
  vect_select_vars <- vect_select_vars$col_name[1:90]
  
  # when
  setThematicTrimming(selection_variables = vect_select_vars, 
                      opts = opts)
  
  # then
  # check write part
  generaldata_file <- readIni(pathIni = "settings/generaldata", 
                              opts = opts)
  
  # expected params
  expect_true(generaldata_file$general$`thematic-trimming`)
  expect_true(generaldata_file$`variables selection`$selected_vars_reset)
  
  # expected values
  index <- names(generaldata_file$`variables selection`)%in%"select_var -"  
  
  values_expected <- unlist(generaldata_file$`variables selection`[index], 
                            use.names = FALSE)
  
  object_to_test <- setdiff(antaresRead::list_thematic_variables(opts = opts)$col_name,
                            vect_select_vars)
  expect_equal(object_to_test, values_expected)
})

test_that("select var- less 50%", {
  # given
  areas <- c("fr", "be")
  study_path <- tempdir()
  study_name <- "add_all"
  opts <- list(
    "inputPath" = study_path,
    "typeLoad"= "txt",
    "areaList" = areas,
    "antaresVersion" = 880,
    "studyPath" = file.path(study_path, study_name),
    "studyName" = study_name
  )
  class(opts) <- c("simOptions")
  
  # create dir with properties
  dir_path <- file.path(study_path, study_name, "settings")
  lapply(dir_path, dir.create, recursive = TRUE, showWarnings = FALSE)
  
  # write generaldata
  writeLines(text = "", con = file.path(dir_path, "generaldata.ini"))
  
  # list of variables
  vect_select_vars <- antaresRead::list_thematic_variables(opts = opts)
  vect_select_vars <- vect_select_vars$col_name[1:10]
  
  # when
  setThematicTrimming(selection_variables = vect_select_vars, 
                      type_select = "suppr",
                      opts = opts)
  
  # then
  # check write part
  generaldata_file <- readIni(pathIni = "settings/generaldata", 
                              opts = opts)
  
  # expected params
  expect_true(generaldata_file$general$`thematic-trimming`)
  expect_true(generaldata_file$`variables selection`$selected_vars_reset)
  
  # expected values
  index <- names(generaldata_file$`variables selection`)%in%"select_var -"  
  
  values_expected <- unlist(generaldata_file$`variables selection`[index], 
                            use.names = FALSE)
  
  expect_equal(vect_select_vars, values_expected)
})

test_that("select var- more 50%", {
  # given
  areas <- c("fr", "be")
  study_path <- tempdir()
  study_name <- "add_all"
  opts <- list(
    "inputPath" = study_path,
    "typeLoad"= "txt",
    "areaList" = areas,
    "antaresVersion" = 880,
    "studyPath" = file.path(study_path, study_name),
    "studyName" = study_name
  )
  class(opts) <- c("simOptions")
  
  # create dir with properties
  dir_path <- file.path(study_path, study_name, "settings")
  lapply(dir_path, dir.create, recursive = TRUE, showWarnings = FALSE)
  
  # write generaldata
  writeLines(text = "", con = file.path(dir_path, "generaldata.ini"))
  
  # list of variables
  vect_select_vars <- antaresRead::list_thematic_variables(opts = opts)
  vect_select_vars <- vect_select_vars$col_name[1:90]
  
  # when
  setThematicTrimming(selection_variables = vect_select_vars, 
                      type_select = "suppr",
                      opts = opts)
  
  # then
  # check write part
  generaldata_file <- readIni(pathIni = "settings/generaldata", 
                              opts = opts)
  
  # expected params
  expect_true(generaldata_file$general$`thematic-trimming`)
  expect_false(generaldata_file$`variables selection`$selected_vars_reset)
  
  # expected values
  index <- names(generaldata_file$`variables selection`)%in%"select_var +"  
  
  values_expected <- unlist(generaldata_file$`variables selection`[index], 
                            use.names = FALSE)
  
  object_to_test <- setdiff(antaresRead::list_thematic_variables(opts = opts)$col_name,
                            vect_select_vars)
  expect_equal(object_to_test, values_expected)
})