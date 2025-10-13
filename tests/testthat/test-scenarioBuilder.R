

context("Function scenarioBuilder")

# v710 ----

sapply(studies, function(study) {
  
  setup_study(study, sourcedir)
  opts <- antaresRead::setSimulationPath(studyPath, "input")
  original_scbuilder <- readScenarioBuilder(ruleset = "Default Ruleset", opts = opts, as_matrix = FALSE)
  
  test_that("scenarioBuilder works", {
    
    # default call
    testthat::expect_warning(
      sbuilder <- scenarioBuilder(),
      regexp = "'n_scenario' parameter set to default value {1}"
      )
    
    # error call with bc (>=v870)
    testthat::expect_error(
      sbuilder <- scenarioBuilder(group_bc = "test"),
      regexp = "Parameter 'group_bc' is only"
    )
    
    testthat::expect_error(
      sbuilder <- scenarioBuilder(group_bc_rand = "test"),
      regexp = "Parameter 'group_bc_rand' is only"
    )
    
    # standard
    sbuilder <- scenarioBuilder(
      n_scenario = 2,
      n_mc = 2,
      areas = c("fr", "it", "be"),
      areas_rand = c("it", "be")
    )
    
    sb <- structure(
      c("1", "rand", "rand", "2", "rand", "rand"),
      .Dim = 3:2,
      .Dimnames = list(c("fr", "it", "be"), NULL)
    )
    
    expect_identical(sbuilder, sb)
    
  })
  
  test_that("scenarioBuilder works when areas_rand has length 1", {
    
    sbuilder <- scenarioBuilder(
      n_scenario = 2,
      n_mc = 2,
      areas = c("fr", "it", "be"),
      areas_rand = "it"
    )
    
    sb <- structure(
      c("1", "rand", "1", "2", "rand", "2"),
      .Dim = 3:2,
      .Dimnames = list(c("fr", "it", "be"), NULL)
    )
    
    expect_identical(sbuilder, sb)
    
  })
  
  test_that("Warning is thrown when n_mc differs from nbyears", {
    
    expect_warning(scenarioBuilder(
      n_scenario = 2,
      n_mc = 3,
      areas = c("fr", "it", "be"),
      areas_rand = "it"
    ))
    
  })
  
  test_that("readScenarioBuilder works", {
    
    expect_identical(
      readScenarioBuilder(),
      list(
        l = structure(
          c(1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 1L, 2L, 
            2L, 2L, 2L, 2L, 2L, 2L, 2L, 2L), .Dim = c(9L, 2L), .Dimnames = list(
              c("a", "a_offshore", "b", "c", "hub", "psp in", "psp in-2", 
                "psp out", "psp out-2"), NULL)
        ),
        t = structure(
          c(1L, 1L, 1L, 1L, 1L, NA, NA, NA, NA, NA, NA, NA, NA, 
            NA, NA, 2L, 2L, 2L, 2L, 2L, NA, NA, NA, NA, NA, NA, NA, NA, NA, 
            NA), .Dim = c(15L, 2L),
          .Dimnames = list(
            c("a_base", "a_base_must_run", 
              "a_peak", "a_peak_must_run_partial", "a_semi base", "b_base", 
              "b_peak", "b_semi base", "c_base", "c_peak", "c_semi base", "psp in-2_psp_in_2", 
              "psp in_psp_in", "psp out-2_psp_out_2", "psp out_psp_out"),
            NULL
          )
        )
      )
    )
    
    expect_warning(readScenarioBuilder(ruleset = "fake ruleset name"))
    
  })
  
  test_that("updateScenarioBuilder works", {
    
    m <- scenarioBuilder(
      n_scenario = 2,
      n_mc = 2,
      areas = c("a", "b", "c"),
      areas_rand = c("b", "c")
    )
    
    m2 <- scenarioBuilder(
      n_scenario = 2,
      n_mc = 2,
      areas = c("a", "b", "c"),
      areas_rand = "c"
    )
    
    expect_error(updateScenarioBuilder(ldata = m))
    
    expect_error(updateScenarioBuilder(ldata = m, series = "h"), NA)
    
    updateScenarioBuilder(ldata = list(w = m, s = m2))
    
    newSB <- readScenarioBuilder(as_matrix = TRUE)
    
    m <- m[m[, 1] != "rand", , drop = FALSE]
    m_out <- apply(m, 2, as.integer)
    # m_out <- as.list(m_out) # to match actual output of readScenarioBuilder()
    attributes(m_out) <- attributes(m)
    
    expect_identical(newSB[["w"]]["a", , drop = FALSE], m_out)
    
    m2 <- m2[m2[, 1] != "rand", , drop = FALSE]
    m2_out <- apply(m2, 2, as.integer)
    # m2_out <- as.list(m2_out)
    attributes(m2_out) <- attributes(m2)
    
    expect_identical(newSB[["s"]][c("a", "b"), , drop = FALSE], m2_out)
    
  })
  
  test_that("clearScenarioBuilder works", {
    expect_true(clearScenarioBuilder())
    expect_length(readScenarioBuilder(), 0L)
  })
  
  test_that("deduplicateScenarioBuilder keeps only one value by key", {
    
    added_list <- list()
    final_sbuilder <- list()
    original_scbuilder_to_write <- list()
    ruleset <- "Default Ruleset"
    nb_new_values <- 10
    
    pathSB <- file.path(opts$studyPath, "settings", "scenariobuilder.dat")
    original_scbuilder_to_write[[ruleset]] <- do.call("c", c(original_scbuilder, use.names = FALSE))
    writeIni(listData = original_scbuilder_to_write, pathIni = pathSB, overwrite = TRUE, default_ext = ".dat")
    
    sbuilder <- readScenarioBuilder(ruleset = ruleset, opts = opts, as_matrix = FALSE)
    
    series <- names(sbuilder)
    serie <- series[1]
    sbuilder_ftype <- sbuilder[[serie]]
    fkey <- names(sbuilder_ftype)[1]
    
    for(i in seq(1,nb_new_values)){
      added_list[i] <- i
    }
    names(added_list) <- rep(fkey,nb_new_values)
    sbuilder_ftype <- append(sbuilder_ftype, added_list)
    sbuilder[[serie]] <- sbuilder_ftype
    final_sbuilder[[ruleset]] <- do.call("c", c(sbuilder, use.names = FALSE))
    
    writeIni(listData = final_sbuilder, pathIni = pathSB, overwrite = TRUE, default_ext = ".dat")
    
    dupSB <- readScenarioBuilder(ruleset = ruleset, opts = opts, as_matrix = FALSE)
    dupSB_serie <- dupSB[[serie]]
    freq_dupSB_serie <- as.data.frame(table(names(dupSB_serie)))
    nb_occur_dup_fkey <- freq_dupSB_serie[freq_dupSB_serie$Var1==fkey,"Freq"]
    
    deduplicateScenarioBuilder(ruleset = ruleset, opts = opts)
    
    dedupSB <- readScenarioBuilder(ruleset = ruleset, opts = opts, as_matrix = FALSE)
    dedupSB_serie <- dedupSB[[serie]]
    freq_dedupSB_serie <- as.data.frame(table(names(dedupSB_serie)))
    nb_occur_dedup_fkey <- freq_dedupSB_serie[freq_dedupSB_serie$Var1==fkey,"Freq"]
    
    expect_equal(nb_occur_dup_fkey-nb_new_values,1)
  })
  
  test_that("deduplicateScenarioBuilder keeps the last value from a duplicated key", {
    
    added_list <- list()
    final_sbuilder <- list()
    original_scbuilder_to_write <- list()
    ruleset <- "Default Ruleset"
    fixed_value <- "123456789"
    nb_new_values <- 10
    
    pathSB <- file.path(opts$studyPath, "settings", "scenariobuilder.dat")
    original_scbuilder_to_write[[ruleset]] <- do.call("c", c(original_scbuilder, use.names = FALSE))
    writeIni(listData = original_scbuilder_to_write, pathIni = pathSB, overwrite = TRUE, default_ext = ".dat")
    
    sbuilder <- readScenarioBuilder(ruleset = ruleset, opts = opts, as_matrix = FALSE)
    
    series <- names(sbuilder)
    serie <- series[1]
    sbuilder_ftype <- sbuilder[[serie]]
    fkey <- names(sbuilder_ftype)[1]
    
    for(i in seq(1,nb_new_values)){
      added_list[i] <- i
    }
    added_list[nb_new_values+1] <- fixed_value
    names(added_list) <- rep(fkey,nb_new_values+1)
    sbuilder_ftype <- append(sbuilder_ftype, added_list)
    sbuilder[[serie]] <- sbuilder_ftype
    
    final_sbuilder[[ruleset]] <- do.call("c", c(sbuilder, use.names = FALSE))
    
    writeIni(listData = final_sbuilder, pathIni = pathSB, overwrite = TRUE, default_ext = ".dat")
    
    dupSB <- readScenarioBuilder(ruleset = ruleset, opts = opts, as_matrix = FALSE)
    dupSB_serie <- dupSB[[serie]]
    dupSB_fkey <- dupSB_serie[which(names(dupSB_serie)==fkey)]
    dupSB_fkey <- unlist(dupSB_fkey, use.names = FALSE)
    dupSB_fkey <- dupSB_fkey[length(dupSB_fkey)]
    
    deduplicateScenarioBuilder(ruleset = ruleset, opts = opts)
    
    dedupSB <- readScenarioBuilder(ruleset = ruleset, opts = opts, as_matrix = FALSE)
    dedupSB_serie <- dedupSB[[serie]]
    dedupSB_fkey <- dedupSB_serie[which(names(dedupSB_serie)==fkey)]
    dedupSB_fkey <- unlist(dedupSB_fkey, use.names = FALSE)
    dedupSB_fkey <- dedupSB_fkey[1]
    
    expect_equal(dupSB_fkey, dedupSB_fkey)
    expect_equal(dupSB_fkey, as.numeric(fixed_value))
  })
  
  # remove temporary study
  unlink(x = file.path(pathstd, "test_case"), recursive = TRUE)
  
})

# v820 ----
# hydro ----

test_that("scenarioBuilder() for hl with inconsistent number of areas or hydro levels coefficients (error expected)", {
  
  ant_version <- "8.2.0"
  st_test <- paste0("my_study_820_", paste0(sample(letters,5),collapse = ""))
  suppressWarnings(opts <- createStudy(path = pathstd, study_name = st_test, antares_version = ant_version))
  area <- "zone51"
  area2 <- "zone52"
  createArea(name = area, opts = opts)
  createArea(name = area2, opts = opts)
  suppressWarnings(opts <- setSimulationPath(opts$studyPath, simulation = "input"))
  updateGeneralSettings(nbyears = 10)
  suppressWarnings(opts <- setSimulationPath(opts$studyPath, simulation = "input"))
  
  # 1 area vs 2 coefs
  expect_error(scenarioBuilder(n_mc = opts$parameters$general$nbyears,
                               areas = area,
                               coef_hydro_levels = c(0.1, 0.2)
  ),
  regexp = "Please check the number of areas and the number of coefficients for hydro levels that you provided."
  )
  
  # 2 areas vs 1 coef
  expect_error(scenarioBuilder(n_mc = opts$parameters$general$nbyears,
                               areas = c(area,area2),
                               coef_hydro_levels = c(0.1)
  ),
  regexp = "Please check the number of areas and the number of coefficients for hydro levels that you provided."
  )
  
  unlink(x = opts$studyPath, recursive = TRUE)
})


## hl ----
test_that("scenarioBuilder() for hl with right number of areas and hydro levels coefficients", {
  
  ant_version <- "8.2.0"
  st_test <- paste0("my_study_820_", paste0(sample(letters,5),collapse = ""))
  suppressWarnings(opts <- createStudy(path = pathstd, study_name = st_test, antares_version = ant_version))
  area <- "zone51"
  area2 <- "zone52"
  createArea(name = area, opts = opts)
  createArea(name = area2, opts = opts)
  suppressWarnings(opts <- setSimulationPath(opts$studyPath, simulation = "input"))
  updateGeneralSettings(nbyears = 10)
  suppressWarnings(opts <- setSimulationPath(opts$studyPath, simulation = "input"))
  
  # number of areas * n_mc = number of coefs
  nbyears <- opts$parameters$general$nbyears
  my_area <- c(area, area2)
  my_coef <- c(runif(nbyears), runif(nbyears))
  
  ldata <- scenarioBuilder(areas = my_area,
                           coef_hydro_levels = my_coef,
                           opts = opts
  )
  
  expect_true(nrow(ldata) == length(my_area))
  expect_true(identical(sort(rownames(ldata)),sort(my_area)))
  expect_true(identical(ldata[row.names(ldata) == area,], as.character(my_coef[seq(1,nbyears)])))
  expect_true(identical(ldata[row.names(ldata) == area2,], as.character(my_coef[seq(nbyears + 1, length(my_coef))])))
  
  # number of areas = number of coefs
  my_coef <- c(0.1, 0.2)
  my_area <- c(area, area2)
  ldata <- scenarioBuilder(areas = my_area,
                           coef_hydro_levels = my_coef,
                           opts = opts
  )
  
  expect_true(nrow(ldata) == length(my_area))
  expect_true(identical(sort(rownames(ldata)),sort(my_area)))
  expect_true(unique(ldata[row.names(ldata) == area,]) == as.character(my_coef[1]))
  expect_true(unique(ldata[row.names(ldata) == area2,]) == as.character(my_coef[2]))
  
  unlink(x = opts$studyPath, recursive = TRUE)
})

## hl - all values between 0 and 1 ----
test_that("updateScenarioBuilder() for hl with all values between 0 and 1", {
  
  ant_version <- "8.2.0"
  st_test <- paste0("my_study_820_", paste0(sample(letters,5),collapse = ""))
  suppressWarnings(opts <- createStudy(path = pathstd, study_name = st_test, antares_version = ant_version))
  area <- "zone51"
  createArea(area)
  suppressWarnings(opts <- setSimulationPath(opts$studyPath, simulation = "input"))
  updateGeneralSettings(nbyears = 10)
  suppressWarnings(opts <- setSimulationPath(opts$studyPath, simulation = "input"))
  
  my_coef <- c(0.234, 0.567)
  ldata <- scenarioBuilder(n_scenario = 100,
                           n_mc = opts$parameters$general$nbyears,
                           areas = area,
                           coef_hydro_levels = rep(my_coef, opts$parameters$general$nbyears/length(my_coef))
  )
  ruleset <- "Default Ruleset"
  
  updateScenarioBuilder(ldata = ldata,
                        ruleset = ruleset,
                        series = "hl",
                        opts = opts
  )
  
  newSB <- readScenarioBuilder(ruleset = ruleset, as_matrix = FALSE, opts = opts)
  expect_true("hl" %in% names(newSB))
  
  values_newSB_hl <- unique(unlist(newSB[["hl"]], use.names = FALSE))
  expect_true(length(setdiff(my_coef, values_newSB_hl)) == 0)
  expect_true(length(setdiff(values_newSB_hl, my_coef)) == 0)
  
  unlink(x = opts$studyPath, recursive = TRUE)
})


# row repeated for each area in matrix scenarioBuilder ----
test_that("scenarioBuilder() works as expected if n_mc is not a multiple of n_scenario, same row for each area except if it is rand", {
  
  ant_version <- "8.2.0"
  st_test <- paste0("my_study_820_", paste0(sample(letters,5),collapse = ""))
  suppressWarnings(opts <- createStudy(path = pathstd, study_name = st_test, antares_version = ant_version))
  
  createArea("zone51", opts = simOptions())
  createArea("zone52", opts = simOptions())
  createArea("zone53", opts = simOptions())
  createArea("zone54", opts = simOptions())
  suppressWarnings(opts <- setSimulationPath(opts$studyPath, simulation = "input"))
  updateGeneralSettings(nbyears = 10)
  suppressWarnings(opts <- setSimulationPath(opts$studyPath, simulation = "input"))
  
  sbuilder <- scenarioBuilder(
      n_scenario = 3,
      n_mc = 10,
      areas = c("zone51", "zone52", "zone53", "zone54"),
      areas_rand = c("zone52")
  )
    
  sb <- structure(
      c("1", "rand", "1", "1", "2", "rand", "2", "2", "3", "rand", "3", "3",
        "1", "rand", "1", "1", "2", "rand", "2", "2", "3", "rand", "3", "3",
        "1", "rand", "1", "1", "2", "rand", "2", "2", "3", "rand", "3", "3",
        "1", "rand", "1", "1"
        ),
      .Dim = c(4L,10L),
      .Dimnames = list(c("zone51", "zone52", "zone53", "zone54"), NULL)
  )
    
  expect_identical(sbuilder, sb)
  
  unlink(x = opts$studyPath, recursive = TRUE)
})


# ntc - cartesian product in merge allowed ----
test_that("updateScenarioBuilder() works as expected for ntc part", {
  
  st_test <- paste0("my_study_820_", paste0(sample(letters,5),collapse = ""))
  ant_version <- "8.2.0"
  suppressWarnings(opts <- createStudy(path = pathstd, study_name = st_test, antares_version = ant_version))
  
  nbyears <- 10
  updateGeneralSettings(nbyears = nbyears, opts = simOptions())
  
  # Create 5 areas
  nb_areas <- 5
  ids_areas <- seq(1,nb_areas)
  my_areas <- paste0("zone",ids_areas)
  lapply(my_areas, function(area){createArea(name = area, opts = simOptions())})
  
  # Create 10 links (all possibilities) between zone{i} and zone{j}, i < j
  my_links <- expand.grid("from" = ids_areas, "to" = ids_areas)
  my_links$check_same <- my_links$from != my_links$to
  my_links <- my_links[my_links$check_same,]
  my_links <- my_links[my_links$from < my_links$to,]
  my_links$from <- paste0("zone",my_links$from)
  my_links$to <- paste0("zone",my_links$to)
  apply(my_links[,c("from","to")],
        MARGIN = 1,
        function(row){
          createLink(as.character(row[1]),as.character(row[2]), opts = simOptions())
        }
  )
  
  suppressWarnings(opts <- setSimulationPath(path = opts$studyPath, simulation = "input"))
  
  my_scenario <- scenarioBuilder(n_scenario = 2, n_mc = nbyears, opts = opts)
  updateScenarioBuilder(my_scenario, series = "ntc", links = as.character(getLinks(opts = opts)))
  
  sb <- readScenarioBuilder(ruleset = "Default Ruleset", as_matrix = TRUE, opts = opts)
  
  expect_true(inherits(sb, what = "list"))
  expect_true("ntc" %in% names(sb))
  expect_true(inherits(sb[["ntc"]], what = "matrix"))
  
  sb_matrix_ntc_expected <- structure(
    c(rep(c(rep(1L,10),rep(2L,10)),5)),
    .Dim = c(10L,10L),
    .Dimnames = list(c("zone1%zone2", "zone1%zone3", "zone1%zone4", "zone1%zone5", "zone2%zone3",
                       "zone2%zone4", "zone2%zone5", "zone3%zone4", "zone3%zone5", "zone4%zone5"
    ),
    NULL
    )
  )
  
  expect_identical(sb[["ntc"]], sb_matrix_ntc_expected)
  
  unlink(x = opts$studyPath, recursive = TRUE)
})

# argument series l or load OK ----
test_that("updateScenarioBuilder() has the same behaviour for one single matrix with argument series l or load", {
  
  ant_version <- "8.2.0"
  st_test <- paste0("my_study_820_", paste0(sample(letters,5),collapse = ""))
  suppressWarnings(opts <- createStudy(path = pathstd, study_name = st_test, antares_version = ant_version))
  
  createArea("zone51", opts = simOptions())
  
  updateGeneralSettings(horizon = "2030", first.month.in.year = "january", january.1st = "Monday", nbyears = 10, opts = simOptions())
  
  # Use scenarioBuilder constructor
  my_scenario <- scenarioBuilder(n_scenario = 2, areas = c("zone51"), opts = simOptions())
  
  # With series = "load"
  updateScenarioBuilder(my_scenario, series = "load", opts = simOptions())
  scbuilder_w_load <- readScenarioBuilder(ruleset = "Default Ruleset", as_matrix = TRUE, opts = simOptions())
  
  # Clear ScenarioBuilder
  clearScenarioBuilder(ruleset = "Default Ruleset", opts = simOptions())
  
  # With series = "l"
  updateScenarioBuilder(my_scenario, series = "l", opts = simOptions())
  scbuilder_w_l <- readScenarioBuilder(ruleset = "Default Ruleset", as_matrix = TRUE, opts = simOptions())
  
  expect_true(inherits(x = scbuilder_w_load, what = "list"))
  expect_true(inherits(x = scbuilder_w_l, what = "list"))
  
  expect_true(length(scbuilder_w_load) == 1)
  expect_true(length(scbuilder_w_l) == 1)
  
  expect_true(names(scbuilder_w_load) == "l")
  expect_true(names(scbuilder_w_l) == "l")
  expect_equal(scbuilder_w_load, scbuilder_w_l)
  
  unlink(x = opts$studyPath, recursive = TRUE)
})


# not allowed argument series KO ----
test_that("updateScenarioBuilder() has error if names of list or argument series is not valid", {
  
  ant_version <- "8.2.0"
  st_test <- paste0("my_study_820_", paste0(sample(letters,5),collapse = ""))
  suppressWarnings(opts <- createStudy(path = pathstd, study_name = st_test, antares_version = ant_version))
  
  createArea("zone51", opts = simOptions())
  
  updateGeneralSettings(horizon = "2030", first.month.in.year = "january", january.1st = "Monday", nbyears = 10, opts = simOptions())
  
  # Use scenarioBuilder constructor
  my_scenario <- scenarioBuilder(n_scenario = 2, areas = c("zone51"), opts = simOptions())
  
  # Single matrix
  # With series = "blablabla"
  expect_error(updateScenarioBuilder(my_scenario, series = "blablabla", opts = simOptions()),
               regexp = "Your argument series must be one of")
  
  # Clear ScenarioBuilder
  clearScenarioBuilder(ruleset = "Default Ruleset", opts = simOptions())
  
  # List of matrixes
  # With list names = "blablabla"(KO) and "l"(OK)
  expect_error(updateScenarioBuilder(ldata = list("blablabla" = my_scenario, "l" = my_scenario), opts = simOptions()),
               regexp = "Each of your list names must be in the following list")
  
  unlink(x = opts$studyPath, recursive = TRUE)
})

# v870 ----
test_that("scenarioBuilder works with binding constraint (v870)", {
  # study test creation ----
  # read script to generate study v8.7.0
  sourcedir_last_study <- system.file("study_test_generator/generate_test_study_870.R", 
                                      package = "antaresEditObject")
  
  # create study
  source(file = sourcedir_last_study)
  opts_test <- simOptions()
  
  ## no group rand ----
  sbuilder <- scenarioBuilder(
    n_scenario = opts_test$parameters$general$nbyears,
    n_mc = 10,
    group_bc = c("group_test", "default"), 
    group_bc_rand = NULL,
    mode = "bc",
    opts = opts_test
  )
  
  # Update scenario builder
  # for binding constraints series
  updateScenarioBuilder(ldata = sbuilder, series = "bc")
  
  # Read scenario builder
  # in a matrix format
  prev_sb <- readScenarioBuilder(as_matrix = TRUE)
  
  # test
  testthat::expect_equal(names(prev_sb), "bc")
  testthat::expect_equal(rownames(prev_sb$bc), c("default",
                                                 "group_test"))
  
  ## with group rand ----
  sbuilder <- scenarioBuilder(
    n_scenario = opts_test$parameters$general$nbyears,
    n_mc = 10,
    group_bc = c("group_test", "default"), 
    group_bc_rand = "default",
    mode = "bc",
    opts = opts_test
  )
  
  # Update scenario builder
  # for binding constraints series
  updateScenarioBuilder(ldata = sbuilder, series = "bc")
  
  # Read scenario builder
  # in a matrix format
  prev_sb <- readScenarioBuilder(as_matrix = TRUE)
  
  # test
  testthat::expect_equal(names(prev_sb), "bc")
  testthat::expect_equal(rownames(prev_sb$bc), 
                         "group_test")
  
  # clear
  clearScenarioBuilder()
  
  ## no bc mode ----
  # (classic mode of operation)
  sbuilder <- scenarioBuilder()
  
  # Update scenario builder
  # for binding constraints series
  updateScenarioBuilder(ldata = sbuilder, series = "t")
  
  # Read scenario builder
  # in a matrix format
  prev_sb <- readScenarioBuilder(as_matrix = TRUE)
  
  # test
  testthat::expect_equal(names(prev_sb), "t")
  
  ## parameter n_mc NULL ----
  # (classic mode of operation)
  sbuilder <- scenarioBuilder(
    n_scenario = opts_test$parameters$general$nbyears,
    n_mc = NULL,
    group_bc = c("group_test", "default"), 
    group_bc_rand = NULL,
    mode = "bc")
  
  # Update scenario builder
  # for binding constraints series
  updateScenarioBuilder(ldata = sbuilder, series = "bc")
  
  # Read scenario builder
  # in a matrix format
  prev_sb <- readScenarioBuilder(as_matrix = TRUE)
  
  # test
  value_default_n_mc <- opts_test$parameters$general$nbyears
  testthat::expect_equal(prev_sb$bc[1, drop = FALSE], rep(value_default_n_mc))
  
  # remove temporary study
  deleteStudy()
})


# v920----
suppressWarnings(
  createStudy(path = tempdir(), 
              study_name = "scenariobuilder9.2", 
              antares_version = "9.2"))

# default area with st cluster
area_test = c("al", "be") 
lapply(area_test, createArea)

test_that("Check minimal version", {
  my_coef <- runif(length(getAreas()))
  
  opts <- simOptions()
  opts$antaresVersion <- 880
  
  # one constraint => nb areas must be equal to nb coeff
  ldata <- scenarioBuilder(
    n_scenario = 10,
    n_mc = 10,
    areas = area_test,
    coef_hydro_levels = my_coef
  )
  
  expect_error(
    updateScenarioBuilder(ldata = ldata,
                          series = "hfl", 
                          opts = opts), 
               regexp = "updateScenarioBuilder: cannot use series='hfl' with Antares < 9.2"
  )
})
  
  
test_that("Add new 'hfl' equivalent to 'hl'", {
  # nb coeff equivalent to nb areas
  my_coef <- runif(length(getAreas()))
  
  opts <- simOptions()
  
  # one constraint => nb areas must be equal to nb coeff
  ldata <- scenarioBuilder(
    n_scenario = 10,
    n_mc = 10,
    areas = area_test,
    coef_hydro_levels = my_coef
  )
  
  updateScenarioBuilder(ldata = ldata,
                        series = "hfl")
  
  newSB <- readScenarioBuilder(as_matrix = FALSE)
  expect_true("hfl" %in% names(newSB))
  
  values_newSB_hfl <- unique(unlist(newSB[["hfl"]], use.names = FALSE))
  expect_equal(length(my_coef), length(values_newSB_hfl)) 
  expect_equal(my_coef, values_newSB_hfl)
})

# v930----
suppressWarnings(
  createStudy(path = tempdir(), 
              study_name = "scenariobuilder9.3", 
              antares_version = "9.3"))

# default area with st cluster
area_test = "al" 
lapply(area_test, createArea)

test_that("Add new 'sts' equivalent to 'sct apports'", {
  createClusterST(area = area_test, 
                  cluster_name = "default_prop")
  opts <- simOptions()
  ldata <- scenarioBuilder(
    n_scenario = 5,
    n_mc = 5,
    areas = area_test
  )
  
  updateScenarioBuilder(ldata = ldata,
                        series = "sts")
  
  newSTS <- readScenarioBuilder(as_matrix = FALSE)
  expect_true("sts" %in% names(newSTS))

})

test_that("Add new 'hfl' to scenariobuilder of sts", {

  # hfl
  # nb coeff equivalent to nb areas
  my_coef <- runif(length(getAreas()))
  
  opts <- simOptions()
  
  # one constraint => nb areas must be equal to nb coeff
  ldata <- scenarioBuilder(
    n_scenario = 10,
    n_mc = 10,
    areas = area_test,
    coef_hydro_levels = my_coef
  )
  
  updateScenarioBuilder(ldata = ldata,
                        series = "hfl")
  
  newSB <- readScenarioBuilder(as_matrix = FALSE)
  expect_true("hfl" %in% names(newSB))
  expect_true("sts" %in% names(newSB))
  
})


test_that("Add new 'sta'equivalent to 'sct contraintes'", {
  
  name_no_prefix <- "add_constraints"
  
  constraints_properties <- list(
    "withdrawal-1"=list(
      variable = "withdrawal",
      operator = "equal",
      hours = c("[1,3,5]", 
                "[120,121,122,123,124,125,126,127,128]")
    ),
    "netting-1"=list(
      variable = "netting",
      operator = "less",
      hours = c("[1, 168]")
    ))
  
  # creat cluster
  createClusterST(area = area_test, 
                  cluster_name = name_no_prefix, 
                  constraints_properties = constraints_properties)

  ldata <- scenarioBuilder(
    n_scenario = 5,
    n_mc = 5,
    areas = area_test
  )
  
  updateScenarioBuilder(ldata = ldata,
                        series = "sta")
  
  newSB <- readScenarioBuilder(as_matrix = FALSE)
  expect_true("sta" %in% names(newSB))
  
})

test_that("Add another'sta'to an existing one", {
  
  name_no_prefix <- "second_constraints"
  
  constraints_properties <- list(
    "test"=list(
      variable = "withdrawal",
      operator = "equal",
      hours = c("[1,50,20]")
    ))
  
  # creat cluster
  createClusterST(area = area_test, 
                  cluster_name = name_no_prefix, 
                  constraints_properties = constraints_properties)
  
  ldata <- scenarioBuilder(
    n_scenario = 3,
    n_mc = 3,
    areas = area_test
  )
  
  updateScenarioBuilder(ldata = ldata,
                        series = "sta")
  
  newSB <- readScenarioBuilder(as_matrix = FALSE)
  expect_true("sta" %in% names(newSB))
})

deleteStudy()
