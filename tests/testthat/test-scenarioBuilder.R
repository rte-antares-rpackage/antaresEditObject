

context("Function scenarioBuilder")


sapply(studies, function(study) {
  
  setup_study(study, sourcedir)
  opts <- antaresRead::setSimulationPath(studyPath, "input")
  original_scbuilder <- readScenarioBuilder(ruleset = "Default Ruleset", opts = opts, as_matrix = FALSE)
  
  test_that("scenarioBuilder works", {
    
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


test_that("updateScenarioBuilder() for hl with some values not between 0 and 1 (error expected)", {
  
  ant_version <- "8.2.0"
  st_test <- paste0("my_study_820_", paste0(sample(letters,5),collapse = ""))
  suppressWarnings(opts <- createStudy(path = pathstd, study_name = st_test, antares_version = ant_version))
  area <- "zone51"
  createArea(area)
  suppressWarnings(opts <- setSimulationPath(opts$studyPath, simulation = "input"))
  updateGeneralSettings(nbyears = 10)
  suppressWarnings(opts <- setSimulationPath(opts$studyPath, simulation = "input"))
  
  ldata <- scenarioBuilder(n_scenario = 10,
                          n_mc = opts$parameters$general$nbyears,
                          areas = area,
                          coef_hydro_levels = c(0.2, 1.3)
  )
  ruleset <- "Default Ruleset"
  series <- "hl"
  
  expect_error(updateScenarioBuilder(ldata = ldata,
                                     ruleset = ruleset,
                                     series = series,
                                     opts = opts
                                     ),
               regexp = "Every coefficient for hydro levels must be between 0 and 1."
  )
  
  unlink(x = opts$studyPath, recursive = TRUE)
})


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
                           coef_hydro_levels = my_coef
  )
  ruleset <- "Default Ruleset"
  
  prevSB <- readScenarioBuilder(ruleset = ruleset, as_matrix = FALSE, opts = opts)
  
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
