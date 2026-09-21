
test_that(".generate_pattern_launcher_endpoint() : generate a pattern for the launcher endpoint with specific options", {
  
  testthat::local_mocked_bindings(
    .get_antares_version_source = function(...) return(9.3),
    .get_available_launchers = function(...) {
      return(list("launchers" = list(list("id" = "calin1opf"), list("id" = "calin2opf"), list("id" = "Azure"))))
    },
    .get_available_solver_presets = function(...) {
      return(
        list(
          list("id" = "1452f",
               "name" = "SIRIUS",
               "minAntaresVersion" = "8.2",
               "maxAntaresVersion" = NULL
               ),
          list("id" = "6953p",
               "name" = "COIN",
               "minAntaresVersion" = "8.6",
               "maxAntaresVersion" = "9.0"
               ),
          list("id" = "8537g",
               "name" = "COIN",
               "minAntaresVersion" = "9.1",
               "maxAntaresVersion" = "9.3"
               ),
          list("id" = "5289r",
               "name" = "COIN",
               "minAntaresVersion" = "9.4",
               "maxAntaresVersion" = NULL
               ),
          list("id" = "6752h",
               "name" = "DOUBLE",
               "minAntaresVersion" = "9.1",
               "maxAntaresVersion" = "9.4"
               ),
          list("id" = "9345w",
               "name" = "DOUBLE",
               "minAntaresVersion" = "9.3",
               "maxAntaresVersion" = NULL
               )
        )
      )
    },
    .package = "antaresEditObject"
  )
  
  # run_at
  api_options <- define_api_extra_options(run_at = "2026-09-01 12:2:00")
  testthat::expect_error(
    .generate_pattern_launcher_endpoint(api_extra_options = api_options, opts = list()),
    regexp = "run_at is not in the expected format YYYY-MM-DD hh:mm:ss"
  )
  
  api_options <- define_api_extra_options(run_at = "2026-09-01 12:12:00")
  patt <- .generate_pattern_launcher_endpoint(api_extra_options = api_options, opts = list())
  testthat::expect_equal(length(patt), 1)
  testthat::expect_equal(patt, "run_at=2026-09-01%2012%3A12%3A00")
  
  # solver_version
  api_options <- define_api_extra_options(solver_version = "9.2")
  testthat::expect_error(
    .generate_pattern_launcher_endpoint(api_extra_options = api_options, opts = list()),
    regexp = "Please provide a solver version greater or equal to your study version."
  )
  
  api_options <- define_api_extra_options(solver_version = "9.4")
  patt <- .generate_pattern_launcher_endpoint(api_extra_options = api_options, opts = list())
  testthat::expect_equal(length(patt), 1)
  testthat::expect_equal(patt, "version=9.4")
  
  # launcher
  api_options <- define_api_extra_options(launcher = "calin3opf")
  testthat::expect_error(
    .generate_pattern_launcher_endpoint(api_extra_options = api_options, opts = list()),
    regexp = "Please provide a valid launcher."
  )
  
  api_options <- define_api_extra_options(launcher = "calin2opf")
  patt <- .generate_pattern_launcher_endpoint(api_extra_options = api_options, opts = list())
  testthat::expect_equal(length(patt), 1)
  testthat::expect_equal(patt, "launcher=calin2opf")
  
  api_options <- define_api_extra_options(launcher = "CALIN2OPF") # Case insensitive
  patt <- .generate_pattern_launcher_endpoint(api_extra_options = api_options, opts = list())
  testthat::expect_equal(length(patt), 1)
  testthat::expect_equal(patt, "launcher=calin2opf")
  
  # solver
  api_options <- define_api_extra_options(solver_preset = "not solver")
  testthat::expect_error(
    .generate_pattern_launcher_endpoint(api_extra_options = api_options, opts = list()),
    regexp = "Please provide a valid solver."
  )  

  api_options <- define_api_extra_options(solver_preset = "DOUBLE")
  testthat::expect_error(
    .generate_pattern_launcher_endpoint(api_extra_options = api_options, opts = list()),
    regexp = "Not able to detect an unique valid solver."
  )
  
  api_options <- define_api_extra_options(solver_preset = "COIN")
  patt <- .generate_pattern_launcher_endpoint(api_extra_options = api_options, opts = list())
  testthat::expect_equal(length(patt), 1)
  testthat::expect_equal(patt, "solver_presets_id=8537g")
  
  api_options <- define_api_extra_options(solver_version = "9.4", solver_preset = "COIN")
  patt <- .generate_pattern_launcher_endpoint(api_extra_options = api_options, opts = list())
  testthat::expect_equal(length(patt), 2)
  testthat::expect_equal(patt, c("solver_presets_id=5289r", "version=9.4"))
  
  # all
  api_options <- define_api_extra_options(run_at = "2026-09-01 12:12:00",
                                          solver_version = "9.4",
                                          launcher = "calin2opf",
                                          solver_preset = "COIN"
                                          )
  patt <- .generate_pattern_launcher_endpoint(api_extra_options = api_options, opts = list())
  testthat::expect_equal(length(patt), 4)
  testthat::expect_equal(patt,
                         c("launcher=calin2opf", "solver_presets_id=5289r", "version=9.4", "run_at=2026-09-01%2012%3A12%3A00")
                         )  
})



test_that("define_api_extra_options() : generate a named list for the api run options with specific names", {
  
  my_options <- define_api_extra_options()
  testthat::expect_equal(length(my_options), 4)
  testthat::expect_true(all(names(my_options) %in% c("launcher", "solver_preset", "solver_version", "run_at")))
  testthat::expect_null(my_options[["launcher"]])
  testthat::expect_null(my_options[["solver_preset"]])
  testthat::expect_null(my_options[["solver_version"]])
  testthat::expect_null(my_options[["run_at"]])
  
  my_options <- define_api_extra_options(run_at = "2026-09-01 12:12:00")
  testthat::expect_equal(length(my_options), 4)
  testthat::expect_true(all(names(my_options) %in% c("launcher", "solver_preset", "solver_version", "run_at")))
  testthat::expect_null(my_options[["launcher"]])
  testthat::expect_null(my_options[["solver_preset"]])
  testthat::expect_null(my_options[["solver_version"]])
  testthat::expect_equal(my_options[["run_at"]], "2026-09-01 12:12:00")
  
  my_options <- define_api_extra_options(run_at = "2026-09-01 12:12:00", solver_version = "9.4")
  testthat::expect_equal(length(my_options), 4)
  testthat::expect_true(all(names(my_options) %in% c("launcher", "solver_preset", "solver_version", "run_at")))
  testthat::expect_null(my_options[["launcher"]])
  testthat::expect_null(my_options[["solver_preset"]])
  testthat::expect_equal(my_options[["solver_version"]], "9.4")
  testthat::expect_equal(my_options[["run_at"]], "2026-09-01 12:12:00")
  
  my_options <- define_api_extra_options(run_at = "2026-09-01 12:12:00", solver_version = "9.4", solver_preset = "Sirius")
  testthat::expect_equal(length(my_options), 4)
  testthat::expect_true(all(names(my_options) %in% c("launcher", "solver_preset", "solver_version", "run_at")))
  testthat::expect_null(my_options[["launcher"]])
  testthat::expect_equal(my_options[["solver_preset"]], "Sirius")
  testthat::expect_equal(my_options[["solver_version"]], "9.4")
  testthat::expect_equal(my_options[["run_at"]], "2026-09-01 12:12:00")
  
  my_options <- define_api_extra_options(run_at = "2026-09-01 12:12:00", solver_version = "9.4", solver_preset = "Sirius", launcher = "calin2opf")
  testthat::expect_equal(length(my_options), 4)
  testthat::expect_true(all(names(my_options) %in% c("launcher", "solver_preset", "solver_version", "run_at")))
  testthat::expect_equal(my_options[["launcher"]], "calin2opf")
  testthat::expect_equal(my_options[["solver_preset"]], "Sirius")
  testthat::expect_equal(my_options[["solver_version"]], "9.4")
  testthat::expect_equal(my_options[["run_at"]], "2026-09-01 12:12:00")
})
