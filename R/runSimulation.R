#' @title Run an Antares Simulation
#' 
#' @description 
#' `r antaresEditObject:::badge_api_ok()`
#' 
#' Run an ANTARES study
#' 
#' 
#' @param name
#'   Name of the simulation. In API mode, `name` will be used as `output_suffix` argument.
#' @param mode
#'   Simulation mode, can take value "economy", "adequacy" or "draft".
#' @param path_solver
#'   Character containing the Antares Solver path
#' @param show_output_on_console
#'   Logical, indicating whether to capture the ANTARES log and show 
#'   it on the R console.
#' @param wait
#'   Logical, indicating whether the R interpreter should wait for the 
#'   simulation to finish, or run it asynchronously. 
#' @param parallel
#'   Logical. If `TRUE` the ANTARES simulation will be run in parallel mode (Work
#'   only with ANTARES v6.0.0 or more). In that case, the number of cores used by the simulation
#'   is the one set in advanced_settings/simulation_cores (see ANTARES interface).
#' @param api_extra_options
#'   Character. In API mode, a list containing the elements to configure the run. Please check define_api_extra_options().
#' @param ... Additional arguments (API only), such as `nb_cpu`, `time_limit`, ...
#'  See API documentation for all available options.
#' @param opts
#'   List of simulation parameters returned by the function
#'   [antaresRead::setSimulationPath()]
#' 
#' @return 
#' In API mode it return a `list` with either the job id in case of success of
#'  the command or details about the error produce.
#' In non-API mode the function does not return anything, it is  used to launch an 
#' ANTARES simulation.
#' 
#' @importFrom assertthat assert_that
#' @importFrom antaresRead simOptions api_post
#' @importFrom jsonlite toJSON
#'
#' @export
#' 
### Taken from antaresXpansion_0.5.2 ###
runSimulation <- function(name, 
                          mode = "economy", 
                          path_solver = getOption("antares.solver"), 
                          wait = TRUE, 
                          show_output_on_console = FALSE, 
                          parallel = TRUE,
                          api_extra_options = define_api_extra_options(),                          
                          ...,
                          opts = antaresRead::simOptions()) {
  
  assertthat::assert_that(inherits(opts, "simOptions"))
  
  if (is_api_study(opts)) {
  
    updateGeneralSettings(mode = mode, opts = opts)
    endpoint <- paste0("launcher/run/", opts[["study_id"]])
    pattern_endpoint <- .generate_pattern_launcher_endpoint(api_extra_options = api_extra_options, opts = opts)
    if (length(pattern_endpoint) > 0) {
      endpoint <- paste0(endpoint, "?", paste0(pattern_endpoint, collapse = "&"))
    }
    
    run <- api_post(
      opts = opts, 
      endpoint = endpoint, 
      default_endpoint = "v1",
      body = jsonlite::toJSON(list(output_suffix = name, ...), auto_unbox = TRUE),
      encode = "raw"
    )
    
    if (is.null(run$job_id)) {
      cli::cli_alert_danger("No job id returned by API, something went wrong.")
      return(run)
    } else {
      cli::cli_alert_success(paste("Job launched with ID:", run$job_id))
    }
    if (!isTRUE(wait)) {
      return(run)
    } else {
      status <- getJobs(run$job_id, opts = opts)
      while (is.null(status$completion_date) || is.na(status$completion_date)) {
        Sys.sleep(300)
        status <- getJobs(run$job_id, opts = opts)
      }
      if (isTRUE(show_output_on_console)) {
        # getJobLogs(job_id = run$job_id, opts = opts)
        cli::cli_alert_info(paste0(
          "Retrieve job logs with: {.code getJobLogs(\"", run$job_id, "\")}"
        ))
      }
      return(status)
     }
  } else {
    if (is.null(path_solver)) {
      path_solver <- setSolverPath()
    }
    # a few checks
    name <- tolower(name)
    assertthat::assert_that(file.exists(path_solver))
    assertthat::assert_that(mode %in% c("economy", "adequacy", "draft", "expansion"))
    
    ##Test version of antares solver
    version_solver <- .get_version_solver_from_path_solver(path_solver = path_solver)
    version_study <- substr(opts$antaresVersion,1,1)
    
    if (version_solver != version_study) {
      stop(paste0(
        "Imcompatibility between antares solver version (", version_solver, ") and study version (", version_study, ")"
      ), call. = FALSE)
    }
    
    #Launch simulation
    cmd <- '"%s" "%s" -n "%s" --%s'
    if (version_solver >= 6 & parallel) {
      cmd <- paste(cmd, "--parallel")
    }
    
    cmd <- sprintf(cmd, path_solver, opts$studyPath, name, mode)
    system(cmd, ignore.stdout = TRUE, wait = wait, show.output.on.console = show_output_on_console)
  }
}


#' @importFrom antaresRead api_get
.get_available_launchers <- function(opts) {
  
  return(antaresRead::api_get(opts = opts, 
                              endpoint = "launcher/launchers",
                              default_endpoint = "v1"
                             )
  )
}


#' @importFrom antaresRead api_get
.get_available_solver_presets <- function(opts) {
  
  return(antaresRead::api_get(opts = opts, 
                              endpoint = "launcher/solver-presets",
                              default_endpoint = "v1"
                             )
  )
}

#' @importFrom antaresRead api_get
.get_antares_version_source <- function(opts) {
  
  result <- antaresRead::api_get(opts = opts,
                                 endpoint = opts[["study_id"]],
                                 default_endpoint = "v1/studies"
                                 )
  
  return(result[["version"]])
}


#' Output profile options for running a simulation in API mode
#'
#' @param launcher Name of the launcher.
#' @param solver_preset Name of the solver.
#' @param solver_version Version of the solver.
#' @param run_at When the simulation should be run. Format YYYY-MM-DD hh-mm-ss. Time provided in GMT.
#'
#' @return a named list
#' @export
#'
#' @examples
#' define_api_extra_options(
#'   launcher="calin2opf",
#'   solver_version="9.4"
#' )
define_api_extra_options <- function(launcher = NULL,
                                     solver_preset = NULL,
                                     solver_version = NULL,
                                     run_at = NULL) {
  list(
    `launcher` = launcher,
    `solver_preset` = solver_preset,
    `solver_version` = solver_version,
    `run_at` = run_at
  )
}


#' @importFrom assertthat assert_that
.generate_pattern_launcher_endpoint <- function(api_extra_options, opts) {
  
  pattern_endpoint <- c()
  no_solver_version <- is.null(api_extra_options[["solver_version"]])
  
  if (!is.null(api_extra_options[["launcher"]])) {
    launcher <- tolower(api_extra_options[["launcher"]])
    launchers <- .get_available_launchers(opts = opts)
    launchers <- sapply(launchers[["launchers"]], "[[", "id")
    assertthat::assert_that(launcher %in% tolower(launchers),
                            msg = "Please provide a valid launcher."
                            )
    pattern_endpoint <- c(pattern_endpoint, paste0("launcher=", launcher))     
  }
  
  if (!is.null(api_extra_options[["solver_preset"]])) {
    solver <- api_extra_options[["solver_preset"]]
    solvers <- .get_available_solver_presets(opts = opts)
    solvers <- sapply(solvers, function(x) {x[c("id", "name", "minAntaresVersion", "maxAntaresVersion")]}, simplify = FALSE)
    available_solvers <- sapply(solvers,"[[", "name")
    assertthat::assert_that(tolower(solver) %in% tolower(available_solvers),
                            msg = "Please provide a valid solver."
                            )
    
    solvers_fi <- Filter(function(x) tolower(x[["name"]]) == tolower(solver), solvers)
    if (no_solver_version) {
      version <- .get_antares_version_source(opts = opts)
    } else {
      version <- api_extra_options[["solver_version"]]
    }
    solvers_fi <- Filter(f = function(x) (is.null(x[["minAntaresVersion"]]) || as.numeric(version) >= as.numeric(x[["minAntaresVersion"]])
                                          &&
                                          is.null(x[["maxAntaresVersion"]]) || as.numeric(version) <= as.numeric(x[["maxAntaresVersion"]])
                                         ),
                         x = solvers_fi
                        )
    assertthat::assert_that(length(solvers_fi) == 1,
                            msg = "Not able to detect an unique valid solver."
                            )
    
    pattern_endpoint <- c(pattern_endpoint, paste0("solver_presets_id=", solvers_fi[[1]][["id"]]))      
  }    
  
  if (!no_solver_version) {
    version <- api_extra_options[["solver_version"]]
    study_version <- .get_antares_version_source(opts = opts)
    assertthat::assert_that(as.numeric(study_version) <= as.numeric(version),
                            msg = "Please provide a solver version greater or equal to your study version."
                            )
    pattern_endpoint <- c(pattern_endpoint, paste0("version=", version))      
  }

  if (!is.null(api_extra_options[["run_at"]])) {
    run_at <- api_extra_options[["run_at"]]
    regex_date <- "^\\d{4}-\\d{2}-\\d{2} \\d{2}:\\d{2}:\\d{2}$"
    assertthat::assert_that(grepl(pattern = regex_date, x = run_at),
                            msg = "run_at is not in the expected format YYYY-MM-DD hh:mm:ss"
                            )
    pattern_endpoint <- c(pattern_endpoint, paste0("run_at=", URLencode(run_at, reserved = TRUE)))      
  }
  
  return(pattern_endpoint)
}
