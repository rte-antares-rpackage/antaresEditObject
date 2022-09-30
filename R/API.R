
#' @title Mock API usage
#' 
#' @description Use this to generate command without an active API connection,
#'  it allow to use function to edit a study to later on get API commands.
#'  
#' @param force Logical, force mocking simulation even if
#'  [antaresRead::setSimulationPathAPI] has already been called.
#' @param antares_version Antares version number.
#'
#' @template opts-value
#' @export
#'
#' @examples
#' \dontrun{
#' # Mock simulation API
#' mockSimulationAPI()
#' # Create an area
#' createArea("new area")
#' # Get commands
#' getVariantCommands()
#' }
mockSimulationAPI <- function(force = FALSE, antares_version = "8.2.0") {
  if (!is.null(getOption("antares")) & !isTRUE(force)) {
    stop("A study is already registered, use force = TRUE to overwrite.", call. = FALSE)
  }
  opts <- list(
    typeLoad = "api",
    modeAPI = "async",
    mockAPI = TRUE,
    antaresVersion = paste(unlist(as.numeric_version(antares_version)), collapse = "")
  )
  class(opts) <- c("list", "simOptions")
  options(antares = opts)
  return(invisible(opts))
}

#' @title Set API mode
#' 
#' @description Two modes are available when using the API:
#'  * **async**: record all API calls, but nothing is sent to the server
#'  * **sync**: send query to the API each time a function is used
#'
#' @param mode The mode you want to use.
#' 
#' @template opts
#' 
#' @export
#'
#' @examples
#' \dontrun{
#' # See vignette for complete documentation
#' vignette("api-variant-management")
#' 
#' # Usage :
#' setAPImode("sync")
#' 
#' }
setAPImode <- function(mode = c("sync", "async"), opts = antaresRead::simOptions()) {
  mode <- match.arg(mode)
  if (is_api_mocked(opts)) {
    warning("Cannot set API mode on sync when using mockSimulationAPI()")
    return(invisible(opts))
  }
  opts$modeAPI <- mode
  options(antares = opts)
  return(invisible(opts))
}


#' Get API commands generated
#' 
#' @param last Return the last command generated if `TRUE`, or a `numeric`
#'  for returning a specified number of commands. Default is to return all commands.
#' @param actions A `character` `vector` of actions to return.
#' @template opts-arg
#'
#' @return a list of commands to edit a variant
#' @export
#' @name variant-commands
#' 
#' @importFrom utils tail
#' @importFrom antaresRead api_get
getVariantCommands <- function(last = NULL, actions = NULL, opts = antaresRead::simOptions()) {
  if (should_command_be_executed(opts)) {
    check_variant(opts)
    commands <- api_get(opts, paste0(opts$study_id, "/commands"))
  } else {
    commands <- getOption("antaresEditObject.apiCommands", default = list())
  }
  if (is.character(actions) && length(actions) > 0) {
    commands_actions <- vapply(commands, "[[", "action", FUN.VALUE = character(1))
    commands <- commands[commands_actions %in% actions]
  }
  if (isTRUE(last))
    commands <- tail(commands, n = 1)
  if (is.numeric(last))
    commands <- tail(commands, n = last)
  class(commands) <- c("list", "antares.api.commands")
  return(commands)
}

#' @param path Path to the JSON file to write on disk.
#' @param ... Additional arguments passed to [jsonlite::write_json()]
#' 
#' @export
#' @rdname variant-commands
#' 
#' @importFrom jsonlite write_json
writeVariantCommands <- function(path, last = NULL, actions = NULL, ..., opts = antaresRead::simOptions()) {
  commands <- getVariantCommands(last = last, actions = actions, opts = opts)
  jsonlite::write_json(x = commands, path = path, auto_unbox = TRUE, ...)
}


#' @title Create a study's variant
#' 
#' @description **API**: create a new variant for a given study or use a pre-existing one. 
#'
#' @param name Name for the variant to create or the name of an existent variant.
#' 
#' @template opts
#' 
#' @export
#' @name variant
#' 
#' @importFrom httr POST accept_json status_code content add_headers
#'
#' @examples
#' \dontrun{
#' # See vignette for complete documentation
#' vignette("api-variant-management")
#' }
createVariant <- function(name, opts = antaresRead::simOptions()) {
  check_api_study(opts)
  if (is_api_mocked(opts)) {
    stop("Cannot create a variant when using mockSimulationAPI()", call. = FALSE)
  }
  config <- list(accept_json())
  if (!is.null(opts$token) && opts$token != "") {
    config <- c(config,
      add_headers(Authorization = paste("Bearer ", opts$token))
    )
  }
  result <- POST(
    sprintf(
      "%s/v1/studies/%s/variants",
      opts$host, opts$study_id
    ),
    config,
    query = list(name = name)
  )
  if (status_code(result) < 300) {
    cli::cli_alert_success("Variant succesfully created!")
  } else {
    cli::cli_alert_danger("Failed to create variant")
  }
  opts$study_id <- content(result)
  opts$modeAPI <- "sync"
  options("antaresEditObject.apiCommands" = list())
  options(antares = opts)
  return(invisible(opts))
}

#' @param variant_id ID of the variant to use, if specified `name` is ignored.
#' 
#' @export
#' @rdname variant
useVariant <- function(name, variant_id = NULL, opts = antaresRead::simOptions()) {
  check_api_study(opts)
  if (is_api_mocked(opts)) {
    stop("Cannot use a variant when using mockSimulationAPI()", call. = FALSE)
  }
  variants <- api_get_variants(opts$study_id, opts)
  variants_names <- vapply(variants, `[[`, "name", FUN.VALUE = character(1))
  variants_ids <- vapply(variants, `[[`, "id", FUN.VALUE = character(1))
  if (!is.null(variant_id)) {
    if (!isTRUE(variant_id %in% variants_ids)) {
      stop("There's no variant with ID: ", variant_id)
    }
  } else {
    if (name %in% variants_names) {
      index <- which(variants_names == name)
      if (length(index) > 1) {
        warning("'name' match (exactly) more than one variant, first one is used.")
        index <- index[1]
      }
      variant_id <- variants[[index]]$id
    } else {
      stop("Variant not found")
    }
  }
  opts$study_id <- variant_id
  options("antaresEditObject.apiCommands" = api_get(opts, paste0(opts$study_id, "/commands")))
  options(antares = opts)
  return(update_api_opts(opts))
}




#' Retrieve API jobs
#'
#' @param job_id The job identifier, if `NULL` (default), retrieve all jobs.
#' @template opts-arg 
#'
#' @return A `data.table` with information about jobs.
#' @export
#' 
#' @importFrom data.table rbindlist
#' @importFrom antaresRead api_get
#' 
#' @examples
#' \dontrun{
#' 
#' getJobs()
#' 
#' }
getJobs <- function(job_id = NULL, opts = antaresRead::simOptions()) {
  assertthat::assert_that(inherits(opts, "simOptions"))
  if (!is_api_study(opts))
    stop("getJobs can only be used with Antares API.", call. = FALSE)
  if (is.null(job_id)) {
    jobs <- api_get(opts = opts, "launcher/jobs", default_endpoint = "v1")
    suppressWarnings(data.table::rbindlist(jobs, fill = TRUE))
  } else {
    jobs <- api_get(opts = opts, paste0("launcher/jobs/", job_id), default_endpoint = "v1")
    data.table::as.data.table(jobs)
  }
}


#' Retrieve job log from API
#'
#' @param job_id The job identifier.
#' @template opts-arg 
#'
#' @return Logs as character string.
#' @export
#'
#' @importFrom antaresRead api_get
#'
#' @examples
#' \dontrun{
#' 
#' antaresRead::setSimulationPathAPI(
#'   host = "http://localhost:8080",
#'   study_id = "39c604fc-687f-46c4-9fa6-59b57ff9c8d1",
#'   token = NULL,
#'   simulation = "input"
#' )
#' job <- runSimulation()
#' getJobLogs(job)
#' 
#' }
getJobLogs <- function(job_id, opts = antaresRead::simOptions()) {
  assertthat::assert_that(inherits(opts, "simOptions"))
  if (!is_api_study(opts))
    stop("getJobLogs can only be used with Antares API.", call. = FALSE)
  if (is.list(job_id) && !is.null(job_id$job_id))
    job_id <- job_id$job_id
  logs <- api_get(opts = opts, paste0("launcher/jobs/", job_id, "/logs"), default_endpoint = "v1")
  class(logs) <- c(class(logs), "antares.api.logs")
  return(logs)
}




#' Search study in AntaREST
#'
#' @param workspace Space in which to search for a study.
#' @param folder Folder in which to search for a study.
#' @param name Name for the study.
#' @param ... Other query parameters.
#' @param host Host of AntaREST server API.
#' @param token API personnal access token.
#'
#' @return a `data.table` with informations about studies on the server.
#' @export
#' 
#' @importFrom data.table rbindlist
#'
#' @examples
#' \dontrun{
#' 
#' searchStudies(host = "http://localhost:8080")
#' 
#' }
searchStudy <- function(workspace = NULL, folder = NULL, name = NULL, ..., host = NULL, token = NULL) {
  if (is.null(host)) {
    opts <- try(simOptions(), silent = TRUE)
    if (inherits(opts, "try-error"))
      stop("searchStudies: You must provide AntaREST host!", call. = FALSE)
    host <- opts$host
    token <- opts$token
  }
  studies <- api_get(
    opts = list(host = host, token = token),
    endpoint = NULL,
    query = dropNulls(list(
      workspace = workspace,
      name = name,
      folder = folder,
      ...
    ))
  )
  suppressWarnings(data.table::rbindlist(lapply(
    X = studies,
    FUN = function(x) {
      lapply(
        X = x,
        FUN = function(x) {
          if (length(x) > 1)
            return(list(x))
          x
        }
      )
    }
  ), fill = TRUE))
}



