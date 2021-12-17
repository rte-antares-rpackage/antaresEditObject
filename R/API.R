
#' @title Mock API usage
#' 
#' @description Use this to generate command without an active API connection,
#'  it allow to use function to edit a study to later on get API commands.
#'  
#' @param force Logical, force mocking simulation even if
#'  [antaresRead::setSimulationPathAPI] has already been called.
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
mockSimulationAPI <- function(force = FALSE) {
  if (!is.null(getOption("antares")) & !isTRUE(force)) {
    stop("A study is already registered, use force = TRUE to overwrite.", call. = FALSE)
  }
  opts <- list(
    typeLoad = "api",
    modeAPI = "async",
    mockAPI = TRUE
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
setAPImode <- function(mode = c("async", "sync"), opts = antaresRead::simOptions()) {
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
#'
getVariantCommands <- function(last = NULL, actions = NULL, opts = antaresRead::simOptions()) {
  if (should_command_be_executed(opts)) {
    commands <- api_get(opts, paste0(opts$variant_id, "/commands"))
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
#' @importFrom httr POST accept_json status_code content
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
  result <- POST(
    url = sprintf(
      "%s/v1/studies/%s/variants",
      opts$host, opts$study_id
    ),
    accept_json(),
    query = list(name = name)
  )
  if (status_code(result) < 300) {
    cli::cli_alert_success("Variant succesfully created!")
  } else {
    cli::cli_alert_danger("Failed to create variant")
  }
  opts$variant_id <- content(result)
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
  opts$variant_id <- variant_id
  options("antaresEditObject.apiCommands" = api_get(opts, paste0(opts$variant_id, "/commands")))
  options(antares = opts)
  return(invisible(opts))
}





