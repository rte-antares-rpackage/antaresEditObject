
check_api_study <- function(opts) {
  if (!is_api_study(opts)) {
    stop("The study is not an API study", call. = FALSE)
  }
}

is_api_study <- function(opts) {
  isTRUE(opts$typeLoad == "api")
}

api_not_implemented <- function(opts, fun = as.character(sys.call(sys.parent()))[1L]) {
  if (is_api_study(opts)) {
    stop(fun, " is not implemented in API mode.", call. = FALSE)
  }
}

is_api_mocked <- function(opts) {
  isTRUE(opts$mockAPI)
}

should_command_be_executed <- function(opts) {
  isTRUE(opts$modeAPI == "sync")
}

#' @importFrom antaresRead api_get
is_variant <- function(opts) {
  infos <- api_get(opts = opts, endpoint = opts$study_id)
  identical(infos$type, "variantstudy")
}

check_variant <- function(opts) {
  if (!is_variant(opts)) {
    stop(
      "Study isn't a variant, please create a variant from your study with createVariant() or use an existing one with useVariant()",
      call. = FALSE
    )
  }
}

is_quiet <- function() {
  getOption("antaresEditObject.quiet", default = FALSE)
}

cli_command_registered <- function(command = "") {
  if (!is_quiet())
    cli::cli_alert_info("Command {.emph {command}} registered, see all commands with {.code getVariantCommands()}")
}


update_api_opts <- function(opts) {
  if (is_api_mocked(opts))
    return(invisible(opts))
  modeAPI <- opts$modeAPI
  if (identical(modeAPI, "async"))
    return(invisible(opts))
  host <- opts$host
  study_id <- opts$study_id
  suppressWarnings({
    opts <- antaresRead::setSimulationPathAPI(
      host = host,
      study_id = study_id,
      token = opts$token,
      simulation = "input"
    )
  })
  opts$host <- host
  opts$study_id <- study_id
  opts$modeAPI <- modeAPI
  options(antares = opts)
  return(invisible(opts))
}


# Print methods -----------------------------------------------------------

#' @importFrom utils head
#' @importFrom jsonlite toJSON
#' @export
print.antares.api.commands <- function(x, ...) {
  p <- lapply(
    X = x,
    FUN = function(x) {
      if (!is.null(x$args)) {
        x$args <- lapply(
          X = x$args,
          FUN = function(y) {
            if (length(y) > 10) {
              y <- paste(
                jsonlite::toJSON(head(y), pretty = FALSE, auto_unbox = TRUE),
                "[truncated]..."
              )
              class(y) <- "json"
            } 
            y
          }
        )
      }
      x
    }
  )
  print(jsonlite::toJSON(as.list(p), pretty = TRUE, auto_unbox = TRUE, force = TRUE, json_verbatim = TRUE))
}
#' @export
print.antares.api.command <- function(x, ...) {
  print(jsonlite::toJSON(list(as.list(x)), pretty = TRUE, auto_unbox = TRUE))
}
#' @export
print.antares.api.logs <- function(x, ...) {
  cat(x)
}


# API commands ------------------------------------------------------------

#' Generate a command
#'
#' @param action Action to perform
#' @param ... Named list of arguments
#'
#' @return The command generated
#' @noRd
#'
#' @examples
#' api_command_generate("create_area", area_name = "new area")
api_command_generate <- function(action, ...) {
  command <- list(
    action = action,
    args = dropNulls(list(...))
  )
  class(command) <- c(class(command), "antares.api.command")
  return(command)
}

#' Generate several commands
#'
#' @param ... List of actions with arguments.
#'
#' @return The commands generated
#' @noRd
#'
#' @examples
#' api_commands_generate(
#'   create_area = list(area_name = "new area"),
#'   create_area = list(area_name = "other area")
#' )
api_commands_generate <- function(...) {
  commands <- list(...)
  commands <- lapply(
    X = seq_along(commands),
    FUN = function(i) {
      list(
        action = names(commands)[i],
        args = commands[[i]]
      )
    }
  )
  class(commands) <- c(class(commands), "antares.api.commands")
  return(commands)
}

api_command_register <- function(command, opts) {
  commands <- getOption("antaresEditObject.apiCommands", default = list())
  if (inherits(command, "antares.api.command")) {
    newCommands <- append(commands, list(command))
  } else if (inherits(command, "antares.api.commands")) {
    newCommands <- c(commands, command)
  } else {
    stop(
      "'command' must be a command generated with api_command_generate() or api_commands_generate()"
    )
  }
  options("antaresEditObject.apiCommands" = newCommands)
  return(invisible(newCommands))
}

#' @importFrom httr POST accept_json content_type_json stop_for_status content
#' @importFrom jsonlite toJSON
#' @importFrom antaresRead api_get api_put api_delete api_post
api_command_execute <- function(command, opts, text_alert = "{msg_api}") {
  if (inherits(command, "antares.api.command")) {
    body <- jsonlite::toJSON(list(command), auto_unbox = TRUE)
  } else if (inherits(command, "antares.api.commands")) {
    body <- jsonlite::toJSON(command, auto_unbox = TRUE)
  } else {
    stop(
      "'command' must be a command generated with api_command_generate() or api_commands_generate()"
    )
  }
  api_post(opts, paste0(opts$study_id, "/commands"), body = body, encode = "raw")
  if (is_variant(opts)) {
    api_put(opts, paste0(opts$study_id, "/generate"))
    result <- api_get(opts, paste0(opts$study_id, "/task"))
    while(is.null(result$result)) {
      result <- api_get(opts, paste0(opts$study_id, "/task"))
    }
    result_log <- jsonlite::fromJSON(result$logs[[length(result$logs)]]$message, simplifyVector = FALSE)
    msg_api <- result_log$message
    if (is.null(msg_api) | identical(msg_api, ""))
      msg_api <- "<no feedback from API>"
    if (identical(result_log$success, TRUE)) {
      if (!is_quiet())
        cli::cli_alert_success(text_alert)
    }
    if (identical(result_log$success, FALSE)) {
      if (!is_quiet())
        cli::cli_alert_danger(text_alert)
      api_delete(opts, paste0(opts$study_id, "/commands/", result_log$id))
      if (!is_quiet())
        cli::cli_alert_warning("Command has been deleted")
    }
    return(invisible(result$result$success))
  }
}




# utils -------------------------------------------------------------------

#' @importFrom antaresRead api_get
api_get_raw_data <- function(id, path, opts) {
  api_get(
    opts, 
    endpoint = paste0(id, "/raw"), 
    query = list(
      path = path, 
      formatted = TRUE
    )
  )
}

#' @importFrom antaresRead api_get
api_get_variants <- function(id, opts) {
  variants <- api_get(
    opts, 
    endpoint = paste0(id, "/variants")
  )
  lapply(
    X = variants$children,
    FUN = function(x) {
      list(name = x$node$name, id = x$node$id)
    }
  )
}



