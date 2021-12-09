

is_api_study <- function(opts) {
  isTRUE(opts$typeLoad == "api")
}

#' api_command_generate("create_area", area_name = "new area")
api_command_generate <- function(action, ...) {
  command <- list(
    action = action,
    args = list(...)
  )
  class(command) <- c(class(command), "antares.api.command")
  return(command)
}

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
  commands <- opts$apiCommands %||% list()
  if (inherits(command, "antares.api.command")) {
    opts$apiCommands <- append(commands, list(command))
  } else if (inherits(command, "antares.api.commands")) {
    opts$apiCommands <- c(commands, command)
  } else {
    stop(
      "'command' must be a command generetad with api_command_generate() or api_commands_generate()"
    )
  }
  return(invisible(command))
}

#' @importFrom httr POST accept_json content_type_json stop_for_status content
#' @importFrom jsonlite toJSON
api_command_execute <- function(command, opts) {
  if (inherits(command, "antares.api.command")) {
    body <- jsonlite::toJSON(list(command), auto_unbox = TRUE)
  } else if (inherits(command, "antares.api.commands")) {
    body <- jsonlite::toJSON(command, auto_unbox = TRUE)
  } else {
    stop(
      "'command' must be a command generetad with api_command_generate() or api_commands_generate()"
    )
  }
  result <- POST(
    url = sprintf(
      "%s/v1/studies/%s/commands",
      opts$host, opts$study_id
    ),
    accept_json(),
    content_type_json(),
    body = body,
    encode = "raw"
  )
  stop_for_status(result)
  return(invisible(content(result)))
}

should_command_be_executed <- function(opts) {
  isTRUE(opts$apiMode == "sync")
}


api_get_raw_data <- function(id, path) {
  result <- GET(
    url = sprintf(
      "%s/v1/studies/%s/raw",
      opts$host, id
    ),
    accept_json(),
    query = list(
      path = path, 
      formatted = TRUE
    )
  )
  stop_for_status(result)
  content(result)
}
