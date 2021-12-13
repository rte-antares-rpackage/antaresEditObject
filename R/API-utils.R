
check_api_study <- function(opts) {
  if (!is_api_study(opts)) {
    stop("The study is not an API study", call. = FALSE)
  }
}

is_api_study <- function(opts) {
  isTRUE(opts$typeLoad == "api")
}

should_command_be_executed <- function(opts) {
  isTRUE(opts$modeAPI == "sync")
}


#' @export
print.antares.api.commands <- function(x, ...) {
  print(jsonlite::toJSON(as.list(x), pretty = TRUE, auto_unbox = TRUE))
}


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
    args = list(...)
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
      opts$host, opts$variant_id
    ),
    accept_json(),
    content_type_json(),
    body = body,
    encode = "raw"
  )
  stop_for_status(result)
  return(invisible(content(result)))
}

#' @importFrom httr GET accept_json stop_for_status content
api_get_raw_data <- function(id, path, opts) {
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

#' @importFrom httr GET accept_json stop_for_status content
api_get_variants <- function(id, opts) {
  result <- GET(
    url = sprintf(
      "%s/v1/studies/%s/variants",
      opts$host, id
    ),
    accept_json()
  )
  stop_for_status(result)
  lapply(
    X = content(result)$children,
    FUN = function(x) {
      list(name = x$node$name, id = x$node$id)
    }
  )
}



