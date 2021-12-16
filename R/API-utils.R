
check_api_study <- function(opts) {
  if (!is_api_study(opts)) {
    stop("The study is not an API study", call. = FALSE)
  }
}

is_api_study <- function(opts) {
  isTRUE(opts$typeLoad == "api")
}

is_api_mocked <- function(opts) {
  isTRUE(opts$mockAPI)
}

should_command_be_executed <- function(opts) {
  isTRUE(opts$modeAPI == "sync")
}

has_variant <- function(opts) {
  isTRUE(is.character(opts$variant_id) & length(opts$variant_id) == 1)
}

check_variant <- function(opts) {
  if (!has_variant(opts)) {
    stop(
      "No variant registered, please create a variant with createVariant() or use an existing one with useVariant()",
      call. = FALSE
    )
  }
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
      "'command' must be a command generated with api_command_generate() or api_commands_generate()"
    )
  }
  check_variant(opts)
  api_post(opts, paste0(opts$variant_id, "/commands"), body = body, encode = "raw")
  api_put(opts, paste0(opts$variant_id, "/generate"))
  result <- api_get(opts, paste0(opts$variant_id, "/task"))
  while(is.null(result$result)) {
    result <- api_get(opts, paste0(opts$variant_id, "/task"))
  }
  result_log <- jsonlite::fromJSON(result$logs[[length(result$logs)]]$message, simplifyVector = FALSE)
  if (identical(result_log$success, TRUE)) {
    cli::cli_alert_success(result_log$message)
  }
  if (identical(result_log$success, FALSE)) {
    cli::cli_alert_danger(result_log$message)
    # cli::cli_alert_danger(paste("Command ID:", result_log$id))
    api_delete(opts, paste0(opts$variant_id, "/commands/", result_log$id))
    cli::cli_alert_info("Command has been deleted")
  }
  return(invisible(result$result$success))
}




# utils -------------------------------------------------------------------

api_get_raw_data <- function(id, path, opts) {
  api_get(
    opts, 
    url = paste0(id, "/raw"), 
    query = list(
      path = path, 
      formatted = TRUE
    )
  )
}

api_get_variants <- function(id, opts) {
  variants <- api_get(
    opts, 
    url = paste0(id, "/variants")
  )
  lapply(
    X = variants$children,
    FUN = function(x) {
      list(name = x$node$name, id = x$node$id)
    }
  )
}



