
is_api_study <- function(opts) {
  isTRUE(opts$typeLoad == "api")
}

api_command_generate <- function(action, ...) {
  command <- list(
    action = action,
    args = list(...)
  )
  return(command)
}

api_command_register <- function(command, opts) {
  commands <- opts$apiCommands %||% list()
  opts$apiCommands <- append(commands, list(command))
  return(invisible(command))
}

api_command_execute <- function(command, opts) {
  # todo
  return(TRUE)
}

should_command_be_executed <- function(opts) {
  isTRUE(opts$apiMode == "sync")
}

