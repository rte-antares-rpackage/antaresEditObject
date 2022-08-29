
#' @importFrom httr GET accept_json stop_for_status content add_headers
api_get <- function(opts, url, ..., default_endpoint = "v1/studies") {
  config <- list(
    accept_json()
  )
  if (!is.null(opts$token) && opts$token != "") {
    config <- c(config,
      add_headers(Authorization = paste("Bearer ", opts$token))
    )
  }
  result <- GET(
    url = paste(c(opts$host, default_endpoint, url), collapse = "/"),
    config = config,
    ...
  )
  stop_for_status(result)
  content(result)
}

#' @importFrom httr POST accept_json content_type_json stop_for_status content add_headers
api_post <- function(opts, url, ..., default_endpoint = "v1/studies") {
  config <- list(
    accept_json(),
    content_type_json()
  )
  if (!is.null(opts$token) && opts$token != "") {
    config <- c(config,
      add_headers(Authorization = paste("Bearer ", opts$token))
    )
  }
  result <- POST(
    url = paste(c(opts$host, default_endpoint, url), collapse = "/"),
    config = config,
    ...
  )
  stop_for_status(result)
  content(result)
}

#' @importFrom httr PUT accept_json stop_for_status content add_headers
api_put <- function(opts, url, ..., default_endpoint = "v1/studies") {
  if (!is.null(opts$token) && opts$token != "") {
    config <- add_headers(Authorization = paste("Bearer ", opts$token), Accept = "application/json")
  } else {
    config <- add_headers(Accept = "application/json")
  }
  result <- PUT(
    url = paste(c(opts$host, default_endpoint, url), collapse = "/"),
    config,
    ...
  )
  stop_for_status(result)
  content(result)
}

#' @importFrom httr DELETE accept_json stop_for_status content 
api_delete <- function(opts, url, ..., default_endpoint = "v1/studies") {
  config <- list(
    accept_json()
  )
  if (!is.null(opts$token) && opts$token != "") {
    config <- c(config,
      add_headers(Authorization = paste("Bearer ", opts$token))
    )
  }
  result <- DELETE(
    url = paste(c(opts$host, default_endpoint, url), collapse = "/"),
    config = config,
    ...
  )
  stop_for_status(result)
  content(result)
}
