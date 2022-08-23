
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
  full_url = sprintf(
      "%s/%s",
      opts$host, default_endpoint
    )
  if (!is.null(url)) {
    full_url = sprintf(
      "%s/%s/%s",
      opts$host, default_endpoint, url
    )
  }
  result <- GET(
    url = full_url,
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
  full_url = sprintf(
      "%s/%s",
      opts$host, default_endpoint
    )
  if (!is.null(url)) {
    full_url = sprintf(
      "%s/%s/%s",
      opts$host, default_endpoint, url
    )
  }

  result <- POST(
    url = full_url,
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
  full_url = sprintf(
      "%s/%s",
      opts$host, default_endpoint
    )
  if (!is.null(url)) {
    full_url = sprintf(
      "%s/%s/%s",
      opts$host, default_endpoint, url
    )
  }
  result <- PUT(
    url = full_url,
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
  full_url = sprintf(
      "%s/%s",
      opts$host, default_endpoint
    )
  if (!is.null(url)) {
    full_url = sprintf(
      "%s/%s/%s",
      opts$host, default_endpoint, url
    )
  }
  result <- DELETE(
    url = full_url,
    config = config,
    ...
  )
  stop_for_status(result)
  content(result)
}
