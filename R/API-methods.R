
#' @importFrom httr GET accept_json stop_for_status content
api_get <- function(opts, url, ...) {
  result <- GET(
    url = sprintf(
      "%s/v1/studies/%s",
      opts$host, url
    ),
    accept_json(),
    ...
  )
  stop_for_status(result)
  content(result)
}

#' @importFrom httr POST accept_json content_type_json stop_for_status content 
api_post <- function(opts, url, ...) {
  result <- POST(
    url = sprintf(
      "%s/v1/studies/%s",
      opts$host, url
    ),
    accept_json(),
    content_type_json(),
    ...
  )
  stop_for_status(result)
  content(result)
}

#' @importFrom httr PUT accept_json stop_for_status content 
api_put <- function(opts, url, ...) {
  result <- PUT(
    url = sprintf(
      "%s/v1/studies/%s",
      opts$host, url
    ),
    accept_json(),
    ...
  )
  stop_for_status(result)
  content(result)
}

#' @importFrom httr DELETE accept_json stop_for_status content 
api_delete <- function(opts, url, ...) {
  result <- DELETE(
    url = sprintf(
      "%s/v1/studies/%s",
      opts$host, url
    ),
    accept_json(),
    ...
  )
  stop_for_status(result)
  content(result)
}
