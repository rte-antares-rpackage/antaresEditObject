
#' @title Set API mode
#' 
#' @description Two modes are available when using the API:
#'  * **async**: record all API calls, but nothing is sent to the server
#'  * **sync**: send query to the API each time a function is used
#'
#' @param mode The mode you want to use.
#' @param opts
#'   List of simulation parameters returned by the function
#'  [antaresRead::setSimulationPath()]
#' 
#' @return An updated list containing various information about the simulation.
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
  opts$apiMode <- mode
  return(invisible(opts))
}


#' @title Create a study's variant
#' 
#' @description **API**: create a new variant for a given study or use a pre-existing one. 
#'
#' @param name Name for the variant to create or the name of an existent variant.
#' @param opts
#'   List of simulation parameters returned by the function
#'  [antaresRead::setSimulationPath()]
#' 
#' @return An updated list containing various information about the simulation.
#' @export
#' @name variant
#' 
#' @importFrom httr POST accept_json stop_for_status content
#'
#' @examples
#' \dontrun{
#' # See vignette for complete documentation
#' vignette("api-variant-management")
#' }
createVariant <- function(name, opts = antaresRead::simOptions()) {
  check_api_study(opts)
  result <- POST(
    url = sprintf(
      "%s/v1/studies/%s/variants",
      opts$host, opts$study_id
    ),
    accept_json(),
    query = list(name = name)
  )
  stop_for_status(result)
  opts$variant_id <- content(result)
  opts$apiCommands <- NULL
  options(antares = opts)
  return(invisible(opts))
}

#' @export
#' @rdname variant
useVariant <- function(name, opts = antaresRead::simOptions()) {
  variants <- api_get_variants(opts$study_id, opts)
  variants_names <- vapply(variants, `[[`, "name", FUN.VALUE = character(1))
  if (name %in% variants_names) {
    index <- which(variants_names == name)
    if (length(index) > 1) {
      warning("'name' match (exactly) more than one variant, first one is used.")
      index <- index[1]
    }
    variant_id <- variants[[index]]$id
    opts$variant_id <- variant_id
    opts$apiCommands <- NULL
  } else {
    stop("Variant not found")
  }
  options(antares = opts)
  return(invisible(opts))
}





