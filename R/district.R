#' @title Create a district
#' 
#' @description Allows selecting a set of areas so as to bundle them together in a "district".
#'
#' @param name District's name.
#' @param caption Caption for the district.
#' @param comments Comments for the district.
#' @param apply_filter Possible values are `add-all` to add all areas to the district,
#'  `remove-all` to clear the district, or `none` (default) to don't apply a filter.
#' @param add_area Character vector of area(s) to add to the district.
#' @param remove_area Character vector of area(s) to remove from the district.
#' @param output Logical, compute the results for the district or not?
#' @param overwrite Logical, should the district be overwritten if already exist?
#' 
#' @seealso [editDistrict()], [removeDistrict()]
#'
#' @importFrom assertthat assert_that
#' @importFrom antaresRead simOptions setSimulationPath getDistricts getAreas api_post
#' @importFrom cli cli_alert_success
#'
#' @template opts
#'
#' @export
#'
#' @examples
#' \dontrun{
#' createDistrict(
#'   name = "mydistrict",
#'   apply_filter = "add-all",
#'   remove_area = c("fr", "be")
#' )
#' }
createDistrict <- function(name, 
                           caption = NULL, 
                           comments = NULL,
                           apply_filter = c("none", "add-all", "remove-all"),
                           add_area = NULL, 
                           remove_area = NULL,
                           output = FALSE, 
                           overwrite = FALSE, 
                           opts = simOptions()) {
  
  apply_filter <- match.arg(arg = apply_filter)
  
  assert_that(inherits(opts, "simOptions"))
  assert_that(inherits(output, "logical"))
  assert_that(inherits(overwrite, "logical"))
  
  if (tolower(name) %in% getDistricts(opts = opts) & !overwrite) {
    stop(paste("District", name, "already exists!"))
  }
  
  with_add_area <- !is.null(add_area)
  with_remove_area <- !is.null(remove_area)
  all_areas <- getAreas(opts = opts)
  
  assert_that(xor(with_add_area, with_remove_area), msg = "You can not use 'add_area' and 'remove_area' at the same time")
  if (with_add_area) {
    assert_that(length(setdiff(add_area, all_areas)) == 0, msg = "Invalid area in 'add_area'")
    assert_that(apply_filter %in% c("remove-all", "none"), msg = "You have to use 'add_area' with 'apply_filter' set to remove-all")
    if (identical(apply_filter, "none")) {
      apply_filter <- "remove-all"
    }
  }
  if (with_remove_area) {
    assert_that(length(setdiff(remove_area, all_areas)) == 0, msg = "Invalid area in 'remove_area'")
    assert_that(apply_filter %in% c("add-all", "none"), msg = "You have to use 'remove_area' with 'apply_filter' set to add-all")
    if (identical(apply_filter, "none")) {
      apply_filter <- "add-all"
    }
  }

  new_district <- list(
    caption = caption,
    comments = comments,
    `apply-filter` = apply_filter,
    output = output
  )
  
  # API block
  if (is_api_study(opts = opts)) {
    new_district[["name"]] <- name
    
    if (with_add_area) {
      new_district[["areas"]] <- add_area
    }
    
    if (with_remove_area) {
      new_district[["areas"]] <- remove_area
    }
    
    body <- transform_list_to_json_for_district_parameters(district_parameters = new_district)
    
    result <- api_post(opts = opts,
                       endpoint = file.path(opts[["study_id"]], "districts"),
                       default_endpoint = "v1/studies",
                       body = body
                      )
    
    cli_alert_success("Endpoint Create {.emph {.strong district}} {.emph {.strong {name}}} success")               
    
    return(update_api_opts(opts = opts))
  }
  
  # Input path
  inputPath <- opts[["inputPath"]]
  assert_that(!is.null(inputPath) && file.exists(inputPath))

  new_district <- c(
    new_district,
    setNames(as.list(add_area), rep_len("+", length(add_area))),
    setNames(as.list(remove_area), rep_len("-", length(remove_area)))
  )
  
  # Read previous sets
  sets_path <- file.path(inputPath, "areas", "sets.ini")
  if (file.exists(sets_path)) {
    sets_params <- readIniFile(file = sets_path)
    sets_names <- names(sets_params)
    sets_params[[name]] <- dropNulls(new_district)
  } else {
    sets_params <- list()
    sets_params[[name]] <- dropNulls(new_district)
  }
  writeIni(listData = sets_params, pathIni = sets_path, overwrite = TRUE)
  
  # Maj simulation
  suppressWarnings({
    res <- setSimulationPath(path = opts[["studyPath"]], simulation = "input")
  })
  
  invisible(res)
}


#' @title Edit a district in an Antares study
#'
#' @inheritParams createDistrict
#'
#' @template opts
#' 
#' @seealso [createDistrict()], [removeDistrict()]
#'
#' @importFrom antaresRead simOptions getDistricts getAreas api_put
#' @importFrom assertthat assert_that
#' @importFrom cli cli_alert_success
#'
#' @export
#'
#' @examples
#' \dontrun{
#' editDistrict(
#'   name = "my_existing_district",
#'   comments = "This is my district",
#'   add_area = c("area1", "area3", "area5"),
#'   opts = simOptions()
#' )
#' }
editDistrict <- function(name, 
                         caption = NULL, 
                         comments = NULL,
                         apply_filter = NULL,
                         add_area = NULL, 
                         remove_area = NULL,
                         output = NULL, 
                         opts = simOptions()) {
  
  assert_that(inherits(opts, "simOptions"))
  assert_that(tolower(name) %in% getDistricts(opts = opts), msg = paste0("No district ", name, " in the study."))
  
  with_add_area <- !is.null(add_area)
  with_remove_area <- !is.null(remove_area)
  all_areas <- getAreas(opts = opts)
  
  if (with_add_area & with_remove_area) {
    assert_that(xor(with_add_area, with_remove_area), msg = "You can not use 'add_area' and 'remove_area' at the same time")
  }
  if (with_add_area) {
    assert_that(length(setdiff(add_area, all_areas)) == 0, msg = "Invalid area in 'add_area'")
  }
  if (with_remove_area) {
    assert_that(length(setdiff(remove_area, all_areas)) == 0, msg = "Invalid area in 'remove_area'")
  }
  if (!is.null(output)) {
    assert_that(inherits(output, "logical"))
  }
  if (!is.null(apply_filter)) {
    assert_that(apply_filter %in% c("none", "add-all", "remove-all"), msg = "apply_filter is not an allowed value")
  }
  
  new_district <- list(
    "caption" = caption,
    "comments" = comments,
    "apply-filter" = apply_filter,
    "output" = output
  )
  
  if (is_api_study(opts = opts)) {
    
    if (with_add_area) {
      new_district[["areas"]] <- add_area
    }
    
    if (with_remove_area) {
      new_district[["areas"]] <- remove_area
    }
    
    body <- transform_list_to_json_for_district_parameters(district_parameters = new_district)
    
    result <- api_put(opts = opts,
                      endpoint = file.path(opts[["study_id"]], "districts", tolower(name)),
                      default_endpoint = "v1/studies",
                      body = body
                      )
    
    cli_alert_success("Endpoint edit {.emph {.strong district}} {.emph {.strong {name}}} success") 
    
    return(update_api_opts(opts = opts))
  }
  
}


#' @title Remove a district
#'
#' @param name District's name.
#'
#' @seealso [createDistrict()], [editDistrict()]
#'
#' @template opts
#'
#' @importFrom antaresRead simOptions setSimulationPath getDistricts api_delete
#' @importFrom assertthat assert_that
#' @importFrom cli cli_alert_success
#'
#' @export
#'
#' @examples
#' \dontrun{
#' removeDistrict(
#'   name = "mydistrict",
#'   opts = simOptions()
#' )
#' }
removeDistrict <- function(name, opts = simOptions()) {
  
  assert_that(tolower(name) %in% getDistricts(opts = opts), msg = paste("No district", name, "in the study."))
  
  if (is_api_study(opts = opts)) {
    
    api_delete(opts = opts, 
               endpoint =  file.path(opts[["study_id"]], "districts", tolower(name)),
               default_endpoint = "v1/studies"
               )
    
    cli_alert_success("Endpoint {.emph {'Delete district'}} {.emph {.strong {name}}} success")
    
    return(update_api_opts(opts = opts))
  }
  
  inputPath <- opts[["inputPath"]]
  assert_that(!is.null(inputPath) && file.exists(inputPath))
  
  sets_path <- file.path(inputPath, "areas", "sets.ini")
  prev_params <- readIniFile(file = sets_path)
  
  existing_districts <- names(prev_params)
  idx_match <- grep(pattern = paste0("^",name,"$"), x = existing_districts, ignore.case = TRUE)
  
  if (length(idx_match) == 1) {
    district_to_delete <- existing_districts[idx_match]
    prev_params[[district_to_delete]] <- NULL
    writeIni(listData = prev_params, pathIni = sets_path, overwrite = TRUE)
  } else {
    warning("No district was removed.")
  }
  
  suppressWarnings({
    res <- setSimulationPath(path = opts[["studyPath"]], simulation = "input")
  })
  
  invisible(res)
}


#' @importFrom assertthat assert_that
#' @importFrom jsonlite toJSON
transform_list_to_json_for_district_parameters <- function(district_parameters) {

  assert_that(inherits(x = district_parameters, what = "list"))
  # caption is an extra input and not permitted by the endpoint
  district_parameters[["caption"]] <- NULL
  district_parameters <- dropNulls(district_parameters)
  names(district_parameters) <- sapply(names(district_parameters), rename_district_parameters_for_endpoint, USE.NAMES = FALSE) 
  
  return(toJSON(district_parameters, auto_unbox = TRUE))
}


#' Correspondence between list of district parameters and endpoint inputs.
#' 
#' @param arg A name from a list of parameters
#'
#' @return The corresponding endpoint input.
#' @noRd
rename_district_parameters_for_endpoint <- function(arg) {
  
  if (length(arg) > 1) { 
    stop("'arg' must be length one")
  }
  
  antares_params <- as.list(c("name", "caption", "comments", "output", "apply_filter", "areas"))
  
  names(antares_params) <- c("name", "caption", "comments", "output", "apply-filter", "areas")
  
  antares_params[[arg]]
}
