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
                           opts = antaresRead::simOptions()) {
  apply_filter <- match.arg(arg = apply_filter)
  assertthat::assert_that(inherits(opts, "simOptions"))
  
  if (name %in% antaresRead::getDistricts() & !overwrite)
    stop(paste("District", name, "already exist!"))
  
  if (length(setdiff(add_area, antaresRead::getAreas())) != 0)
    stop("Invalid area in 'add_area'")
  
  if (length(setdiff(remove_area, antaresRead::getAreas())) != 0)
    stop("Invalid area in 'remove_area'")
  
  new_district <- list(
    caption = caption,
    comments = comments,
    `apply-filter` = if (apply_filter != "none") apply_filter else NULL,
    # `+` = if (!is.null(add_area)) paste(add_area, collapse = ", ") else NULL,
    # `-` = if (!is.null(remove_area)) paste(remove_area, collapse = ", ") else NULL,
    output = output
  )
  
  new_district <- c(
    new_district,
    setNames(as.list(add_area), rep_len("+", length(add_area))),
    setNames(as.list(remove_area), rep_len("-", length(remove_area)))
  )
  
  
  # API block
  if (is_api_study(opts)) {
    
    # cmd <- api_command_generate(
    #   action = "update_config",
    #   target = paste0("input/areas/sets/", name),
    #   data = dropNulls(new_district)
    # )
    
    cmd <- api_command_generate(
      action = "create_district",
      name = name,
      base_filter = if (apply_filter != "none") apply_filter else NULL,
      # filter_items = ,# ?
      output = output,
      comments = comments
    )
    api_command_register(cmd, opts = opts)
    `if`(
      should_command_be_executed(opts), 
      api_command_execute(cmd, opts = opts, text_alert = "{.emph create_district}: {msg_api}"),
      cli_command_registered("create_district")
    )
    
    return(update_api_opts(opts))
  }
  
  # Input path
  inputPath <- opts$inputPath
  assertthat::assert_that(!is.null(inputPath) && file.exists(inputPath))
  
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
    res <- antaresRead::setSimulationPath(path = opts$studyPath, simulation = "input")
  })
  
  invisible(res)
}



#' @title Remove a district
#'
#' @param name District's name.
#'
#' @seealso [createDistrict()]
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
    
    cli_alert_success("Endpoint {.emph {'Delete district'}} {.emph 
                           {.strong {name}}} success")
                           
    return(update_api_opts(opts))
  }
  
  inputPath <- opts[["inputPath"]]
  assert_that(!is.null(inputPath) && file.exists(inputPath))
  
  # Read previous sets
  sets_path <- file.path(inputPath, "areas", "sets.ini")
  prev_params <- readIniFile(file = sets_path)
  if (name %in% names(prev_params)) {
    prev_params[[name]] <- NULL
    writeIni(listData = prev_params, pathIni = sets_path, overwrite = TRUE)
  } else {
    warning("No district was removed. Please provide the exact name of the district.")
  }
  
  suppressWarnings({
    res <- setSimulationPath(path = opts[["studyPath"]], simulation = "input")
  })
  
  invisible(res)
}
