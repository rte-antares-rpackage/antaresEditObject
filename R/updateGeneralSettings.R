#' @title Update general parameters of an Antares study
#' 
#' @description 
#' `r antaresEditObject::badge_api_ok()`
#' 
#' Update general parameters of an Antares study
#'
#' @param mode Economy, Adequacy, Draft.
#' @param horizon Reference year (static tag, not used in the calculations)
#' @param nbyears Number of Monte-Carlo years that should be prepared for the simulation (not always the same as the Number of MC years actually simulated, see 'selection mode' below).
#' @param simulation.start First day of the simulation (e.g. 8 for a simulation beginning on the second week of the first month of the year)
#' @param simulation.end Last day of the simulation (e.g. 28 for a simulation ending on the fourth week of the first month of the year)
#' @param january.1st First day of the year (Mon, Tue, etc.).
#' @param first.month.in.year Actual month by which the Time-series begin (Jan to Dec, Oct to Sep, etc.)
#' @param first.weekday In economy or adequacy simulations, indicates the frame (Mon- Sun, Sat-Fri, etc.) to use for the edition of weekly results.
#' @param leapyear (TRUE/FALSE) indicates whether February has 28 or 29 days.
#' @param year.by.year (False) No individual results will be printed out,
#' (True) For each simulated year, detailed results will be
#' printed out in an individual directory7 :
#'   Study_name/OUTPUT/simu_tag/Economy /mc-i-number
#' @param derated See Antares General Reference Guide.
#' @param custom.ts.numbers See Antares General Reference Guide.
#' @param user.playlist See Antares General Reference Guide.
#' @param filtering See Antares General Reference Guide.
#' @param active.rules.scenario See Antares General Reference Guide.
#' @param generate See Antares General Reference Guide.
#' @param nbtimeseriesload See Antares General Reference Guide.
#' @param nbtimeserieshydro See Antares General Reference Guide.
#' @param nbtimeserieswind See Antares General Reference Guide.
#' @param nbtimeseriesthermal See Antares General Reference Guide.
#' @param nbtimeseriessolar See Antares General Reference Guide.
#' @param refreshtimeseries See Antares General Reference Guide.
#' @param intra.modal See Antares General Reference Guide.
#' @param inter.modal See Antares General Reference Guide.
#' @param refreshintervalload See Antares General Reference Guide.
#' @param refreshintervalhydro See Antares General Reference Guide.
#' @param refreshintervalwind See Antares General Reference Guide.
#' @param refreshintervalthermal See Antares General Reference Guide.
#' @param refreshintervalsolar See Antares General Reference Guide.
#' @param readonly See Antares General Reference Guide.
#' 
#' @template opts
#' 
#' @export
#' 
#' @importFrom utils modifyList
#' @importFrom assertthat assert_that
#' @importFrom antaresRead setSimulationPath
#'
#' @examples
#' \dontrun{
#' 
#' # Update number of Monte-Carlo years
#' updateGeneralSettings(nbyears = 42)
#' 
#' # Use a vector to update a parameter that
#' # can take multiple values
#' updateGeneralSettings(generate = c("thermal", "hydro"))
#' 
#' }
updateGeneralSettings <- function(mode = NULL,
                                  horizon = NULL,
                                  nbyears = NULL, 
                                  simulation.start = NULL,
                                  simulation.end = NULL, 
                                  january.1st = NULL,
                                  first.month.in.year = NULL,
                                  first.weekday = NULL, 
                                  leapyear = NULL,
                                  year.by.year = NULL, 
                                  derated = NULL, 
                                  custom.ts.numbers = NULL,
                                  user.playlist = NULL,
                                  filtering = NULL, 
                                  active.rules.scenario = NULL, 
                                  generate = NULL, 
                                  nbtimeseriesload = NULL, 
                                  nbtimeserieshydro = NULL, 
                                  nbtimeserieswind = NULL, 
                                  nbtimeseriesthermal = NULL, 
                                  nbtimeseriessolar = NULL, 
                                  refreshtimeseries = NULL, 
                                  intra.modal = NULL, 
                                  inter.modal = NULL, 
                                  refreshintervalload = NULL, 
                                  refreshintervalhydro = NULL, 
                                  refreshintervalwind = NULL, 
                                  refreshintervalthermal = NULL, 
                                  refreshintervalsolar = NULL, 
                                  readonly = NULL,
                                  opts = antaresRead::simOptions()) {
  
  assertthat::assert_that(inherits(opts, "simOptions"))
  
  intra.modal <- check_param_modal(intra.modal, opts)
  inter.modal <- check_param_modal(inter.modal, opts)
  generate <- check_param_RES(generate, opts)
  refreshtimeseries <- check_param_RES(refreshtimeseries, opts)
  
  new_params <- list(
    mode = mode,
    horizon = horizon,
    nbyears = nbyears,
    simulation.start = simulation.start,
    simulation.end = simulation.end,
    january.1st = january.1st,
    first.month.in.year = first.month.in.year,
    first.weekday = first.weekday,
    leapyear = leapyear,
    year.by.year = year.by.year,
    derated = derated,
    custom.ts.numbers = custom.ts.numbers,
    user.playlist = user.playlist,
    filtering = filtering,
    active.rules.scenario = active.rules.scenario,
    generate = generate,
    nbtimeseriesload = nbtimeseriesload,
    nbtimeserieshydro = nbtimeserieshydro,
    nbtimeserieswind = nbtimeserieswind,
    nbtimeseriesthermal = nbtimeseriesthermal,
    nbtimeseriessolar = nbtimeseriessolar,
    refreshtimeseries = refreshtimeseries,
    intra.modal = intra.modal,
    inter.modal = inter.modal,
    refreshintervalload = refreshintervalload,
    refreshintervalhydro = refreshintervalhydro,
    refreshintervalwind = refreshintervalwind,
    refreshintervalthermal = refreshintervalthermal,
    refreshintervalsolar = refreshintervalsolar,
    readonly = readonly
  )
  new_params <- dropNulls(new_params)
  for (i in seq_along(new_params)) {
    new_params[[i]] <- paste(as.character(new_params[[i]]), collapse = ", ")
    names(new_params)[i] <- dicoGeneralSettings(names(new_params)[i])
  }
  
  # API block
  if (is_api_study(opts)) {

    actions <- lapply(
      X = seq_along(new_params),
      FUN = function(i) {
        list(
          target = paste0("settings/generaldata/general/", names(new_params)[i]),
          data = new_params[[i]]
        )
      }
    )
    actions <- setNames(actions, rep("update_config", length(actions)))
    cmd <- do.call(api_commands_generate, actions)
    api_command_register(cmd, opts = opts)
    `if`(
      should_command_be_executed(opts), 
      api_command_execute(cmd, opts = opts, text_alert = "Updating general settings: {msg_api}"),
      cli_command_registered("update_config")
    )
    
    return(update_api_opts(opts))
  }
  
  # read current settings
  generaldatapath <- file.path(opts$studyPath, "settings", "generaldata.ini")
  generaldata <- readIniFile(file = generaldatapath)
  
  # update general field
  l_general <- generaldata$general
  
  l_general <- utils::modifyList(x = l_general, val = new_params)
  generaldata$general <- l_general
  
  # write
  writeIni(listData = generaldata, pathIni = generaldatapath, overwrite = TRUE)
  
  # Maj simulation
  suppressWarnings({
    res <- antaresRead::setSimulationPath(path = opts$studyPath, simulation = "input")
  })
  
  invisible(res)
}

check_param_modal <- function(x, opts) {
  if (is.null(x))
    return(NULL)
  name <- deparse(substitute(x))
  if (is_active_RES(opts)) {
    possible_values <- c("load", "hydro", "thermal", "renewables")
  } else {
    possible_values <- c("load", "hydro", "wind", "thermal", "solar")
  }
  if (!all(x %in% possible_values)) {
    warning(
      "updateGeneralSettings: '", name, "' parameter should be one of: ", 
      paste(possible_values, collapse = ", "), 
      call. = FALSE
    )
  }
  return(x)
}

check_param_RES <- function(x, opts) {
  if (is.null(x))
    return(NULL)
  name <- deparse(substitute(x))
  if (isTRUE("renewable" %in% x)) {
    warning(
      "updateGeneralSettings: '", name, "' parameter should not contain 'renewable'", 
      call. = FALSE
    )
  }
  return(x)
}


#' Correspondence between arguments of \code{updateGeneralSettings} and actual Antares parameters.
#' 
#' @param arg An argument from function \code{updateGeneralSettings}.
#'
#' @return The corresponding Antares general parameter.
#' 
#' @export
#' 
#' @examples 
#' dicoGeneralSettings("year.by.year") # "year-by-year"
dicoGeneralSettings <- function(arg) {
  if (length(arg) > 1) 
    stop("'arg' must be length one")
  
  antares_params <- as.list(
    c("mode", "horizon", "nbyears", "simulation.start", "simulation.end", 
      "january.1st", "first-month-in-year", "first.weekday", "leapyear", 
      "year-by-year", "derated", "custom-ts-numbers", "user-playlist", 
      "filtering", "active-rules-scenario", "generate", "nbtimeseriesload", 
      "nbtimeserieshydro", "nbtimeserieswind", "nbtimeseriesthermal", 
      "nbtimeseriessolar", "refreshtimeseries", "intra-modal", "inter-modal", 
      "refreshintervalload", "refreshintervalhydro", "refreshintervalwind", 
      "refreshintervalthermal", "refreshintervalsolar", "readonly")
  )
  names(antares_params) <- c("mode", "horizon", "nbyears", "simulation.start", "simulation.end", 
                             "january.1st", "first.month.in.year", "first.weekday", "leapyear", 
                             "year.by.year", "derated", "custom.ts.numbers", "user.playlist", 
                             "filtering", "active.rules.scenario", "generate", "nbtimeseriesload", 
                             "nbtimeserieshydro", "nbtimeserieswind", "nbtimeseriesthermal", 
                             "nbtimeseriessolar", "refreshtimeseries", "intra.modal", "inter.modal", 
                             "refreshintervalload", "refreshintervalhydro", "refreshintervalwind", 
                             "refreshintervalthermal", "refreshintervalsolar", "readonly")
  antares_params[[arg]]
}



