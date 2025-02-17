#' @title Update general parameters of an Antares study
#' 
#' @description 
#' `r antaresEditObject:::badge_api_ok()`
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
#' @param custom.scenario See Antares General Reference Guide (see link below). Replace custom.ts.numbers.
#' @param custom.ts.numbers See Antares General Reference Guide (see link below). Replaced by custom.scenario.
#' @param user.playlist See Antares General Reference Guide (see link below).
#' @param filtering See Antares General Reference Guide (see link below).
#' @param active.rules.scenario See Antares General Reference Guide (see link below).
#' @param generate See Antares General Reference Guide (see link below).
#' @param nbtimeseriesload See Antares General Reference Guide (see link below).
#' @param nbtimeserieshydro See Antares General Reference Guide (see link below).
#' @param nbtimeserieswind See Antares General Reference Guide (see link below).
#' @param nbtimeseriesthermal See Antares General Reference Guide (see link below).
#' @param nbtimeseriessolar See Antares General Reference Guide (see link below).
#' @param refreshtimeseries See Antares General Reference Guide (see link below).
#' @param intra.modal See Antares General Reference Guide (see link below).
#' @param inter.modal See Antares General Reference Guide (see link below).
#' @param refreshintervalload See Antares General Reference Guide (see link below).
#' @param refreshintervalhydro See Antares General Reference Guide (see link below).
#' @param refreshintervalwind See Antares General Reference Guide (see link below).
#' @param refreshintervalthermal See Antares General Reference Guide (see link below).
#' @param refreshintervalsolar See Antares General Reference Guide (see link below).
#' @param readonly See Antares General Reference Guide (see link below).
#' @param geographic.trimming \code{logical} indicates whether to store the results for all time spans (FALSE) or for custom time spans (TRUE)
#' @param thematic.trimming See Antares General Reference Guide (see link below).
#' @template opts
#' 
#' @export
#' 
#' @importFrom utils modifyList
#' @importFrom assertthat assert_that
#' @importFrom antaresRead setSimulationPath readIniFile
#' @importFrom lifecycle is_present deprecate_warn deprecated
#'
#' @seealso {Antares General Reference Guide}
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
                                  custom.scenario = NULL,
                                  custom.ts.numbers = deprecated(),
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
                                  geographic.trimming = NULL,
                                  thematic.trimming = NULL,
                                  opts = antaresRead::simOptions()) {
  
  assertthat::assert_that(inherits(opts, "simOptions"))
  
  # Replace custom.ts.numbers argument by custom.scenario
  if (lifecycle::is_present(custom.ts.numbers)) {
    lifecycle::deprecate_warn(when = "0.7.1",
                              what = "updateGeneralSettings(custom.ts.numbers = )",
                              with = "updateGeneralSettings(custom.scenario = )"
                              )
    custom.scenario <- custom.ts.numbers
  }
  
  intra.modal <- check_param_modal(intra.modal, opts)
  inter.modal <- check_param_modal(inter.modal, opts)
  
  generate <- check_param_RES(generate, opts)
  refreshtimeseries <- check_param_RES(refreshtimeseries, opts)
  
  generate <- check_param_links(generate, opts)
  refreshtimeseries <- check_param_links(refreshtimeseries, opts)
  readonly <- check_param_links(readonly, opts)
  
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
    custom.scenario = custom.scenario,
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
    readonly = readonly,
    geographic.trimming = geographic.trimming,
    thematic.trimming = thematic.trimming
  )
  
  new_params <- dropNulls(x = new_params)
  new_params <- lapply(X = new_params, FUN = .format_ini_rhs)
  names(new_params) <- sapply(names(new_params), dicoGeneralSettings, USE.NAMES = FALSE)  
  
  res <- update_generaldata_by_section(opts = opts, section = "general", new_params = new_params)
  
  invisible(res)
}


check_param_modal <- function(x, opts) {
  if (is.null(x))
    return(NULL)
  name <- deparse(substitute(x))
  if (is_active_RES(opts)) {
    possible_values <- c("load", "hydro", "thermal", "renewables", "ntc")
  } else {
    possible_values <- c("load", "hydro", "wind", "thermal", "solar", "ntc")
  }
  if (!all(x %in% possible_values)) {
    warning(
      "updateGeneralSettings: '", name, "' parameter should be one of: ", 
      paste(possible_values, collapse = ", "), 
      call. = FALSE
    )
  }
  if (!is_antares_v820(opts)) {
    if (isTRUE("ntc" %in% x)) {
      warning(
        "updateGeneralSettings: '", name, "' parameter cannot be set to 'ntc' with Antares < 820" ,
        call. = FALSE
      )
      x <- setdiff(x, "ntc")
    }
  }
  if (identical(name, "inter.modal") & isTRUE("ntc" %in% x)) {
    warning(
      "updateGeneralSettings: '", name, "' parameter cannot be set to 'ntc', it'll be ignored." ,
      call. = FALSE
    )
    x <- setdiff(x, "ntc")
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


check_param_links <- function(x, opts) {
  if (is.null(x))
    return(NULL)
  name <- deparse(substitute(x))
  if (isTRUE("links" %in% x)) {
    warning(
      "updateGeneralSettings: '", name, "' parameter should not contain 'links'", 
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
  
  if (length(arg) > 1) { 
    stop("'arg' must be length one")
  }
  
  antares_params <- as.list(
    c("mode", "horizon", "nbyears", "simulation.start", "simulation.end", 
      "january.1st", "first-month-in-year", "first.weekday", "leapyear", 
      "year-by-year", "derated", "custom-scenario", "user-playlist", 
      "filtering", "active-rules-scenario", "generate", "nbtimeseriesload", 
      "nbtimeserieshydro", "nbtimeserieswind", "nbtimeseriesthermal", 
      "nbtimeseriessolar", "refreshtimeseries", "intra-modal", "inter-modal", 
      "refreshintervalload", "refreshintervalhydro", "refreshintervalwind", 
      "refreshintervalthermal", "refreshintervalsolar", "readonly", "geographic-trimming", "thematic-trimming")
  )
  names(antares_params) <- c("mode", "horizon", "nbyears", "simulation.start", "simulation.end", 
                             "january.1st", "first.month.in.year", "first.weekday", "leapyear", 
                             "year.by.year", "derated", "custom.scenario", "user.playlist", 
                             "filtering", "active.rules.scenario", "generate", "nbtimeseriesload", 
                             "nbtimeserieshydro", "nbtimeserieswind", "nbtimeseriesthermal", 
                             "nbtimeseriessolar", "refreshtimeseries", "intra.modal", "inter.modal", 
                             "refreshintervalload", "refreshintervalhydro", "refreshintervalwind", 
                             "refreshintervalthermal", "refreshintervalsolar", "readonly", "geographic.trimming", "thematic.trimming")
  antares_params[[arg]]
}
