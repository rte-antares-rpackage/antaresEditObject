#' Update optimization parameters of an Antares study
#'
#' @param simplex.range week or day
#' @param transmission.capacities true, false or infinite
#' @param include.constraints true or false
#' @param include.hurdlecosts true or false
#' @param include.tc.min.stable.power true or false
#' @param include.tc.min.up.down.time true or false
#' @param include.dayahead true or false
#' @param include.strategicreserve true or false
#' @param include.spinningreserve true or false
#' @param include.primaryreserve true or false
#' @param include.exportmps true or false
#' @param power.fluctuations free modulations, minimize excursions or minimize ramping
#' @param shedding.strategy share margins
#' @param shedding.policy shave peaks or minimize duration
#' @param unit.commitment.mode fast or accurate
#' @param number.of.cores.mode minimum, low, medium, high or maximum
#' @param renewable.generation.modelling aggregated or cluster
#' @param day.ahead.reserve.management global
#' @param opts
#'   List of simulation parameters returned by the function
#'   \code{antaresRead::setSimulationPath}
#'
#' @return An updated list containing various information about the simulation options.
#' @export
#'
#' @importFrom utils modifyList
#' @importFrom assertthat assert_that
#' @importFrom antaresRead setSimulationPath
#'
# @examples
updateOptimizationSettings <- function(simplex.range = NULL,
                                       transmission.capacities = NULL,
                                       include.constraints = NULL,
                                       include.hurdlecosts = NULL,
                                       include.tc.min.stable.power = NULL,
                                       include.tc.min.up.down.time = NULL,
                                       include.dayahead = NULL,
                                       include.strategicreserve = NULL,
                                       include.spinningreserve = NULL,
                                       include.primaryreserve = NULL,
                                       include.exportmps = NULL,
                                       power.fluctuations = NULL,
                                       shedding.strategy = NULL,
                                       shedding.policy = NULL,
                                       unit.commitment.mode = NULL,
                                       number.of.cores.mode = NULL,
                                       renewable.generation.modelling = NULL,
                                       day.ahead.reserve.management = NULL,
                                       opts = antaresRead::simOptions()) {
  assertthat::assert_that(class(opts) == "simOptions")
  
  # check inputs
  if (!is.null(simplex.range))
    assertthat::assert_that(simplex.range %in% c("week", "day"))
  if (!is.null(transmission.capacities))
    assertthat::assert_that(transmission.capacities %in% c("true", "false", "infinite"))
  if (!is.null(include.constraints))
    assertthat::assert_that(include.constraints %in% c("true", "false"))
  if (!is.null(include.hurdlecosts))
    assertthat::assert_that(include.hurdlecosts %in% c("true", "false"))
  if (!is.null(include.tc.min.stable.power))
    assertthat::assert_that(include.tc.min.stable.power %in% c("true", "false"))
  if (!is.null(include.tc.min.up.down.time))
    assertthat::assert_that(include.tc.min.up.down.time %in% c("true", "false"))
  if (!is.null(include.dayahead))
    assertthat::assert_that(include.dayahead %in% c("true", "false"))
  if (!is.null(include.strategicreserve))
    assertthat::assert_that(include.strategicreserve %in% c("true", "false"))
  if (!is.null(include.spinningreserve))
    assertthat::assert_that(include.spinningreserve %in% c("true", "false"))
  if (!is.null(include.primaryreserve))
    assertthat::assert_that(include.primaryreserve %in% c("true", "false"))
  if (!is.null(include.exportmps))
    assertthat::assert_that(include.exportmps %in% c("true", "false"))
  
  if (!is.null(power.fluctuations))
    assertthat::assert_that(
      power.fluctuations %in% c("free modulations", "minimize excursions", "minimize ramping")
    )
  if (!is.null(shedding.strategy))
    assertthat::assert_that(shedding.strategy %in% c("share margins"))
  if (!is.null(shedding.policy))
    assertthat::assert_that(shedding.policy %in% c("shave peaks", "minimize duration"))
  if (!is.null(unit.commitment.mode))
    assertthat::assert_that(unit.commitment.mode %in% c("fast", "accurate"))
  if (!is.null(number.of.cores.mode))
    assertthat::assert_that(number.of.cores.mode %in% c("minimum", "low", "medium", "high", "maximum"))
  if (!is.null(renewable.generation.modelling))
    assertthat::assert_that(renewable.generation.modelling %in% c("aggregated", "cluster"))
  if (!is.null(day.ahead.reserve.management))
    assertthat::assert_that(day.ahead.reserve.management %in% c("global"))
  
  
  # read
  generaldatapath <-
    file.path(opts$studyPath, "settings", "generaldata.ini")
  generaldata <- readIniFile(file = generaldatapath)
  
  # update
  l_optimization <- generaldata$optimization
  l_others <- generaldata$`other preferences`
  
  new_params_optimization <- list(
    simplex.range = simplex.range,
    transmission.capacities = transmission.capacities,
    include.constraints = include.constraints,
    include.hurdlecosts = include.hurdlecosts,
    include.tc.min.stable.power = include.tc.min.stable.power,
    include.tc.min.up.down.time = include.tc.min.up.down.time,
    include.dayahead = include.dayahead,
    include.strategicreserve = include.strategicreserve,
    include.spinningreserve = include.spinningreserve,
    include.primaryreserve = include.primaryreserve,
    include.exportmps = include.exportmps
  )
  
  new_params_optimization <- dropNulls(new_params_optimization)
  
  new_params_others <- list(
    power.fluctuations = power.fluctuations,
    shedding.strategy = shedding.strategy,
    shedding.policy = shedding.policy,
    unit.commitment.mode = unit.commitment.mode,
    number.of.cores.mode = number.of.cores.mode,
    renewable.generation.modelling = renewable.generation.modelling,
    day.ahead.reserve.management = day.ahead.reserve.management
  )
  
  new_params_others <- dropNulls(new_params_others)
  
  for (i in seq_along(new_params_optimization)) {
    new_params_optimization[i] <-
      as.character(new_params_optimization[i])
    names(new_params_optimization)[i] <-
      dicoOptimizationSettings(names(new_params_optimization)[i])
  }
  
  for (i in seq_along(new_params_others)) {
    new_params_others[i] <- as.character(new_params_others[i])
    names(new_params_others)[i] <-
      dicoOptimizationSettings(names(new_params_others)[i])
  }
  
  l_optimization <-
    utils::modifyList(x = l_optimization, val = new_params_optimization)
  l_others <-
    utils::modifyList(x = l_others, val = new_params_others)
  
  generaldata$optimization <- l_optimization
  generaldata$`other preferences` <- l_others
  
  
  # write
  writeIni(listData = generaldata,
           pathIni = generaldatapath,
           overwrite = TRUE)
  
  # Maj simulation
  suppressWarnings({
    res <-
      antaresRead::setSimulationPath(path = opts$studyPath, simulation = "input")
  })
  
  invisible(res)
}





#' Correspondence between arguments of \code{updateOptimizationSettings} and actual Antares parameters.
#'
#' @param arg An argument from function \code{updateOptimizationSettings}.
#'
#' @return The corresponding Antares general parameter.
#'
#' @export
#'
#' @examples
#' dicoGeneralSettings("year.by.year") # "year-by-year"
dicoOptimizationSettings <- function(arg) {
  if (length(arg) > 1)
    stop("'arg' must be length one")
  
  
  antares_params <- as.list(
    c(
      "simplex-range",
      "transmission-capacities",
      "include-constraints",
      "include-hurdlecosts",
      "include-tc-minstablepower",
      "include-tc-min-ud-time",
      "include-dayahead",
      "include-strategicreserve",
      "include-spinningreserve",
      "include-primaryreserve",
      "include-exportmps",
      "power-fluctuations",
      "shedding-strategy",
      "shedding-policy",
      "unit-commitment-mode",
      "number-of-cores-mode",
      "renewable-generation-modelling",
      "day-ahead-reserve-management"
    )
  )
  
  
  names(antares_params) <- c(
    "simplex.range",
    "transmission.capacities",
    "include.constraints",
    "include.hurdlecosts",
    "include.tc.min.stable.power",
    "include.tc.min.up.down.time",
    "include.dayahead",
    "include.strategicreserve",
    "include.spinningreserve",
    "include.primaryreserve",
    "include.exportmps",
    "power.fluctuations",
    "shedding.strategy",
    "shedding.policy",
    "unit.commitment.mode",
    "number.of.cores.mode",
    "renewable.generation.modelling",
    "day.ahead.reserve.management"
  )
  
  antares_params[[arg]]
}
