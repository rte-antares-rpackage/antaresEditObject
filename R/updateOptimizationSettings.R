#' @title Update optimization parameters of an Antares study
#' 
#' @description 
#' `r antaresEditObject:::badge_api_ok()`
#' 
#' Update optimization parameters and other preferences of an Antares study
#'
#' @param simplex.range week or day
#' @param transmission.capacities true, false or infinite (since v8.4 can also take : local-values, 
#' null-for-all-links, infinite-for-all-links, null-for-physical-links, infinite-for-physical-links)
#' @param include.constraints true or false
#' @param include.hurdlecosts true or false
#' @param include.tc.min.stable.power true or false
#' @param include.tc.min.up.down.time true or false
#' @param include.dayahead true or false
#' @param include.strategicreserve true or false
#' @param include.spinningreserve true or false
#' @param include.primaryreserve true or false
#' @param include.exportmps true or false (since v8.3.2 can take also : none, optim-1, optim-2, both-optims)
#' @param solver.log true or false (available for version >= 8.8)
#' @param power.fluctuations free modulations, minimize excursions or minimize ramping
#' @param shedding.strategy share margins
#' @param shedding.policy shave peaks (accurate shave peaks for study >= v9.2)or minimize duration
#' @param unit.commitment.mode fast, accurate or milp
#' @param number.of.cores.mode minimum, low, medium, high or maximum
#' @param renewable.generation.modelling aggregated or clusters
#' @param day.ahead.reserve.management global
#' @param include.exportstructure true or false
#' @param include.unfeasible.problem.behavior warning-dry, warning-verbose, error-dry or error-verbose
#' @param hydro.heuristic.policy accommodate rule curves or maximize generation
#' @param hydro.pricing.mode fast or accurate
#' @param accurate.shave.peaks.include.short.term.storage true or false
#' 
#' @template opts
#'
#' @seealso \href{https://antares-simulator.readthedocs.io/en/latest/user-guide/solver/04-parameters/#optimization-parameters}{Optimization parameters}
#'
#' @export
#'
#' @importFrom utils modifyList
#' @importFrom assertthat assert_that
#' @importFrom antaresRead setSimulationPath readIniFile
#'
#' @examples
#' \dontrun{
#' 
#' updateOptimizationSettings(
#'   simplex.range = "week", 
#'   power.fluctuations = "minimize ramping"
#' )
#' 
#' }
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
                                       solver.log = NULL,
                                       power.fluctuations = NULL,
                                       shedding.strategy = NULL,
                                       shedding.policy = NULL,
                                       unit.commitment.mode = NULL,
                                       number.of.cores.mode = NULL,
                                       renewable.generation.modelling = NULL,
                                       day.ahead.reserve.management = NULL,
                                       include.exportstructure = NULL,
                                       include.unfeasible.problem.behavior = NULL,
                                       hydro.heuristic.policy = NULL,
                                       hydro.pricing.mode = NULL,
                                       accurate.shave.peaks.include.short.term.storage = NULL,
                                       opts = antaresRead::simOptions()) {
  assertthat::assert_that(inherits(opts, "simOptions"))
  
  bad_value_msg <- " is not an authorized value"
  
	# check inputs
  if (!is.null(simplex.range)) {
    dico_optimization <- dicoOptimizationSettings(arg = "simplex.range")
    assertthat::assert_that(simplex.range %in% dico_optimization[["values"]],
                            msg = paste0(simplex.range, bad_value_msg)
                           )
  }
  
  if (!is.null(transmission.capacities))
    if (opts$antaresVersion >= 840){
      assertthat::assert_that(transmission.capacities %in% c("true", "false", "infinite",
                                                             "local-values", "null-for-all-links", "infinite-for-all-links",
                                                             "null-for-physical-links", "infinite-for-physical-links"))
      if (transmission.capacities == "true") transmission.capacities <- "local-values"
      else if (transmission.capacities == "false") transmission.capacities <- "null-for-all-links"
      else if (transmission.capacities == "infinite") transmission.capacities <- "infinite-for-all-links"
    } else {
      assertthat::assert_that(transmission.capacities %in% c("true", "false", "infinite"))
      
    }
  
  if (!is.null(include.constraints)) {
    dico_optimization <- dicoOptimizationSettings(arg = "include.constraints")
    assertthat::assert_that(include.constraints %in% dico_optimization[["values"]],
                            msg = paste0(include.constraints, bad_value_msg)
                          )
  }
  
  if (!is.null(include.hurdlecosts)) {
    dico_optimization <- dicoOptimizationSettings(arg = "include.hurdlecosts")
    assertthat::assert_that(include.hurdlecosts %in% dico_optimization[["values"]],
                            msg = paste0(include.hurdlecosts, bad_value_msg)
                           )
  }
  
  if (!is.null(include.tc.min.stable.power)) {
    dico_optimization <- dicoOptimizationSettings(arg = "include.tc.min.stable.power")
    assertthat::assert_that(include.tc.min.stable.power %in% dico_optimization[["values"]],
                            msg = paste0(include.tc.min.stable.power, bad_value_msg)
                           )
  }
  
  if (!is.null(include.tc.min.up.down.time)) {
    dico_optimization <- dicoOptimizationSettings(arg = "include.tc.min.up.down.time")
    assertthat::assert_that(include.tc.min.up.down.time %in% dico_optimization[["values"]],
                            msg = paste0(include.tc.min.up.down.time, bad_value_msg)
                           )
  }
  
  if (!is.null(include.dayahead)) {
    dico_optimization <- dicoOptimizationSettings(arg = "include.dayahead")
    assertthat::assert_that(include.dayahead %in% dico_optimization[["values"]],
                            msg = paste0(include.dayahead, bad_value_msg)
                           )
  }
  
  if (!is.null(include.strategicreserve)) {
    dico_optimization <- dicoOptimizationSettings(arg = "include.strategicreserve")
    assertthat::assert_that(include.strategicreserve %in% dico_optimization[["values"]],
                            msg = paste0(include.strategicreserve, bad_value_msg)
                           )
  }
  
  if (!is.null(include.spinningreserve)) {
    dico_optimization <- dicoOptimizationSettings(arg = "include.spinningreserve")
    assertthat::assert_that(include.spinningreserve %in% dico_optimization[["values"]],
                            msg = paste0(include.spinningreserve, bad_value_msg)
                           )
  }
  
  if (!is.null(include.primaryreserve)) {
    dico_optimization <- dicoOptimizationSettings(arg = "include.primaryreserve")
    assertthat::assert_that(include.primaryreserve %in% dico_optimization[["values"]],
                            msg = paste0(include.primaryreserve, bad_value_msg)
                           )
  }
  
  if (!is.null(include.exportstructure)) {
    dico_optimization <- dicoOptimizationSettings(arg = "include.exportstructure")
    assertthat::assert_that(include.exportstructure %in% dico_optimization[["values"]],
                            msg = paste0(include.exportstructure, bad_value_msg)
                            )
  }
  
  if (!is.null(include.unfeasible.problem.behavior)) {
    dico_optimization <- dicoOptimizationSettings(arg = "include.unfeasible.problem.behavior")
    assertthat::assert_that(include.unfeasible.problem.behavior %in% dico_optimization[["values"]],
                            msg = paste0(include.unfeasible.problem.behavior, bad_value_msg)
                           )
  }
  
  if (!is.null(include.exportmps)){
    if (opts$antaresVersion >= 832){
      assertthat::assert_that(include.exportmps %in% c("true", "false",
                                                       "none", "optim-1", "optim-2", "both-optims"))
      if (include.exportmps == "true") include.exportmps <- "both-optims"
      else if (include.exportmps == "false") include.exportmps <- "none"
    } else {
      assertthat::assert_that(include.exportmps %in% c("true", "false"))
    }
  }
  
  if (!is.null(solver.log)) {
    if (opts$antaresVersion < 880) {
      stop("updateOptimizationSettings: solver.log parameter is only available if using Antares >= 8.8.0", call. = FALSE)
    }
    dico_optimization <- dicoOptimizationSettings(arg = "solver.log")    
    assertthat::assert_that(solver.log %in% dico_optimization[["values"]],
                            msg = paste0(solver.log, bad_value_msg)
                            )
  }
  
  if (!is.null(power.fluctuations)) {
    dico_optimization <- dicoOptimizationSettings(arg = "power.fluctuations")
    assertthat::assert_that(power.fluctuations %in% dico_optimization[["values"]],
                            msg = paste0(power.fluctuations, bad_value_msg)
                            )
  }
  
  if (!is.null(shedding.strategy)) {
    dico_optimization <- dicoOptimizationSettings(arg = "shedding.strategy")
    assertthat::assert_that(shedding.strategy %in% dico_optimization[["values"]],
                            msg = paste0(shedding.strategy, bad_value_msg)
                           )
  }
  
  if (!is.null(shedding.policy)){
    if(opts$antaresVersion>=920)
      assertthat::assert_that(
        shedding.policy %in% 
          c("accurate shave peaks", "minimize duration"))
    else
      assertthat::assert_that(
        shedding.policy %in% 
          c("shave peaks", "minimize duration"))
  }
  
  if (!is.null(unit.commitment.mode)) {
    dico_optimization <- dicoOptimizationSettings(arg = "unit.commitment.mode")
    assertthat::assert_that(unit.commitment.mode %in% dico_optimization[["values"]],
                            msg = paste0(unit.commitment.mode, bad_value_msg)
                           )
  }
  
  if (!is.null(number.of.cores.mode)) {
    dico_optimization <- dicoOptimizationSettings(arg = "number.of.cores.mode")
    assertthat::assert_that(number.of.cores.mode %in% dico_optimization[["values"]],
                            msg = paste0(number.of.cores.mode, bad_value_msg)
                           )
  }
  
  if (!is.null(renewable.generation.modelling)) {
    if (opts$antaresVersion < 810) {
      stop("updateOptimizationSettings: renewable.generation.modelling parameter is only available if using Antares >= 8.1.0", call. = FALSE)
    }
    dico_optimization <- dicoOptimizationSettings(arg = "renewable.generation.modelling")
    assertthat::assert_that(renewable.generation.modelling %in% dico_optimization[["values"]],
                            msg = paste0(renewable.generation.modelling, bad_value_msg)
                            )
  }
  
  if (!is.null(day.ahead.reserve.management)) {
    dico_optimization <- dicoOptimizationSettings(arg = "day.ahead.reserve.management")
    assertthat::assert_that(day.ahead.reserve.management %in% dico_optimization[["values"]],
                            msg = paste0(day.ahead.reserve.management, bad_value_msg)
                           )
  }
  
  if (!is.null(hydro.heuristic.policy)) {
    dico_optimization <- dicoOptimizationSettings(arg = "hydro.heuristic.policy")
    assertthat::assert_that(hydro.heuristic.policy %in% dico_optimization[["values"]],
                            msg = paste0(hydro.heuristic.policy, bad_value_msg)
                            )
  }
  
  if (!is.null(hydro.pricing.mode)) {
    dico_optimization <- dicoOptimizationSettings(arg = "hydro.pricing.mode")
    assertthat::assert_that(hydro.pricing.mode %in% dico_optimization[["values"]],
                            msg = paste0(hydro.pricing.mode, bad_value_msg)
                           )
  }
  
  if (!is.null(accurate.shave.peaks.include.short.term.storage)) {
    dico_optimization <- dicoOptimizationSettings(arg = "accurate.shave.peaks.include.short.term.storage")
    assertthat::assert_that(accurate.shave.peaks.include.short.term.storage %in% dico_optimization[["values"]],
                            msg = paste0(accurate.shave.peaks.include.short.term.storage, bad_value_msg)
                            )
  }
  
  new_params_optimization <- dropNulls(list(
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
    include.exportmps = include.exportmps,
    solver.log = solver.log,
    include.exportstructure = include.exportstructure,
    include.unfeasible.problem.behavior = include.unfeasible.problem.behavior
  ))
  for (i in seq_along(new_params_optimization)) {
    new_params_optimization[[i]] <- as.character(new_params_optimization[[i]])
    names(new_params_optimization)[i] <- dicoOptimizationSettings(names(new_params_optimization)[i])[["property"]]
  }
  
  new_params_others <- dropNulls(list(
    power.fluctuations = power.fluctuations,
    shedding.strategy = shedding.strategy,
    shedding.policy = shedding.policy,
    unit.commitment.mode = unit.commitment.mode,
    number.of.cores.mode = number.of.cores.mode,
    renewable.generation.modelling = renewable.generation.modelling,
    day.ahead.reserve.management = day.ahead.reserve.management,
    hydro.heuristic.policy = hydro.heuristic.policy,
    hydro.pricing.mode = hydro.pricing.mode,
    accurate.shave.peaks.include.short.term.storage = accurate.shave.peaks.include.short.term.storage
  ))
  for (i in seq_along(new_params_others)) {
    new_params_others[[i]] <- as.character(new_params_others[[i]])
    names(new_params_others)[i] <- dicoOptimizationSettings(names(new_params_others)[i])[["property"]]
  }
  
  
  # API block
  if (is_api_study(opts)) {
    
    if (length(new_params_optimization) > 0) {
      writeIni(
        listData = new_params_optimization,
        pathIni = "settings/generaldata/optimization",
        opts = opts
      )
    }
    
    if (length(new_params_others) > 0) {
      writeIni(
        listData = new_params_others,
        pathIni = "settings/generaldata/other preferences",
        opts = opts
      )
    }
    
    return(update_api_opts(opts))
  }
  
  
  # read
  generaldatapath <- file.path(opts$studyPath, "settings", "generaldata.ini")
  generaldata <- readIniFile(file = generaldatapath)
  
  # previous parameters
  l_optimization <- generaldata$optimization
  l_others <- generaldata$`other preferences`
  
  
  l_optimization <- utils::modifyList(x = l_optimization, val = new_params_optimization)
  l_others <- utils::modifyList(x = l_others, val = new_params_others)
  
  generaldata$optimization <- l_optimization
  generaldata$`other preferences` <- l_others
  
  
  # write
  writeIni(
    listData = generaldata,
    pathIni = generaldatapath,
    overwrite = TRUE
  )
  
  # Maj simulation
  suppressWarnings({
    res <- antaresRead::setSimulationPath(path = opts$studyPath, simulation = "input")
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
  
  if (length(arg) > 1) {
    stop("'arg' must be length one")
  }
  
  # properties with NULL as values have a more complex logic.
  # So they are still computed in updateOptimizationSettings()
  antares_params <- list(
    "simplex.range" = list("property" = "simplex-range",
                           "values" = c("week", "day")
                          ),
    "transmission.capacities" = list("property" = "transmission-capacities",
                                     "values" = NULL
                                    ),
    "include.constraints" = list("property" = "include-constraints",
                                 "values" = c("true", "false")
                                 ),
    "include.hurdlecosts" = list("property" = "include-hurdlecosts",
                                 "values" = c("true", "false")
                                ),
    "include.tc.min.stable.power" = list("property" = "include-tc-minstablepower",
                                         "values" = c("true", "false")
                                        ),
    "include.tc.min.up.down.time" = list("property" = "include-tc-min-ud-time",
                                         "values" = c("true", "false")
                                        ),
    "include.dayahead" = list("property" = "include-dayahead",
                              "values" = c("true", "false")
                             ),
    "include.strategicreserve" = list("property" = "include-strategicreserve",
                                      "values" = c("true", "false")
                                     ),
    "include.spinningreserve" = list("property" = "include-spinningreserve",
                                     "values" = c("true", "false")
                                    ),
    "include.primaryreserve" = list("property" = "include-primaryreserve",
                                    "values" = c("true", "false")
                                   ),
    "include.exportmps" = list("property" = "include-exportmps",
                               "values" = NULL
                              ),
    "power.fluctuations" = list("property" = "power-fluctuations",
                                "values" = c("free modulations", "minimize excursions", "minimize ramping")
                               ),
    "shedding.strategy" = list("property" = "shedding-strategy",
                               "values" = c("share margins")
                              ),
    "shedding.policy" = list("property" = "shedding-policy",
                             "values" = NULL
                            ),
    "unit.commitment.mode" = list("property" = "unit-commitment-mode",
                                  "values" = c("fast", "accurate", "milp")
                                 ),
    "number.of.cores.mode" = list("property" = "number-of-cores-mode",
                                  "values" = c("minimum", "low", "medium", "high", "maximum")
                                 ),
    "renewable.generation.modelling" = list("property" = "renewable-generation-modelling",
                                            "values" = c("aggregated", "clusters")
                                           ),
    "day.ahead.reserve.management" = list("property" = "day-ahead-reserve-management",
                                          "values" = c("global")
                                         ),
    "solver.log" = list("property" = "solver-log",
                        "values" = c("true", "false")
                       ),
    "include.exportstructure" = list("property" = "include-exportstructure",
                                     "values" = c("true", "false")
                                    ),
    "include.unfeasible.problem.behavior" = list("property" = "include-unfeasible-problem-behavior",
                                                 "values" = c("warning-dry", "warning-verbose", "error-dry", "error-verbose")
                                                ),
    "hydro.heuristic.policy" = list("property" = "hydro-heuristic-policy",
                                    "values" = c("accommodate rule curves", "maximize generation")
                                   ),
    "hydro.pricing.mode" = list("property" = "hydro-pricing-mode",
                                "values" = c("fast", "accurate")
                               ),
    "accurate.shave.peaks.include.short.term.storage" = list("property" = "accurate-shave-peaks-include-short-term-storage",
                                                             "values" = c("true", "false")
                                                            )
  )
  
  antares_params[[arg]]
}

