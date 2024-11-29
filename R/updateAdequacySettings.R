#' @title Update adequacy parameters of an Antares study
#' 
#' @description 
#' `r antaresEditObject:::badge_api_ok()`
#' 
#' Update adequacy parameters of an Antares study
#' 
#'
#' @param include_adq_patch Logical. If TRUE, will run Adequacy Patch
#' @param set_to_null_ntc_from_physical_out_to_physical_in_for_first_step Logical. default to TRUE
#' @param set_to_null_ntc_between_physical_out_for_first_step Logical. default to TRUE
#' @param include_hurdle_cost_csr Logical. default to FALSE
#' @param check_csr_cost_function Logical. default to FALSE
#' @param enable_first_step Logical. default to TRUE
#' @param price_taking_order Character. can take values DENS (default value) and Load.
#' @param threshold_initiate_curtailment_sharing_rule Double. default to 0.0
#' @param threshold_display_local_matching_rule_violations Double. default to 0.0
#' @param threshold_csr_variable_bounds_relaxation Integer. default to 3
#' 
#' @template opts
#' 
#' @export
#'
#' @importFrom assertthat assert_that
#'
#' @examples
#' \dontrun{
#'
#' updateAdequacySettings(
#'   include_adq_patch = TRUE,
#'   set_to_null_ntc_from_physical_out_to_physical_in_for_first_step = TRUE,
#'   set_to_null_ntc_between_physical_out_for_first_step = TRUE
#' )
#' 
#' }
#'
updateAdequacySettings <- function(include_adq_patch = NULL,
                                   set_to_null_ntc_from_physical_out_to_physical_in_for_first_step = NULL,
                                   set_to_null_ntc_between_physical_out_for_first_step = NULL,
                                   include_hurdle_cost_csr = NULL,
                                   check_csr_cost_function = NULL,
                                   enable_first_step = NULL,
                                   price_taking_order = NULL,
                                   threshold_initiate_curtailment_sharing_rule = NULL,
                                   threshold_display_local_matching_rule_violations = NULL,
                                   threshold_csr_variable_bounds_relaxation = NULL,
                                   opts = antaresRead::simOptions()) {
  
  assertthat::assert_that(inherits(opts, "simOptions"))
  
  if (opts[["antaresVersion"]] < 830) {
    stop("This function is only available for studies v8.3 or higher.")
  }
  
  new_params <- list(
    "include_adq_patch" = include_adq_patch,
    "set_to_null_ntc_from_physical_out_to_physical_in_for_first_step" = set_to_null_ntc_from_physical_out_to_physical_in_for_first_step,
    "set_to_null_ntc_between_physical_out_for_first_step" = set_to_null_ntc_between_physical_out_for_first_step,
    "include_hurdle_cost_csr" = include_hurdle_cost_csr,
    "check_csr_cost_function" = check_csr_cost_function,
    "enable_first_step" = enable_first_step,
    "price_taking_order" = price_taking_order,
    "threshold_initiate_curtailment_sharing_rule" = threshold_initiate_curtailment_sharing_rule,
    "threshold_display_local_matching_rule_violations" = threshold_display_local_matching_rule_violations,
    "threshold_csr_variable_bounds_relaxation" = threshold_csr_variable_bounds_relaxation
  )
  
  new_params <- dropNulls(x = new_params)
  
  if (opts[["antaresVersion"]] < 850) {
    properties_850 <- c("price_taking_order",
                        "include_hurdle_cost_csr",
                        "check_csr_cost_function",
                        "threshold_initiate_curtailment_sharing_rule",
                        "threshold_display_local_matching_rule_violations",
                        "threshold_csr_variable_bounds_relaxation")
    new_params <- new_params[!names(new_params) %in% properties_850]
  }
  
  if (opts[["antaresVersion"]] >= 860) {
    if ("enable_first_step" %in% names(new_params)) {
      message("Property enable_first_step is disabled for the moment. Set to FALSE.\n")
      new_params[["enable_first_step"]] <- FALSE
    }
  }
  
  new_params <- lapply(X = new_params, FUN = .format_ini_rhs)
  names(new_params) <- sapply(names(new_params), dicoAdequacySettings, USE.NAMES = FALSE)
  
  res <- update_generaldata_by_section(opts = opts, section = "adequacy patch", new_params = new_params)
  
  invisible(res)
}


#' @title Read adequacy patch config.yml into Antares (v8.5+)
#' 
#' @description 
#' Use this function to load config.yml used in older Antares versions for adequacy patch.
#' Areas in config will be updated to be included in adequacy patch perimeter.
#' 
#' @param opts List. study options.
#' @param path Character. path to config.yml. Default points to "/user/adequacypatch/" in study
#' 
#' @importFrom yaml read_yaml
#' @export
#'
#' @seealso
#' \code{\link{updateAdequacySettings}}
#' 
convertConfigToAdq <- function(opts = simOptions(), path = "default"){
  configAdq <- ifelse(path == "default",
                      file.path(opts$studyPath, "user", "adequacypatch", "config.yml"),
                      path)
  if (!file.exists(configAdq)) stop("Config.yml not found in selected path.")
  
  config <- read_yaml(configAdq, fileEncoding = "UTF-8", text)
  
  pathOut <- character(0)
  #temporarily switch to mode "input" if necessary
  if (opts$mode != "input"){
    pathOut <- opts$simPath
    setSimulationPath(opts$studyPath, "input")
  }
  lapply(setdiff(config$areas, config$excluded_areas),
         editArea, adequacy = adequacyOptions(adequacy_patch_mode = "inside"))
  if (length(pathOut) > 0) setSimulationPath(pathOut)
}


#' Correspondence between arguments of \code{updateAdequacySettings} and actual Antares parameters.
#' 
#' @param arg An argument from function \code{updateAdequacySettings}.
#'
#' @return The corresponding Antares general parameter.
#' 
#' @export
#' 
#' @examples 
#' dicoAdequacySettings("threshold_initiate_curtailment_sharing_rule")
dicoAdequacySettings <- function(arg) {
  
  if (length(arg) > 1) { 
    stop("'arg' must be length one")
  }
  
  antares_params <- as.list(c("include-adq-patch", "set-to-null-ntc-from-physical-out-to-physical-in-for-first-step",
                             "set-to-null-ntc-between-physical-out-for-first-step", "include-hurdle-cost-csr",
                             "check-csr-cost-function", "enable-first-step",
                             "price-taking-order", "threshold-initiate-curtailment-sharing-rule",
                             "threshold-display-local-matching-rule-violations", "threshold-csr-variable-bounds-relaxation"
                             )
                          )
  
  names(antares_params) <- c("include_adq_patch", "set_to_null_ntc_from_physical_out_to_physical_in_for_first_step",
                             "set_to_null_ntc_between_physical_out_for_first_step", "include_hurdle_cost_csr",
                             "check_csr_cost_function", "enable_first_step",
                             "price_taking_order", "threshold_initiate_curtailment_sharing_rule",
                             "threshold_display_local_matching_rule_violations", "threshold_csr_variable_bounds_relaxation"
                             )
  
  return(antares_params[[arg]])
}
