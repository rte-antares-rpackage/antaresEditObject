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
#' @param price_taking_order Character. can take values DENS (default value) and Load.
#' @param include_hurdle_cost_csr Logical. default to FALSE
#' @param check_csr_cost_function Logical. default to FALSE
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
                                   price_taking_order = NULL,
                                   include_hurdle_cost_csr = NULL,
                                   check_csr_cost_function = NULL,
                                   threshold_initiate_curtailment_sharing_rule = NULL,
                                   threshold_display_local_matching_rule_violations = NULL,
                                   threshold_csr_variable_bounds_relaxation = NULL,
                                   opts = antaresRead::simOptions()) {
  
  if (opts$antaresVersion < 830) stop("This function is only available for studies v8.3 or higher.")
  
  assertthat::assert_that(inherits(opts, "simOptions"))
  
  # API block
  if (is_api_study(opts)) {
    
    writeIni(
      listData = list(
        `include-adq-patch` = include_adq_patch,
        `set-to-null-ntc-from-physical-out-to-physical-in-for-first-step` = set_to_null_ntc_from_physical_out_to_physical_in_for_first_step,
        `set-to-null-ntc-between-physical-out-for-first-step` = set_to_null_ntc_between_physical_out_for_first_step,
        `price-taking-order` = price_taking_order,
        `include-hurdle-cost-csr` = include_hurdle_cost_csr,
        `check-csr-cost-function` = check_csr_cost_function,
        `threshold-initiate-curtailment-sharing-rule` = threshold_initiate_curtailment_sharing_rule,
        `threshold-display-local-matching-rule-violations` = threshold_display_local_matching_rule_violations,
        `threshold-csr-variable-bounds-relaxation` = threshold_csr_variable_bounds_relaxation
      ),
      pathIni = "settings/generaldata/adequacy patch",
      opts = opts
    )
    
    return(update_api_opts(opts))
  }
  
  pathIni <- file.path(opts$studyPath, "settings", "generaldata.ini")
  general <- readIniFile(file = pathIni)
  
  adequacy <- general$`adequacy patch`
  if (!is.null(include_adq_patch))
    adequacy$`include-adq-patch` <- include_adq_patch
  if (!is.null(set_to_null_ntc_from_physical_out_to_physical_in_for_first_step))
    adequacy$`set-to-null-ntc-from-physical-out-to-physical-in-for-first-step` <- set_to_null_ntc_from_physical_out_to_physical_in_for_first_step
  if (!is.null(set_to_null_ntc_between_physical_out_for_first_step))
    adequacy$`set-to-null-ntc-between-physical-out-for-first-step` <- set_to_null_ntc_between_physical_out_for_first_step
  
  if (opts$antaresVersion >= 850){
    if (!is.null(price_taking_order))
      adequacy$`price-taking-order` <- price_taking_order
    if (!is.null(include_hurdle_cost_csr))
      adequacy$`include-hurdle-cost-csr` <- include_hurdle_cost_csr
    if (!is.null(check_csr_cost_function))
      adequacy$`check-csr-cost-function` <- check_csr_cost_function
    if (!is.null(threshold_initiate_curtailment_sharing_rule))
      adequacy$`threshold-initiate-curtailment-sharing-rule` <- threshold_initiate_curtailment_sharing_rule
    if (!is.null(threshold_display_local_matching_rule_violations))
      adequacy$`threshold-display-local-matching-rule-violations` <- threshold_display_local_matching_rule_violations
    if (!is.null(threshold_csr_variable_bounds_relaxation))
      adequacy$`threshold-csr-variable-bounds-relaxation` <- threshold_csr_variable_bounds_relaxation
  }

  general$`adequacy patch` <- adequacy
  
  writeIni(listData = general, pathIni = pathIni, overwrite = TRUE)

  # Maj simulation
  suppressWarnings({
    res <- antaresRead::setSimulationPath(path = opts$studyPath, simulation = "input")
  })
  
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
