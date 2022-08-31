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
                                   opts = antaresRead::simOptions()) {
  
  assertthat::assert_that(inherits(opts, "simOptions"))
  
  # API block
  if (is_api_study(opts)) {
    
    writeIni(
      listData = list(
        `include-adq-patch` = include_adq_patch,
        `set-to-null-ntc-from-physical-out-to-physical-in-for-first-step` = set_to_null_ntc_from_physical_out_to_physical_in_for_first_step,
        `set-to-null-ntc-between-physical-out-for-first-step` = set_to_null_ntc_between_physical_out_for_first_step
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
  general$`adequacy patch` <- adequacy
  
  writeIni(listData = general, pathIni = pathIni, overwrite = TRUE)

  # Maj simulation
  suppressWarnings({
    res <- antaresRead::setSimulationPath(path = opts$studyPath, simulation = "input")
  })
  
  invisible(res)
}
