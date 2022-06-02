
#' @title Write Misc Gen data
#' 
#' @description 
#' `r antaresEditObject::badge_api_ok()`
#'
#' @param data Data to write.
#' @param area Name of the area for which to write data.
#' @template opts
#'
#' @export
#'
#' @examples
#' \dontrun{
#' 
#' writeMiscGen(matrix(data = c(rep(0, 8760 * 7), rep(-100000, 8760)), ncol = 8), "area1")
#' 
#' }
writeMiscGen <- function(data, 
                         area, 
                         opts = antaresRead::simOptions()) {
  assertthat::assert_that(inherits(opts, "simOptions"))
  
  if (is_api_study(opts)) {
    cmd <- api_command_generate(
      action = "replace_matrix",
      target = sprintf("input/misc-gen/miscgen-%s", area),
      matrix = data
    )
    api_command_register(cmd, opts = opts)
    `if`(
      should_command_be_executed(opts), 
      api_command_execute(cmd, opts = opts, text_alert = "Writing misc-gen data: {msg_api}"),
      cli_command_registered("replace_matrix")
    )
    return(update_api_opts(opts))
  }
  
  inputPath <- opts$inputPath
  check_area_name(area, opts)
  path <- file.path(inputPath, "misc-gen", paste0("miscgen-", tolower(area), ".txt"))
  utils::write.table(
    x = data,
    file = path,
    row.names = FALSE,
    col.names = FALSE,
    sep = "\t"
  )
  
  suppressWarnings({
    res <- antaresRead::setSimulationPath(path = opts$studyPath, simulation = "input")
  })
  
  invisible(res)
}
