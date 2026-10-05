#' @title Create a link between two areas
#' 
#' @description 
#' `r antaresEditObject:::badge_api_ok()`
#' 
#' Create a new link between two areas in an Antares study.
#' 
#'
#' @param from,to The two areas linked together.
#' @param propertiesLink a named list containing the link properties, e.g. hurdles-cost
#'  or transmission-capacities for example. See [propertiesLinkOptions()].
#' @param dataLink See Details section below.
#' @param tsLink Transmission capacities time series. First N columns are direct TS, following N are indirect ones.
#' @param overwrite Logical, overwrite the previous between the two areas if exist
#'  
#' @template opts
#' 
#' @seealso [editLink()], [removeLink()]
#'  
#' @note In Antares, areas are sorted in alphabetical order to establish links between.
#' For example, link between "fr" and "be" will appear under "be". 
#' So the areas are sorted before creating the link between them, and `dataLink` is
#' rearranged to match the new order.
#' 
#' @details The eight potential times-series are:
#' 
#' * **NTC direct** : the upstream-to-downstream capacity, in MW. Default to `1`.
#' * **NTC indirect** : the downstream-to-upstream capacity, in MW. Default to `1`.
#' * **Hurdle cost direct** : an upstream-to-downstream transmission fee, in euro/MWh. Default to `0`.
#' * **Hurdle cost indirect** : a downstream-to-upstream transmission fee, in euro/MWh. Default to `0`.
#' * **Impedances** : virtual impedances that are used in economy simulations to give a physical meaning to raw outputs,
#'  when no binding constraints have been defined to enforce Kirchhoff's laws. Default to `0`.
#' * **Loop flow** : amount of power flowing circularly though the grid when all "nodes" are perfectly balanced (no import and no export). Default to `0`.
#' * **PST min** : lower bound of phase-shifting that can be reached by a PST installed on the link, if any. Default to `0`.
#' * **PST max** : upper bound of phase-shifting that can be reached by a PST installed on the link, if any. Default to `0`.
#' 
#' According to Antares version, usage may vary :
#' 
#' **< v7.0.0** : 5 first columns are used in the following order: NTC direct, NTC indirect, Impedances, Hurdle cost direct,
#' Hurdle cost indirect.
#' 
#' **>= v7.0.0** : 8 columns in order above are expected.
#' 
#' **>= v8.2.0** : there's 2 cases :
#'  * 8 columns are provided: 2 first are used in `tsLink`, other 6 are used for link data
#'  * 6 columns are provided: you must provide NTC data in `tsLink` argument.
#' 
#' 
#'
#' @export
#' 
#' @importFrom assertthat assert_that
#' @importFrom stats setNames
#' @importFrom utils read.table write.table
#' @importFrom data.table fwrite as.data.table
#' @importFrom antaresRead simOptions setSimulationPath readIniFile
#'
#' @examples
#' \dontrun{
#' 
#' library(antaresRead)
#' 
#' # Set simulation path
#' setSimulationPath(path = "PATH/TO/SIMULATION", simulation = "input")
#' 
#' # Create a link between two areas
#' createLink(from = "first_area", to  = "second_area")
#' 
#' }
createLink <- function(from,
                       to, 
                       propertiesLink = propertiesLinkOptions(), 
                       dataLink = NULL, 
                       tsLink = NULL,
                       overwrite = FALSE,
                       opts = antaresRead::simOptions()) {
  
  assertthat::assert_that(inherits(opts, "simOptions"))
  
  # control areas name
  # can be with some upper case (list.txt)
  from <- tolower(from)
  to <- tolower(to)
  "check_area_name"(from, opts)
  check_area_name(to, opts)
  # areas' order
  areas <- c(from, to)
  are_areas_sorted <- identical(areas, sort(areas))
  if (!are_areas_sorted) {
    from <- areas[2]
    to <- areas[1]
  }
  
  # check version
  v7 <- is_antares_v7(opts)
  v820 <- is_antares_v820(opts)
  
  if (!is.null(dataLink)) {
    .control_dataLink_time_series_dimensions(dataLink = dataLink, v820 = v820, v7 = v7)
  }
  
  if (!is.null(tsLink)) {
    .control_tsLink_time_series_dimensions(tsLink = tsLink, v820 = v820)
  }
  
  # set initialization data if not provided
  if (is.null(dataLink)) {
    dataLink <- .initialize_dataLink_time_series(v820 = v820, v7 = v7)
  } else {
    if (v820 & ncol(dataLink) == 8) {
      if (!is.null(tsLink)) {
        warning(
          "createLink: `tsLink` will be ignored since `dataLink` is provided with 8 columns."
        )
      }
      tsLink <- dataLink[, 1:2]
      dataLink <- dataLink[, -c( 1:2)]
    }
  }
  
  # set transmission capacities time series if not provided
  if (is.null(tsLink)) {
    tsLink <- matrix(data = rep(0, 8760*2), ncol = 2)
  }
  tsLink <- data.table::as.data.table(tsLink)
  first_cols <- seq_len(NCOL(tsLink) / 2)
  last_cols <- setdiff(seq_len(NCOL(tsLink)), seq_len(NCOL(tsLink) / 2))
  if (are_areas_sorted) {
    direct <- first_cols
    indirect <- last_cols
  } else {
    direct <- last_cols
    indirect <- first_cols
  }
  
  # correct column order for antares < 820
  if (!v820) {
    if (!are_areas_sorted) {
      dataLink[, 1:2] <- dataLink[, 2:1]
      if (v7) {
        dataLink[, 3:4] <- dataLink[, 4:3]
      } else {
        dataLink[, 4:5] <- dataLink[, 5:4]
      }
    }
  }
  
  
  # API block
  if (is_api_study(opts = opts)) {
    if (!is_api_mocked(opts = opts)) {
      body <- transform_list_to_json_for_createLink(link_parameters = propertiesLink,
                                                    from = from,
                                                    to = to
                                                    )      
      result <- api_post(opts = opts,
                         endpoint = file.path(opts[["study_id"]], "links"),
                         body = body,
                         encode = "raw")
      cli::cli_alert_success("Endpoint Create link success")
      
      .replace_matrix_link(from = from,
                           to = to,
                           ts_parameters = dataLink,
                           ts_direct = tsLink[, .SD, .SDcols = direct],
                           ts_indirect = tsLink[, .SD, .SDcols = indirect],
                           opts = opts
                           )
    }
    return(update_api_opts(opts))
  }
  

  # Input path
  inputPath <- opts$inputPath
  assertthat::assert_that(!is.null(inputPath) && file.exists(inputPath))
  
  # Previous links
  prev_links <- readIniFile(
    file = file.path(inputPath, "links", from, "properties.ini")
  )
  
  if (to %in% names(prev_links) & !overwrite)
    stop(paste("Link to", to, "already exist"))
  
  if (to %in% names(prev_links) & overwrite) {
    opts <- removeLink(from = from, to = to, opts = opts)
    prev_links <- readIniFile(
      file = file.path(inputPath, "links", from, "properties.ini")
    )
  }
  
  # propLink <- list(propertiesLink)
  
  prev_links[[to]] <- propertiesLink
 
  
  writeIni(
    listData = prev_links, # c(prev_links, stats::setNames(propLink, to)),
    pathIni = file.path(inputPath, "links", from, "properties.ini"),
    overwrite = TRUE
  )
  
  if (v820) {
    data.table::fwrite(
      x = data.table::as.data.table(dataLink), 
      row.names = FALSE, 
      col.names = FALSE,
      sep = "\t",
      scipen = 12,
      file = file.path(inputPath, "links", from, paste0(to, "_parameters.txt"))
    )
    dir.create(file.path(inputPath, "links", from, "capacities"), showWarnings = FALSE)
    data.table::fwrite(
      x = tsLink[, .SD, .SDcols = direct], 
      row.names = FALSE, 
      col.names = FALSE,
      sep = "\t",
      scipen = 12,
      file = file.path(inputPath, "links", from, "capacities", paste0(to, "_direct.txt"))
    )
    data.table::fwrite(
      x = tsLink[, .SD, .SDcols = indirect], 
      row.names = FALSE, 
      col.names = FALSE,
      sep = "\t",
      scipen = 12,
      file = file.path(inputPath, "links", from, "capacities", paste0(to, "_indirect.txt"))
    )
  } else {
    data.table::fwrite(
      x = dataLink, 
      row.names = FALSE, 
      col.names = FALSE,
      sep = "\t",
      scipen = 12,
      file = file.path(inputPath, "links", from, paste0(to, ".txt"))
    )
  }
  
  # Maj simulation
  suppressWarnings({
    res <- antaresRead::setSimulationPath(path = opts$studyPath, simulation = "input")
  })
  
  invisible(res)
}


#' Properties for creating a link
#'
#' @param hurdles_cost Logical, which is used to state whether (linear)
#'  transmission fees should be taken into account or not in economy and adequacy simulations
#' @param transmission_capacities Character, one of `enabled`, `ignore` or `infinite`, which is used to state whether 
#' the capacities to consider are those indicated in 8760-hour arrays or 
#' if zero or infinite values should be used instead (actual values / set to zero / set to infinite)
#' @param asset_type Character, one of `ac`, `dc`, `gas`, `virt` or `other`. Used to
#'   state whether the link is either an AC component (subject to Kirchhoff’s laws), a DC component, 
#'   or another type of asset.
#' @param display_comments Logical, display comments or not.
#' @param filter_synthesis Character, vector of time steps used in the output synthesis, among `hourly`, `daily`, `weekly`, `monthly`, and `annual`
#' @param filter_year_by_year Character, vector of time steps used in the output year-by-year, among `hourly`, `daily`, `weekly`, `monthly`, and `annual`
#' @param use_phase_shifter Logical.
#' @param loop_flow Logical.
#' @param colorr Integer, color of the line.
#' @param colorb Integer, color of the line.
#' @param colorg Integer, color of the line.
#' @param link_width Numeric, width of the line.
#' @param link_style Character, style of the line.
#'
#' @return A named list that can be used in [createLink()].
#' @export
#'
#' @examples
#' \dontrun{
#' propertiesLinkOptions(
#'   hurdles_cost = TRUE,
#'   filter_synthesis=c("hourly","daily"),
#'   filter_year_by_year=c("weekly","monthly")
#' )
#' }
propertiesLinkOptions <- function(hurdles_cost = FALSE, 
                                  transmission_capacities = "enabled",
                                  asset_type = "ac",
                                  display_comments = TRUE,
                                  filter_synthesis = c("hourly", "daily", "weekly", "monthly", "annual"),
                                  filter_year_by_year = c("hourly", "daily", "weekly", "monthly", "annual"),
                                  use_phase_shifter = FALSE,
                                  loop_flow = FALSE,
                                  colorr = 112,
                                  colorb = 112,
                                  colorg = 112,
                                  link_width = 1,
                                  link_style = "plain") {
  list(
    `hurdles-cost` = hurdles_cost,
    `transmission-capacities` = transmission_capacities,
    `asset-type` = asset_type,
    `display-comments` = display_comments,
    `filter-synthesis` = paste(filter_synthesis, collapse = ", "),
    `filter-year-by-year` = paste(filter_year_by_year, collapse = ", "),
    `use-phase-shifter` = use_phase_shifter,
    `loop-flow` = loop_flow,
    `colorr` = colorr,
    `colorb` = colorb,
    `colorg` = colorg,
    `link-width` = link_width,
    `link-style` = link_style
  )
}


#' @title Control the dimensions of the time series dataLink. 8760 rows and a specific number of columns expected.
#'
#' @param dataLink a time series for the link parameters
#' @param v820 logical, is study with Antares version >= 820 ?
#' @param v7 logical, is study with Antares version >= 700 ?
#'
#' @importFrom assertthat assert_that
#'
#' @keywords internal
#' @noRd
.control_dataLink_time_series_dimensions <- function(dataLink, v820, v7) {
  
  assert_that(nrow(dataLink) == 8760, msg = "dataLink is an hourly data and must have 8760 rows")
  if (v820) {
    assert_that(ncol(dataLink) == 8 | ncol(dataLink) == 6)
  } else if (v7) {
    assert_that(ncol(dataLink) == 8)
  } else {
    assert_that(ncol(dataLink) == 5)
  }
}


#' @title Control the dimensions of the time series tsLink. 8760 rows and even number of columns expected.
#'
#' @param tsLink a time series for the link capacities
#' @param v820 logical, is study with Antares version >= 820 ?
#'
#' @importFrom assertthat assert_that
#'
#' @keywords internal
#' @noRd
.control_tsLink_time_series_dimensions <- function(tsLink, v820) {
  
  if (v820) {
    stopifnot(
      "tsLink must have an even number of columns" = identical(ncol(tsLink) %% 2, 0)
    )
    assert_that(nrow(tsLink) == 8760, msg = "tsLink is an hourly data and must have 8760 rows")
  } else {
    warning("tsLink will be ignored since Antares version < 820.", call. = FALSE)
  }
}


#' @title Initialize the time series dataLink by version.
#'
#' @param v820 logical, is study with Antares version >= 820 ?
#' @param v7 logical, is study with Antares version >= 700 ?
#'
#' @keywords internal
#' @noRd
.initialize_dataLink_time_series <- function(v820, v7) {
  
  if (v820) {
    return(matrix(data = rep(0, 8760*6), ncol = 6))
  } else if (v7) {
    return(matrix(data = c(rep(1, 8760*2), rep(0, 8760*6)), ncol = 8))
  } else {
    return(matrix(data = c(rep(1, 8760*2), rep(0, 8760*3)), ncol = 5))
  }
}


#' Transform a user list to a json object to use in the endpoint of link creation
#'
#' @importFrom jsonlite toJSON
#' @importFrom assertthat assert_that
#'
#' @param link_parameters a list containing the metadata of the link to create.
#' @param from, to the two areas linked together.
#'
#' @return a json object
#' @noRd
transform_list_to_json_for_createLink <- function(link_parameters, from, to) {

  assertthat::assert_that(inherits(x = link_parameters, what = "list"))

  link_parameters <- list("area1" = from,
                          "area2" = to, 
                          "hurdlesCost" = link_parameters[["hurdles-cost"]],
                          "transmissionCapacities" = link_parameters[["transmission-capacities"]],
                          "assetType" = link_parameters[["asset-type"]],
                          "displayComments" = link_parameters[["display-comments"]],
                          "filterSynthesis" = link_parameters[["filter-synthesis"]],
                          "filterYearByYear" = link_parameters[["filter-year-by-year"]],
                          "usePhaseShifter" = link_parameters[["use-phase-shifter"]],
                          "loopFlow" = link_parameters[["loop-flow"]],
                          "colorr" = link_parameters[["colorr"]],
                          "colorb" = link_parameters[["colorb"]],
                          "colorg" = link_parameters[["colorg"]],
                          "linkWidth" = link_parameters[["link-width"]],
                          "linkStyle" = link_parameters[["link-style"]]
                          )
  
  link_parameters <- dropNulls(link_parameters)

  return(jsonlite::toJSON(link_parameters, auto_unbox = TRUE))
}


.generate_targets_createLink <- function(from, to, ts_parameters, ts_direct, ts_indirect, is_820) {
  
  if (is_820) {
    return(
      list(
        "parameters" = 
          list(
            "target" = sprintf("input/links/%s/%s", from, paste0(to, "_parameters")),
            "matrix" = as.matrix(ts_parameters)
              ),
        "direct" = 
          list(
            "target" = sprintf("input/links/%s/capacities/%s", from, paste0(to, "_direct")),
            "matrix" = as.matrix(ts_direct)
              ),
        "indirect" =  
          list(
            "target" = sprintf("input/links/%s/capacities/%s", from, paste0(to, "_indirect")),
            "matrix" = as.matrix(ts_indirect)
              )                       
      )
    )
  } else {
    return(
      list(
        "parameters" = 
          list(
            "target" = sprintf("input/links/%s/%s", from, to),
            "matrix" = as.matrix(ts_parameters)
              )                  
      )
    )
  }
}


.replace_matrix_link <- function(from, to, ts_parameters, ts_direct, ts_indirect, opts) {
  
  ts_link_params <- .generate_targets_createLink(from = from,
                                                 to = to, 
                                                 ts_parameters = ts_parameters,
                                                 ts_direct = ts_direct,
                                                 ts_indirect = ts_indirect,
                                                 is_820 = is_antares_v820(opts = opts)
                                                )
  
  actions <- lapply(
            X = seq_along(ts_link_params),
            FUN = function(i) {
              list(
                target = ts_link_params[[i]][["target"]],
                matrix = ts_link_params[[i]][["matrix"]]
              )
            }
  )
  actions <- setNames(actions, rep("replace_matrix", length(actions)))
  cmd <- do.call(api_commands_generate, actions)
  api_command_register(cmd, opts = opts)
  `if`(
    should_command_be_executed(opts = opts),
    api_command_execute(cmd, opts = opts, text_alert = "Writing links's time series: {msg_api}"),
    cli_command_registered("replace_matrix")
  )  
}
