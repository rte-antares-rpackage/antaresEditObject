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
  if (!identical(areas, sort(areas))) {
    from <- areas[2]
    to <- areas[1]
  }
  
  # check version
  v7 <- is_antares_v7(opts)
  v820 <- is_antares_v820(opts)
  
  # check number of columns in datalink according to antares version
  if (!is.null(dataLink)) {
    if (v820) {
      assertthat::assert_that(ncol(dataLink) == 8 | ncol(dataLink) == 6)
    } else if (v7) {
      assertthat::assert_that(ncol(dataLink) == 8)
    } else {
      assertthat::assert_that(ncol(dataLink) == 5)
    }
  }
  
  # tsLink should be provided with an even number of columns and only with antares >= 820
  if (!is.null(tsLink)) {
    if (v820) {
      stopifnot(
        "tsLink must have an even number of columns" = identical(ncol(tsLink) %% 2, 0)
      )
    } else {
      warning("tsLink will be ignored since Antares version < 820.", call. = FALSE)
    }
  }
  
  
  # set initialization data if not provided
  if (is.null(dataLink)) {
    if (v820) {
      dataLink <- matrix(data = rep(0, 8760*6), ncol = 6)
    } else if (v7) {
      dataLink <- matrix(data = c(rep(1, 8760*2), rep(0, 8760*6)), ncol = 8)
    } else {
      dataLink <- matrix(data = c(rep(1, 8760*2), rep(0, 8760*3)), ncol = 5)
    }
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
  direct <- seq_len(NCOL(tsLink) / 2)
  indirect <- setdiff(seq_len(NCOL(tsLink)), seq_len(NCOL(tsLink) / 2))
  
  # correct column order for antares < 820
  if (!v820) {
    if (!identical(areas, sort(areas))) {
      dataLink[, 1:2] <- dataLink[, 2:1]
      if (v7) {
        dataLink[, 3:4] <- dataLink[, 4:3]
      } else {
        dataLink[, 4:5] <- dataLink[, 5:4]
      }
    }
  }
  
  
  # API block
  if (is_api_study(opts)) {
    cmd <- api_command_generate(
      action = "create_link",
      area1 = from,
      area2 = to,
      parameters = if (is_different(propertiesLink, propertiesLinkOptions())) propertiesLink else NULL,
      series = dataLink
    )
    api_command_register(cmd, opts = opts)
    `if`(
      should_command_be_executed(opts), 
      api_command_execute(cmd, opts = opts, text_alert = "{.emph create_link}: {msg_api}"),
      cli_command_registered("create_link")
    )
    
    if (v820) {
      cmd <- api_command_generate(
        action = "replace_matrix",
        target = sprintf("input/links/%s/capacities/%s", from, paste0(to, "_direct")),
        matrix = as.matrix(tsLink[, .SD, .SDcols = direct])
      )
      api_command_register(cmd, opts = opts)
      `if`(
        should_command_be_executed(opts), 
        api_command_execute(cmd, opts = opts, text_alert = "Writing transmission capacities (direct): {msg_api}"),
        cli_command_registered("replace_matrix")
      )
      
      cmd <- api_command_generate(
        action = "replace_matrix",
        target = sprintf("input/links/%s/capacities/%s", from, paste0(to, "_indirect")),
        matrix = as.matrix(tsLink[, .SD, .SDcols = indirect])
      )
      api_command_register(cmd, opts = opts)
      `if`(
        should_command_be_executed(opts), 
        api_command_execute(cmd, opts = opts, text_alert = "Writing transmission capacities (indirect): {msg_api}"),
        cli_command_registered("replace_matrix")
      )
    }
    
    return(invisible(opts))
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
#' @param filter_synthesis Output synthesis.
#' @param filter_year_by_year Output year-by-year.
#'
#' @return A named list that can be used in [createLink()].
#' @export
#'
#' @examples
#' propertiesLinkOptions(hurdles_cost = TRUE)
propertiesLinkOptions <- function(hurdles_cost = FALSE, 
                                  transmission_capacities = "enabled",
                                  asset_type = "ac",
                                  display_comments = TRUE,
                                  filter_synthesis = c("hourly", "daily", "weekly", "monthly", "annual"),
                                  filter_year_by_year = c("hourly", "daily", "weekly", "monthly", "annual")) {
  list(
    `hurdles-cost` = hurdles_cost,
    `transmission-capacities` = transmission_capacities,
    `asset-type` = asset_type,
    `display-comments` = display_comments,
    `filter-synthesis` = paste(filter_synthesis, collapse = ", "),
    `filter-year-by-year` = paste(filter_year_by_year, collapse = ", ")
  )
}

