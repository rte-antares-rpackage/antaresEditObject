#' @title Edit a link between two areas
#' 
#' @description 
#' `r antaresEditObject:::badge_api_ok()`
#' 
#' Edit a link between two areas in an Antares study.
#' 
#'
#' @inheritParams createLink
#' @inheritParams propertiesLinkOptions
#' 
#' @template opts
#' 
#' @seealso [createLink()], [removeLink()]
#' 
#' @note See [createLink()] for more documentation
#'
#' @export
#' 
#' @importFrom assertthat assert_that
#' @importFrom stats setNames 
#' @importFrom utils read.table write.table modifyList
#'
#' @examples
#' \dontrun{
#' editLink(
#'   from = "area1",
#'   to = "area2",
#'   transmission_capacities = "infinite"
#' )
#' }
editLink <- function(from, 
                     to,
                     hurdles_cost = NULL, 
                     transmission_capacities = NULL, 
                     asset_type = NULL,
                     display_comments = NULL,
                     filter_synthesis = NULL,
                     filter_year_by_year = NULL, 
                     dataLink = NULL,
                     tsLink = NULL,
                     opts = antaresRead::simOptions()) {
  
  assertthat::assert_that(inherits(opts, "simOptions"))
  
  propertiesLink <- dropNulls(list(
    `hurdles-cost` = hurdles_cost,
    `transmission-capacities` = transmission_capacities,
    `asset-type` = asset_type,
    `display-comments` = display_comments,
    `filter-synthesis` = filter_synthesis,
    `filter-year-by-year` = filter_year_by_year
  ))
  
  # control areas name
  # can be with some upper case (list.txt)
  from <- tolower(from)
  to <- tolower(to)
  
  v7 <- is_antares_v7(opts)
  v820 <- is_antares_v820(opts)
  
  if (!is.null(dataLink)) {
    if (v820) {
      assertthat::assert_that(ncol(dataLink) == 8 | ncol(dataLink) == 6)
    } else if (v7) {
      assertthat::assert_that(ncol(dataLink) == 8)
    } else {
      assertthat::assert_that(ncol(dataLink) == 5)
    }
  }
  
  if (!is.null(tsLink)) {
    if (v820) {
      stopifnot(
        "tsLink must have an even number of columns: 2N" = identical(ncol(tsLink) %% 2, 0)
      )
    } else {
      warning("tsLink will be ignored since Antares version < 820.", call. = FALSE)
    }
  }
  
  if (v820 & (!is.null(dataLink) && ncol(dataLink) == 8)) {
    if (!is.null(tsLink)) {
      warning(
        "editLink: `tsLink` will be ignored since `dataLink` is provided with 8 columns."
      )
    }
    tsLink <- dataLink[, 1:2]
    dataLink <- dataLink[, -c( 1:2)]
  }
  
  # areas' order
  areas <- c(from, to)
  are_areas_sorted <- identical(areas, sort(areas))
  if (!are_areas_sorted) {
    from <- areas[2]
    to <- areas[1]
  }
  
  check_area_name(from, opts)
  check_area_name(to, opts)
  
  
  if (!is.null(tsLink)) {
    stopifnot(
      "tsLink must have an even number of columns" = identical(ncol(tsLink) %% 2, 0)
    )
    if (v820) {
      first_cols <- seq_len(NCOL(tsLink) / 2)
      last_cols <- setdiff(seq_len(NCOL(tsLink)), seq_len(NCOL(tsLink) / 2))
      if (are_areas_sorted) {
        direct <- first_cols
        indirect <- last_cols
      } else {
        direct <- last_cols
        indirect <- first_cols
      }
      tsLink <- data.table::as.data.table(tsLink)
    } else {
      warning("tsLink will be ignored since Antares version < 820.", call. = FALSE)
    }
  }
  
  
  # API block
  if (is_api_study(opts)) {
    
    # Link properties
    if (length(propertiesLink) > 0) {
      actions <- lapply(
        X = seq_along(propertiesLink),
        FUN = function(i) {
          list(
            target = sprintf("input/links/%s/properties/%s/%s", from, to, names(propertiesLink)[i]),
            data = propertiesLink[[i]]
          )
        }
      )
      actions <- setNames(actions, rep("update_config", length(actions)))
      cmd <- do.call(api_commands_generate, actions)
      api_command_register(cmd, opts = opts)
      `if`(
        should_command_be_executed(opts), 
        api_command_execute(cmd, opts = opts, text_alert = "Update link's properties: {msg_api}"),
        cli_command_registered("update_config")
      )
    }
    
    if (!is.null(dataLink)) {
      if (v820){
        cmd <- api_command_generate(
          action = "replace_matrix",
          target = sprintf("input/links/%s/%s_parameters", from, to),
          matrix = dataLink
        )
      } else {
        cmd <- api_command_generate(
          action = "replace_matrix",
          target = sprintf("input/links/%s/%s", from, to),
          matrix = dataLink
        )
      }
      api_command_register(cmd, opts = opts)
      `if`(
        should_command_be_executed(opts), 
        api_command_execute(cmd, opts = opts, text_alert = "Update link's series: {msg_api}"),
        cli_command_registered("replace_matrix")
      )
    }
    
    if (v820 && !is.null(tsLink)) {
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
  
  if (!to %in% names(prev_links))
    stop(paste("Link to", to, "doesn't exist"))
  
  prev_links[[to]] <- modifyList(x = prev_links[[to]], val = propertiesLink)
  
  # Write INI file
  writeIni(
    listData = prev_links, 
    pathIni = file.path(inputPath, "links", from, "properties.ini"),
    overwrite = TRUE
  )
  
  if (!is.null(dataLink)) {
    if (v820) {
      data.table::fwrite(
        x = data.table::as.data.table(dataLink), 
        row.names = FALSE, 
        col.names = FALSE,
        sep = "\t",
        scipen = 12,
        file = file.path(inputPath, "links", from, paste0(to, "_parameters.txt"))
      )
    } else {
      if (!are_areas_sorted) {
        dataLink[, 1:2] <- dataLink[, 2:1]
        dataLink[, 4:5] <- dataLink[, 5:4]
      }
      data.table::fwrite(
        x = data.table::as.data.table(dataLink),
        row.names = FALSE, 
        col.names = FALSE, 
        sep = "\t",
        scipen = 12,
        file = file.path(inputPath, "links", from, paste0(to, ".txt"))
      )
    }
  }
  
  
  if (!is.null(tsLink)) {
    if (v820) {
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
    }
  }
  
  
  # Maj simulation
  suppressWarnings({
    res <- antaresRead::setSimulationPath(path = opts$studyPath, simulation = "input")
  })
  
  invisible(res)
}
