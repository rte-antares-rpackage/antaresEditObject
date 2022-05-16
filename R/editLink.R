#' @title Edit a link between two areas
#' 
#' @description 
#' `r antaresEditObject::badge_api_ok()`
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
      cmd <- api_command_generate(
        action = "replace_matrix",
        target = sprintf("input/links/%s/%s", from, to),
        matrix = dataLink
      )
      api_command_register(cmd, opts = opts)
      `if`(
        should_command_be_executed(opts), 
        api_command_execute(cmd, opts = opts, text_alert = "Update link's series: {msg_api}"),
        cli_command_registered("replace_matrix")
      )
    }
    
    return(invisible(opts))
  }
  
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
  
  if (v820 & (!is.null(tsLink) && ncol(dataLink) == 8)) {
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
  if (!identical(areas, sort(areas))) {
    from <- areas[2]
    to <- areas[1]
  }
  
  # Input path
  inputPath <- opts$inputPath
  assertthat::assert_that(!is.null(inputPath) && file.exists(inputPath))
  
  check_area_name(from, opts)
  check_area_name(to, opts)
  
  # Previous links
  prev_links <- readIniFile(
    file = file.path(inputPath, "links", from, "properties.ini")
  )
  
  prev_links[[to]] <- modifyList(x = prev_links[[to]], val = propertiesLink)
  
  # Write INI file
  writeIni(
    listData = prev_links, 
    pathIni = file.path(inputPath, "links", from, "properties.ini"),
    overwrite = TRUE
  )
  
  if (!is.null(dataLink)) {
    if (v820) {
      utils::write.table(
        x = dataLink, 
        row.names = FALSE, 
        col.names = FALSE,
        sep = "\t",
        file = file.path(inputPath, "links", from, paste0(to, "_parameters.txt"))
      )
    } else {
      if (!identical(areas, sort(areas))) {
        dataLink[, 1:2] <- dataLink[, 2:1]
        dataLink[, 4:5] <- dataLink[, 5:4]
      }
      
      utils::write.table(
        x = dataLink,
        row.names = FALSE, 
        col.names = FALSE, 
        sep = "\t",
        file = file.path(inputPath, "links", from, paste0(to, ".txt"))
      )
    }
  }
  
  
  if (!is.null(tsLink)) {
    stopifnot(
      "tsLink must have an even number of columns" = identical(ncol(tsLink) %% 2, 0)
    )
    if (v820) {
      dir.create(file.path(inputPath, "links", from, "capacities"), showWarnings = FALSE)
      direct <- seq_len(NCOL(tsLink) / 2)
      indirect <- setdiff(seq_len(NCOL(tsLink)), seq_len(NCOL(tsLink) / 2))
      utils::write.table(
        x = tsLink[, direct], 
        row.names = FALSE, 
        col.names = FALSE,
        sep = "\t",
        file = file.path(inputPath, "links", from, "capacities", paste0(to, "_direct.txt"))
      )
      utils::write.table(
        x = tsLink[, indirect], 
        row.names = FALSE, 
        col.names = FALSE,
        sep = "\t",
        file = file.path(inputPath, "links", from, "capacities", paste0(to, "_indirect.txt"))
      )
    } else {
      warning("tsLink will be ignored since Antares version < 820.", call. = FALSE)
    }
  }
  
  
  # Maj simulation
  suppressWarnings({
    res <- antaresRead::setSimulationPath(path = opts$studyPath, simulation = "input")
  })
  
  invisible(res)
}



