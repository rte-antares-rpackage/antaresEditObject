
#' Write configuration options in file or API
#'
#' @param listData A `list` of configuration.
#' @param pathIni Path to config/ini file to read.
#' @template opts-arg
#' @param ... Additional arguments.
#' @param default_ext Default extension used for config files.
#'
#' @export
#' @name write-ini
#'
#' @examples
#'
#' \dontrun{
#' pathIni <- "D:/exemple_test/settings/generaldata.ini"
#' generalSetting <- antaresRead:::readIniFile(pathIni)
#' generalSetting$output$synthesis <- FALSE
#' writeIni(generalSetting, pathIni)
#' }
writeIni <- function(listData, pathIni, opts = antaresRead::simOptions(), ..., default_ext = ".ini") {
  assertthat::assert_that(inherits(opts, "simOptions"))
  args <- list(...)
  if (is_api_study(opts)) {
    if (endsWith(pathIni, default_ext))
      pathIni <- sub(sprintf("\\%s$", default_ext), "", x = pathIni)
    writeIniAPI(
      listData = listData,
      pathIni = pathIni,
      opts = opts
    )
  } else {
    if (!endsWith(pathIni, default_ext))
      pathIni <- paste0(pathIni, default_ext)
    if (!is_study_path(pathIni, opts))
      pathIni <- file.path(opts$studyPath, pathIni)
    writeIniFile(
      listData = listData,
      pathIni = pathIni,
      overwrite = isTRUE(args$overwrite)
    )
  }
}

#' @param listData \code{list}, modified list obtained by antaresRead:::readIniFile.
#' @param pathIni \code{Character}, Path to ini file.
#' @param overwrite logical, should file be overwritten if already exist?
#' @export
#' @rdname write-ini
writeIniFile <- function(listData, pathIni, overwrite = FALSE) {
  if (file.exists(pathIni)) {
    if (overwrite) {
      file.remove(pathIni)
    } else {
      stop("files already exist")
    }
  }
  con <- file(pathIni, "wb")
  on.exit(close(con))
  invisible(
    lapply(
      X = seq_along(listData),
      FUN = .formatedIniList,
      dtaToTransform = listData,
      namesdtaToTransform = names(listData),
      con = con
    )
  )
}

#' @export
#' @rdname write-ini
writeIniAPI <- function(listData, pathIni, opts) {
  assertthat::assert_that(inherits(opts, "simOptions"))
  actions <- lapply(
    X = seq_along(listData),
    FUN = function(i) {
      values <- .formatedIni(listData[[i]])
      values <- paste(values, collapse = ", ")
      list(
        target = paste(pathIni, names(listData)[i], sep = "/"),
        data = values
      )
    }
  )
  actions <- setNames(actions, rep("update_config", length(actions)))
  cmd <- do.call(api_commands_generate, actions)
  api_command_register(cmd, opts = opts)
  `if`(
    should_command_be_executed(opts), 
    api_command_execute(cmd, opts = opts, text_alert = sprintf("Updating %s: {msg_api}", pathIni)),
    cli_command_registered("update_config")
  )
}




#' Change R format to ini format
#' @param val value to format
#'
#' @return val formated value
#'
#' @noRd
.formatedIni <- function(val) {
  if (inherits(val, c("numeric", "integer"))) {
    format(val, nsmall = 6, scientific = FALSE)
  } else if (inherits(val, "logical")) {
    if (is.na(val)) {
      ""
    } else {
      tolower(as.character(val))
    }
  } else {
    val
  }
}

#' write ini 
#'
#' @param dtaToTransform \code{list} data to write
#' @param namesdtaToTransform \code{character} names of data to write
#' @param con file connection where data are write
#'
#' @noRd
.formatedIniList <- function(x, dtaToTransform, namesdtaToTransform, con) {
  if (length(dtaToTransform[[x]]) > 0) {
    if (!is.null(namesdtaToTransform)) {
      writeChar( paste0("[", namesdtaToTransform[x], "]\n"), con, eos = NULL)
    } else {
      writeChar(paste0("[", x-1, "]\n"), con, eos = NULL)
    }
    tmp_data <- dtaToTransform[[x]]
    # format values
    values <- lapply(X = tmp_data, FUN = .formatedIni)
    values <- lapply(X = values, FUN = paste, collapse = ", ")
    # write
    writeChar(paste(paste0(names(tmp_data), " = ", values), collapse = "\n"), con, eos = NULL)
    writeChar("\n\n", con, eos = NULL)
  } else {
    if (nzchar(namesdtaToTransform[x]))
      writeChar( paste0("[", namesdtaToTransform[x], "]\n"), con, eos = NULL)
    writeChar("\n\n", con, eos = NULL)
  }
}


is_study_path <- function(path, opts) {
  path <- normalizePath(path, mustWork = FALSE)
  studyPath <- normalizePath(opts$studyPath, mustWork = FALSE)
  startsWith(x = path, prefix = studyPath)
}
