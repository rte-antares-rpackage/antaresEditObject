
# From antaresFlowbased


#' Write ini file from list obtain by antaresRead:::readIniFile and modify by user
#'
#' @param listData \code{list}, modified list obtained by antaresRead:::readIniFile.
#' @param pathIni \code{Character}, Path to ini file.
#' @param overwrite logical, should file be overwritten if already exist?
#'
#' @examples
#'
#' \dontrun{
#' pathIni <- "D:/exemple_test/settings/generaldata.ini"
#' generalSetting <- antaresRead:::readIniFile(pathIni)
#' generalSetting$output$synthesis <- FALSE
#' writeIni(generalSetting, pathIni)
#' }
#'
#'
#' @export
#'
writeIni <- function(listData, pathIni, overwrite = FALSE) {
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
