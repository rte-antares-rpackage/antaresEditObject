
# From antaresRead

#' Read a INI file
#'
#' @param file file path.
#' @param stringsAsFactors logical: should character vectors be converted to factors?
#'
#' @return A list with an element for each section of the .ini file.
#' 
#' @importFrom utils type.convert
#'
#' @export
#'
readIniFile <- function(file, stringsAsFactors = FALSE) {
  X <- readLines(file)
  sections <- grep("^\\[.*\\]$", X)
  starts <- sections + 1
  ends <- c(sections[-1] - 1, length(X))
  L <- vector(mode="list", length=length(sections))
  names(L) <- gsub("\\[|\\]", "", X[sections])
  for(i in seq(along = sections)){
    if (starts[i] >= ends[i]) next
    pairs <- X[seq(starts[i], ends[i])]
    pairs <- pairs[pairs != ""]
    pairs <- strsplit(pairs, "=")
    
    key <- lapply(pairs, `[`, 1)
    key <- unlist(key)
    key <- trimws(key)
    
    value <- lapply(pairs, `[`, 2)
    value <- as.list(trimws(unlist(value)))
    value <- lapply(value, function(x) {
      if (x %in% c("true", "false")) {
        ifelse(x == "true", TRUE, FALSE)
      } else {
        utils::type.convert(x, as.is = TRUE)
      }
    })
    
    L[[i]] <- value
    names(L[[i]]) <- key
  }
  L
}
