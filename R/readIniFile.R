
# From antaresRead

#' Read a .ini file
#'
#' @param file file path
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
    
    key <- sapply(pairs, function(p) trimws(p[1]))
    value <- lapply(pairs, function(p) {
      v <- trimws(p[2])
      if (v == "true") return(TRUE)
      if (v == "false") return(FALSE)
      utils::type.convert(v, as.is = !stringsAsFactors)
    })
    
    L[[i]] <- value
    names(L[[i]]) <- key
  }
  L
}