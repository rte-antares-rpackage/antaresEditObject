
dropNulls <- function(x) {
  x[!vapply(x, is.null, FUN.VALUE = logical(1))]
}

`%||%` <- function(x, y)  {
  if (is.null(x)) { 
    y
  } else {
    x
  }
}

#TODO to copy/paste to antaresRead in a next release. 
.getLinkName<-function(areaX=NULL, areaY=NULL){
  if(areaX<areaY){
    c1<-areaX
    c2<-areaY
  }else{
    c1<-areaY
    c2<-areaX
  }
  return(tolower(paste0(c1, " - ", c2)))
}

 
#' @importFrom antaresRead getAreas
check_area_name <- function(area, opts = antaresRead::simOptions()) {
  areaList <- antaresRead::getAreas(opts = opts)
  if (!tolower(area) %in% areaList)
    stop("'", area, "' is not a valid area name, possible names are: ", paste(areaList, collapse = ", "), call. = FALSE)
}



hyphenize_names <- function(.list) {
  names(.list) <- gsub(pattern = "_", replacement = "-", x = names(.list))
  names(.list) <- tolower(gsub("([A-Z])", "-\\1", names(.list)))
  return(.list)
}




