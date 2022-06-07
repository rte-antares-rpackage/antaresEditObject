
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

is_different <- function(x, y) {
  if (is.null(x))
    return(FALSE)
  !identical(x, y)
}

#TODO to copy/paste to antaresRead in a next release. 
.getLinkName <- function(areaX=NULL, areaY=NULL){
  if(areaX<areaY) {
    c1 <- areaX
    c2 <- areaY
  }else{
    c1 <- areaY
    c2 <- areaX
  }
  return(tolower(paste0(c1, " - ", c2)))
}

 
#' @importFrom antaresRead getAreas
check_area_name <- function(area, opts = antaresRead::simOptions()) {
  areaList <- antaresRead::getAreas(opts = opts)
  if (!tolower(area) %in% areaList)
    stop("'", area, "' is not a valid area name, possible names are: ", paste(areaList, collapse = ", "), call. = FALSE)
}

validate_area_name <- function(name) {
  if (grepl(pattern = "(?!_)(?!-)[[:punct:]]", x = name, perl = TRUE)) {
    stop("Area's name must not contain ponctuation except - and _")
  }
}



hyphenize_names <- function(.list) {
  names(.list) <- gsub(pattern = "_", replacement = "-", x = names(.list))
  names(.list) <- tolower(gsub("([A-Z])", "-\\1", names(.list)))
  return(.list)
}


badge_api_ok <- function() {
  "\\ifelse{html}{\\figure{badge_api_ok.svg}{options: alt='Antares API OK'}}{Antares API: \\strong{OK}}"
}
badge_api_no <- function() {
  "\\ifelse{html}{\\figure{badge_api_no.svg}{options: alt='Antares API NO'}}{Antares API: \\strong{NO}}"
}




update_opts <- function(opts) {
  if (is_api_study(opts)) {
    update_api_opts(opts)
  } else {
    suppressWarnings({
      antaresRead::setSimulationPath(path = opts$studyPath, simulation = "input")
    })
  }
}



