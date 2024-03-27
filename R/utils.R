
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


is_named <- function(x) {
  nms <- names(x)
  if (is.null(nms))
    return(FALSE)
  if (any(!nzchar(nms)))
    return(FALSE)
  TRUE
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
  if (is_api_study(opts) && is_api_mocked(opts))
    return(invisible(NULL))
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

# reorder a list into a simple named list 
# target a named list within the list, then move items up one step in the main list
rename_floor_list <- function(target_name, list_to_reforge){
  assertthat::assert_that(inherits(target_name, "character"))
  assertthat::assert_that(inherits(list_to_reforge, "list"))
  
  if(target_name %in% names(list_to_reforge)){
    if(class(list_to_reforge[[target_name]]) %in% "list"){
      target_elements <- list_to_reforge[[target_name]]
      names(target_elements) <- paste(target_name, names(target_elements), sep = "_")
      
      # overwrite list
      list_to_reforge[[target_name]] <- NULL
      list_to_reforge <- append(list_to_reforge, target_elements)
      
      return(list_to_reforge)
    }else{
      list_to_reforge[[target_name]] <- NULL
      return(list_to_reforge)
    }
      
  }else
    return(list_to_reforge)
}

#' @importFrom antaresRead readClusterSTDesc
check_cluster_name <- function(area, cluster_name, add_prefix, opts = antaresRead::simOptions()) {
  
  exists <- FALSE
  
  area <- tolower(area)
  cluster_name <- tolower(cluster_name)
  if (add_prefix) {
    cluster_name <- paste(area, cluster_name, sep = "_")
  }
  
  clusters <- readClusterSTDesc(opts = opts)
  if (nrow(clusters) > 0) {
    clusters_filtered <- clusters[clusters$area == area & clusters$cluster == cluster_name,]
    exists <- nrow(clusters_filtered) > 0
  }
    
  return(exists)
}
