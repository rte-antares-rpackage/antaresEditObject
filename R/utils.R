
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



#' @title Detect a pattern in a binding constraint coefficient
#'
#' @importFrom antaresRead readBindingConstraints
#'
#' @param pattern The pattern to detect.
#' @template opts
#'
#' @return the names of the binding constraints containing the pattern
detect_pattern_in_binding_constraint <- function(pattern, opts = antaresRead::simOptions()) {
  
  pattern <- as.character(pattern)
  assertthat::assert_that(inherits(opts, "simOptions"))
  assertthat::assert_that(all(nchar(pattern) - nchar(gsub("%", "", pattern)) <= 1))
  assertthat::assert_that(all(!startsWith(pattern, prefix = "%")))
  assertthat::assert_that(all(!endsWith(pattern, suffix = "%")))
  assertthat::assert_that(all(nchar(as.character(pattern)) - nchar(gsub("\\.", "", pattern)) <= 1))
  assertthat::assert_that(all(!startsWith(pattern, prefix = ".")))
  assertthat::assert_that(all(!endsWith(pattern, suffix = ".")))
  
  bc_not_remove <- character(0)
  bc <- readBindingConstraints(opts = opts)
  
  if (length(bc) > 0) {
    bc_coefs <- lapply(bc, "[[", "coefs")
    names_bc_coefs <- lapply(bc_coefs, names)
    pattern_in_names_bc_coefs <- lapply(names_bc_coefs, FUN = function(coef_name){sum(pattern %in% coef_name)})
    bc_not_remove <- pattern_in_names_bc_coefs[which(pattern_in_names_bc_coefs >= 1)]
    bc_not_remove <- names(bc_not_remove)
  }
  
  return(bc_not_remove)
}


generate_cluster_name <- function(area, cluster_name, add_prefix) {
  
  cluster_name <- tolower(cluster_name)
  
  if (add_prefix) {
    cluster_name <- paste(tolower(area), cluster_name, sep = "_")
  }
  
  return(cluster_name)
}


#' @importFrom antaresRead readClusterSTDesc
check_cluster_name <- function(area, cluster_name, add_prefix, opts = antaresRead::simOptions()) {
  
  exists <- FALSE
  
  clusters <- readClusterSTDesc(opts = opts)
  if (nrow(clusters) > 0) {
    cluster_name <- generate_cluster_name(area, cluster_name, add_prefix)
    clusters_filtered <- clusters[clusters$area == tolower(area) & clusters$cluster == cluster_name,]
    exists <- nrow(clusters_filtered) > 0
  }
    
  return(exists)
}

