#' @title Create a Demand Side Response (DSR)
#' 
#' @description 
#' `r antaresEditObject:::badge_api_ok()`
#' 
#' Create a Demand Side Response (DSR)
#'
#' @param areasAndDSRParam A `data.frame` with 4 columns `area`, `unit`,
#'  `nominalCapacity`, `marginalCost` and `hour`. 
#'  Hour represent the number of activation hours for the DSR per day.
#' @param spinning DSR spinning
#' @param overwrite Overwrite the DSR plant if already exist.
#'  This will overwrite the previous area and links. 
#' 
#' @template opts
#' 
#' @importFrom antaresRead simOptions setSimulationPath readLayout getLinks getAreas
#' @importFrom utils write.table
#' 
#' @examples
#' \dontrun{
#' 
#' library(antaresEditObject)
#' path <- pathToYourStudy
#' opts <- setSimulationPath(path, simulation = "input")
#' 
#' # area, unit, nominalCapacity and marginalCost
#' dsrData <- data.frame(area = c("a", "b"), unit = c(10,20), 
#'                     nominalCapacity = c(100, 120), marginalCost = c(52, 65), hour = c(3, 7))
#' 
#' createDSR(dsrData)
#' 
#' createDSR(dsrData, spinning = 3, overwrite = TRUE)
#' getAreas()
#' 
#' }
#' @export
#' 
createDSR <- function(areasAndDSRParam = NULL, 
                      spinning = 2,
                      overwrite = FALSE,
                      opts = antaresRead::simOptions()) {
  
  assertthat::assert_that(inherits(opts, "simOptions"))
  
  oldOps <- opts
  areasAndDSRParam <- .checkDataForAddDSR(areasAndDSRParam, spinning, overwrite, oldOps)
  
  newOpts <- .addDSRArea(
    areasAndDSRParam, 
    overwrite = overwrite, 
    opts = oldOps
  )
  newOpts <- .addLinksBetweenDSRAndAreas(
    areasAndDSRParam = areasAndDSRParam,
    overwrite = overwrite, 
    opts = newOpts
  )
  newOpts <- .addBindingConstraintToDSR(
    areasAndDSRParam = areasAndDSRParam,
    overwrite = overwrite,
    opts = newOpts
  )
  newOpts <- .AddClusterToDST(
    areasAndDSRParam = areasAndDSRParam, 
    spinning = spinning, 
    overwrite = overwrite,
    opts = newOpts
  )
  
  # Maj simulation
  res <- if (is_api_study(opts)) {
    update_api_opts(opts)
  } else {
    suppressWarnings({
      antaresRead::setSimulationPath(path = opts$studyPath, simulation = "input")
    })
  }
  
  invisible(res)
}

.checkDataForAddDSR <- function(areasAndDSRParam = NULL, spinning = NULL, overwrite = NULL, opts = NULL) {

  #check if we have the necessary data : class
  if (!is.data.frame(areasAndDSRParam)) {
    stop("areasAndDSRParam must be a data.frame", call. = FALSE)
  }
  
  #check if we have the necessary data : areasAndDSRParam
  if (is.null(areasAndDSRParam$area) | 
      is.null(areasAndDSRParam$unit) | 
      is.null(areasAndDSRParam$nominalCapacity) |
      is.null(areasAndDSRParam$marginalCost) |
      is.null(areasAndDSRParam$hour)) {
    stop("areasAndDSRParam must be a data.frame with a column area, unit, nominalCapacity, marginalCost and hour", call. = FALSE)
  }
  
  for ( i in c("marginalCost", "hour", "unit")) {
    if (!is.numeric(areasAndDSRParam[i][1, ])) {
      stop(paste0(i, " is not numeric."), call. = FALSE)
    }
  }
  
  #check if we have the necessary data : areasAndDSRParam$area
  sapply(areasAndDSRParam$area, function(x) {
    if (!(x %in% antaresRead::getAreas())){
      stop(paste0(x, " is not a valid area."), call. = FALSE)
    }
  })
  
  #check if we have the necessary data : areas
  if (length(antaresRead::getAreas()) == 0 | identical(antaresRead::getAreas(), "")) {
    stop("There is no area in your study.", call. = FALSE)
  }
  
  #check if we have the necessary data : spinning
  if (is.null(spinning)) {
    stop("spinning is set to NULL", call. = FALSE)
  }
  if (!is.double(spinning)) {
    stop("spinning is not a double.", call. = FALSE)
  }
  
  return(areasAndDSRParam)
}

.getNameDsr <- function(area = NULL, hour = NULL) {
  paste0(area, "_dsr_", hour, "h")
}

.addDSRArea <- function(areasAndDSRParam = NULL, overwrite = NULL, opts = NULL) {
  #data.table pb 
  area <- NULL
  y <- NULL
  
  invisible(apply(areasAndDSRParam, 1, function(x) {
  
    areaName <- x["area"]
    numberHour <- x["hour"]
    nameDsr <- .getNameDsr(areaName, numberHour)
    #check if the area exist
    if (!(casefold(nameDsr, upper = FALSE)  %in% antaresRead::getAreas(opts = opts)) | overwrite) {
      
      #overwrite if the virtual area is in getAreas 
      if (overwrite & (casefold(nameDsr, upper = FALSE) %in% antaresRead::getAreas(opts = opts))) {
        opts <- removeArea(name = nameDsr, opts = opts)
      }
      
      xyLayout <- antaresRead::readLayout(opts = opts)
      
      LocX <- xyLayout$areas[area == areaName, x] - 20 
      LocY <- xyLayout$areas[area == areaName, y] - 20
      
      createArea(
        nameDsr,
        color = grDevices::rgb(150, 150, 150, max = 255),
        localization = c(LocX, LocY),
        overwrite = overwrite,
        opts = opts
      )
      
    } else {
      warning(paste0(
        nameDsr,
        " already exists, use argument overwrite if you want to edit this area. ",
        "All previous links will be lost."
      ), call. = FALSE)
    }
    
  }))
  
  # Maj simulation
  res <- if (is_api_study(opts)) {
    update_api_opts(opts)
  } else {
    suppressWarnings({
      antaresRead::setSimulationPath(path = opts$studyPath, simulation = "input")
    })
  }
  
  invisible(res)
  
}

.addLinksBetweenDSRAndAreas <- function(areasAndDSRParam = NULL, overwrite = NULL, opts = NULL) {
  
  invisible(apply(areasAndDSRParam, 1, function(x) {
    
    areaName <- x["area"]
    numberHour <- x["hour"]
    installedCapacityLink <- as.double(x["unit"]) * as.double(x["nominalCapacity"])
    nameDsr <- .getNameDsr(areaName, numberHour)
    
    conditionToCreateALink <- paste0(areaName, " - ", nameDsr) %in% antaresRead::getLinks(opts = opts) |
      paste0(nameDsr, " - ", areaName) %in% antaresRead::getLinks(opts = opts)
    if (!conditionToCreateALink | overwrite) {
      
      if (is_antares_v820(opts)) {
        dataLinkVirtual <- matrix(data = rep(0, 8760*6), ncol = 6)
        tsLinkVirtual <- matrix(
          data = c(rep(0, 8760), rep(installedCapacityLink, 8760)),
          ncol = 2
        )
      } else if (is_antares_v7(opts)) {
        dataLinkVirtual <- matrix(
          data = c(rep(0, 8760), rep(installedCapacityLink, 8760), rep(0, 8760*6)),
          ncol = 8
        )
        tsLinkVirtual <- NULL
      } else {
        dataLinkVirtual <- matrix(
          data = c(rep(0, 8760), rep(installedCapacityLink, 8760), rep(0, 8760*3)), 
          ncol = 5
        )
        tsLinkVirtual <- NULL
      }
      
      dataLinkProperties <- propertiesLinkOptions()
      dataLinkProperties$`hurdles-cost` <- FALSE
      createLink(
        from = areaName,
        to = nameDsr, 
        dataLink = dataLinkVirtual, 
        tsLink = tsLinkVirtual, 
        propertiesLink = dataLinkProperties,
        overwrite = overwrite,
        opts = opts
      )
      
    } else {
      stop(
        paste0("The link ", areaName, " - ", nameDsr, " already exist, use overwrite."),
        call. = FALSE
      )
    }
  }))
  
  # Maj simulation
  res <- if (is_api_study(opts)) {
    update_api_opts(opts)
  } else {
    suppressWarnings({
      antaresRead::setSimulationPath(path = opts$studyPath, simulation = "input")
    })
  }
  
  invisible(res)
}

.addBindingConstraintToDSR <- function(areasAndDSRParam = NULL, overwrite = NULL, opts = NULL) {
  
  invisible(apply(areasAndDSRParam, 1, function(x){
    
    
    areaName <- x["area"]
    numberHour <- x["hour"]
    installedCapacityLink <- as.double(x["unit"]) * as.double(x["nominalCapacity"])
    nameDsr <- .getNameDsr(areaName, numberHour)
    #name binding
    nameBindDSR <- nameDsr
    
    #coef binding
    coefficientsDSR <- .getCoefDsr(areaName, nameDsr)
    # A binding constraint at a daily timestep expects 366 rows
    createBindingConstraint(
      nameBindDSR, 
      values = matrix(data = c(rep(installedCapacityLink * as.double(numberHour), 366), rep(0, 366 * 2)), ncol = 3),
      enabled = TRUE, 
      timeStep = "daily",
      operator = c("less"),
      coefficients = coefficientsDSR,
      overwrite = overwrite,
      opts = opts
    )
    
  }))
  
  # Maj simulation
  res <- if (is_api_study(opts)) {
    update_api_opts(opts)
  } else {
    suppressWarnings({
      antaresRead::setSimulationPath(path = opts$studyPath, simulation = "input")
    })
  }
  
  invisible(res)
  
}

.getCoefDsr <- function(areaName = NULL, dsrName = NULL) {
  if (areaName < dsrName){
    nameCoefDSR <- tolower(paste0(areaName, "%", dsrName))
    coeffDsr <- (-1)
  }else {
    nameCoefDSR <- tolower(paste0(dsrName, "%", areaName))
    coeffDsr <- (1)
  }
  
  coefficientsDSR <- c( coeffDsr )
  names(coefficientsDSR)[1] <- nameCoefDSR
  
  return(coefficientsDSR)
}

.AddClusterToDST <- function(areasAndDSRParam = NULL, spinning = NULL, overwrite = NULL, opts = NULL) {
  
  invisible(apply(areasAndDSRParam, 1, function(x){
    areaName <- x["area"]
    numberHour <- x["hour"]
    unitDSR <- x["unit"]
    marginalCost <- x["marginalCost"]
    nominalCapacity <- x["nominalCapacity"]
    
    nameDsr <- .getNameDsr(areaName, numberHour)
    
    createCluster(
      nameDsr, 
      cluster_name = paste0(nameDsr, "_cluster"),
      group = "Other",
      unitcount = as.integer(unitDSR),
      `marginal-cost` = marginalCost,
      enabled = TRUE,
      spinning = spinning,
      nominalcapacity = nominalCapacity,
      overwrite = overwrite,
      add_prefix = FALSE
    )
    
  }))
  
  # Maj simulation
  res <- if (is_api_study(opts)) {
    update_api_opts(opts)
  } else {
    suppressWarnings({
      antaresRead::setSimulationPath(path = opts$studyPath, simulation = "input")
    })
  }
  
  invisible(res)
  
}


#' @rdname createDSR
#' 
#' @return \code{getCapacityDSR()} returns DSR capacity (unit * nominalCapacity of virtual cluster) of the area
#' 
#' @examples
#' \dontrun{
#' 
#' getCapacityDSR("a")
#' editDSR("a", unit = 50, nominalCapacity = 8000)
#' getCapacityDSR("a")
#' 
#' }
#' @export
#' 
getCapacityDSR <- function(area = NULL,  opts = antaresRead::simOptions()) {
  
  check_area_name(area, opts = opts)
  nameDsr <- .getTheDsrName(area)
  
  clusterList <- antaresRead::readClusterDesc(opts = opts)
  unit <- as.double(clusterList[area == nameDsr]$unitcount)
  nominalcapacity <- as.double(clusterList[area == nameDsr]$nominalcapacity)
  
  return(unit * nominalcapacity)
}

.getTheDsrName <- function(area = NULL) {
  if (TRUE %in% grepl(paste0(area, "_dsr"), antaresRead::getAreas())) {
    nameDsr <- grep(paste0(area, "_dsr"), antaresRead::getAreas(), value = TRUE )
  }else {
    stop("There is no DSR for this area")
  }
  
  return(nameDsr)
}

#' @rdname createDSR
#' @param area an area where to edit the DSR  
#' @param unit DSR unit number 
#' @param nominalCapacity DSR nominalCapacity
#' @param marginalCost DSR marginalCost
#'  
#' @examples
#' \dontrun{
#' 
#' getCapacityDSR("a")
#' editDSR("a", unit = 50, nominalCapacity = 8000, marginalCost = 45, hour = 9)
#' getCapacityDSR("a")
#' 
#' }
#' @export
#' 
editDSR <- function(area = NULL, 
                    unit = NULL, 
                    nominalCapacity = NULL, 
                    marginalCost = NULL, 
                    spinning = NULL, 
                    opts = antaresRead::simOptions()) {
  
  check_area_name(area, opts = opts)
  .checkDataEditDSR(area, unit, nominalCapacity, marginalCost, spinning)

  #get previous Data 
  bindingList <- antaresRead::readBindingConstraints(opts = opts)
  clusterList <- antaresRead::readClusterDesc(opts = opts)
  previousNameDsr <- .getTheDsrName(area)
  previousUnitCount <- as.double(clusterList[area == previousNameDsr]$unitcount)
  previousNominalCapacity <- as.double(clusterList[area == previousNameDsr]$nominalcapacity)
  capaBinding <- unique(bindingList[previousNameDsr][[1]]$values$less[1])
  previousNumberHour <- round(as.double(capaBinding / (previousNominalCapacity * previousUnitCount)))
  
  if (is.null(unit) & is.null(nominalCapacity)) {
    newCapacityLink <- previousUnitCount * previousNominalCapacity
  } else{
    newCapacityLink <- unit * nominalCapacity
  }
  
  #edit cluster 
  createCluster(
    previousNameDsr, 
    cluster_name = paste0(previousNameDsr, "_cluster"),
    group = "Other",
    unitcount = as.integer(unit),
    `marginal-cost` = marginalCost,
    enabled = TRUE,
    spinning = spinning,
    nominalcapacity = nominalCapacity,
    overwrite = TRUE,
    add_prefix = FALSE,
    opts = opts
  )
  
  #edit binding constraint
  #coef binding
  coefficientsDSR <- .getCoefDsr(area, previousNameDsr)
  
  # A binding constraint at a daily timestep expects 366 rows
  createBindingConstraint(
    previousNameDsr,
    values = matrix(data = c(rep(newCapacityLink * as.double(previousNumberHour), 366), rep(0, 366 * 2)), ncol = 3),
    enabled = TRUE, 
    timeStep = "daily",
    operator = c("less"), 
    coefficients = coefficientsDSR,
    overwrite = TRUE, 
    opts = opts
  )
  
  #edit Link
  if (is_antares_v820(opts)) {
    dataLinkVirtual <- matrix(data = rep(0, 8760*6), ncol = 6)
    tsLinkVirtual <- matrix(data = c(rep(0, 8760), rep(newCapacityLink, 8760)), ncol = 2)
  } else if (is_antares_v7(opts)) {
    dataLinkVirtual <- matrix(data = c(rep(0, 8760), rep(newCapacityLink, 8760), rep(0, 8760*6)), ncol = 8)
    tsLinkVirtual <- NULL
  } else {
    dataLinkVirtual <- matrix(data = c(rep(0, 8760), rep(newCapacityLink, 8760), rep(0, 8760*3)), ncol = 5)
    tsLinkVirtual <- NULL
  }
  dataLinkProperties <- propertiesLinkOptions()
  dataLinkProperties$`hurdles-cost` <- FALSE
  createLink(
    from = area, 
    to = previousNameDsr, 
    dataLink = dataLinkVirtual, 
    tsLink = tsLinkVirtual,
    propertiesLink = dataLinkProperties, 
    opts = opts,
    overwrite = TRUE
  )
  
  # Maj simulation
  res <- if (is_api_study(opts)) {
    update_api_opts(opts)
  } else {
    suppressWarnings({
      antaresRead::setSimulationPath(path = opts$studyPath, simulation = "input")
    })
  }
  
  invisible(res)
  
}

.checkDataEditDSR <- function(area = NULL,
                              unit = NULL, 
                              nominalCapacity = NULL, 
                              marginalCost = NULL,
                              spinning = NULL) {
  
  for ( i in c(unit, nominalCapacity, marginalCost, spinning)) {
    if (!is.numeric(i)) {
      stop(paste0(i, " is not numeric."), call. = FALSE)
    }
  }
  
  if ( (is.null(unit) & !is.null(nominalCapacity)) | (!is.null(unit) & is.null(nominalCapacity))) {
    stop(paste0("unit or nominalCapacity is set to NULL"), call. = FALSE)
  } 
  
}
