#' @title Create a Pumped Storage Power plant (PSP)
#' 
#' @description 
#' `r antaresEditObject:::badge_api_ok()`
#' 
#' Create a Pumped Storage Power plant (PSP)
#'
#' @param areasAndCapacities A data.frame with 2 columns `installedCapacity` and `area`.
#' @param namePumping The name of the pumping area
#' @param nameTurbining The name of the turbining area
#' @param hurdleCost The cost of the PSP
#' @param timeStepBindConstraint Time step for the binding constraint : `daily` or `weekly`
#' @param efficiency The efficiency of the PSP
#' @param overwrite Overwrite the Pumped Storage Power plant if already exist.
#' This will overwrite the previous area and links. 
#' @param area an area name 
#' @param capacity PSP capacity for the area
#' 
#' @template opts
#' 
#' @export
#' 
#' @importFrom antaresRead simOptions setSimulationPath readLayout getLinks getAreas
#' @importFrom utils write.table
#' 
#' @examples
#' \dontrun{
#' 
#' library(antaresEditObject)
#' path<-pathToYourStudy
#' opts<-setSimulationPath(path, simulation = "input")
#' pspData<-data.frame(area=c("a", "b"), installedCapacity=c(800,900))
#' 
#' createPSP(pspData, efficiency = 0.8)
#' 
#' createPSP(pspData, efficiency = 0.66, overwrite = TRUE)
#' createPSP(pspData, efficiency = 0.98, timeStepBindConstraint = "daily")
#' getAreas()
#' 
#' }
#' 

createPSP <- function(areasAndCapacities = NULL, 
                      namePumping = "Psp_In", 
                      nameTurbining = "Psp_Out", 
                      hurdleCost = 0.0005, 
                      timeStepBindConstraint = "weekly", 
                      efficiency = NULL, 
                      overwrite = FALSE, 
                      opts = antaresRead::simOptions()) {
  
  assertthat::assert_that(inherits(opts, "simOptions"))

  oldOps <- opts
  areasAndCapacities <- .checkDataForAddPSP(
    areasAndCapacities,
    namePumping, 
    nameTurbining, 
    overwrite = overwrite,
    oldOps, 
    hurdleCost, 
    timeStepBindConstraint, 
    efficiency
  )
  namePumping <- .getTheLastVirualName(
    namePumping,
    timeStepBindConstraint = timeStepBindConstraint
  )
  nameTurbining <- .getTheLastVirualName(
    nameTurbining, 
    timeStepBindConstraint = timeStepBindConstraint
  )
  
  newOpts <- .addOneOfTheVirtualArea(
    nameTurbining,
    pump = FALSE,
    overwrite, 
    timeStepBindConstraint = timeStepBindConstraint, 
    opts = oldOps
  )
  newOpts <- .addOneOfTheVirtualArea(
    namePumping,
    pump = TRUE, 
    overwrite, 
    timeStepBindConstraint = timeStepBindConstraint,
    opts = newOpts
  )
  newOpts <- .addLinksBetweenPspAndAreas(
    areasAndCapacities = areasAndCapacities, 
    nameVirtualArea = nameTurbining, 
    pumpP = FALSE,
    overwrite = overwrite, 
    hurdleCost = hurdleCost, 
    opts = newOpts
  )
  newOpts <- .addLinksBetweenPspAndAreas(
    areasAndCapacities = areasAndCapacities, 
    nameVirtualArea = namePumping, 
    pumpP = TRUE, 
    overwrite = overwrite, 
    hurdleCost = hurdleCost,
    opts = newOpts
  )
  
  .addRowBalance(
    namePumping = namePumping,
    nameTurbining = nameTurbining,
    opts = newOpts
  )
  
  newOpts <- .addBindingConstraintToPSP(
    areasAndCapacities = areasAndCapacities, 
    namePumping = namePumping, 
    nameTurbining = nameTurbining, 
    opts = newOpts, 
    overwrite = overwrite, 
    timeStepBindConstraint = timeStepBindConstraint,
    efficiency = efficiency
  )
  
  # Maj simulation
  res <- update_opts(opts)
  
  invisible(res)
}


.checkDataForAddPSP <- function(areasAndCapacities = NULL, 
                                namePumping = NULL, 
                                nameTurbining = NULL, 
                                overwrite = NULL, 
                                opts = NULL, 
                                hurdleCost = NULL, 
                                timeStepBindConstraint = NULL, 
                                efficiency = NULL){
  #error with globalVariable with a data.table R CMD Check 
  country <- NULL

  
  #check if we have the necessary data : class
  if (!is.data.frame(areasAndCapacities)) {
    stop("areasAndCapacities must be a data.frame")
  }
  
  #check if we have the necessary data : data
  if (is.null(areasAndCapacities$country) & is.null(areasAndCapacities$area)) {
    stop("areasAndCapacities must be a data.frame with a column area")
  }
  
  #check if we have the necessary data : data
  if (is.null(areasAndCapacities$installedCapacity)) {
    stop("areasAndCapacities must be a data.frame with a column installedCapacity")
  }
  
  if (!is.null(areasAndCapacities$area)) {
    areasAndCapacities$country <- areasAndCapacities$area
  }
  
  sapply(areasAndCapacities$area, function(x) {
    if (!(x %in% antaresRead::getAreas())) {
      stop(paste0(x, " is not a valid area."), call. = FALSE)
    }
  })
  
  #check if we have the necessary data : areas
  if (length(antaresRead::getAreas()) == 0 | identical(antaresRead::getAreas(), "")) {
    stop("There is no area in your study.", call. = FALSE)
  }
  
  #check if we have the necessary data : correct name
  if (is.null(namePumping) | is.null(nameTurbining)) {
    stop("One of the pumping or turbining name is set to NULL")
  }
  
  #check if we have the necessary data : correct name
  if (!is.character(namePumping) | !is.character(nameTurbining)) {
    stop("One of the pumping or turbining name is not a character.")
  }
  
  #check if we have the necessary data : correct efficiency
  if (is.null(efficiency)) {
    stop("efficiency is set to NULL")
  }
  if (!is.double(efficiency)) {
    stop("efficiency is not a double.")
  }

  #check if we have the necessary data
  if (!(timeStepBindConstraint %in% c("weekly", "daily"))) {
    stop("timeStepBindConstraint is not equal to weekly or daily.")
  }
  
  #check hurdleCost
  if (!is.double(hurdleCost)) {
    stop("hurdleCost is not a double.")
  }
  
  areasAndCapacities <- data.table::data.table(areasAndCapacities)
  areasAndCapacities[, country := as.character(country)]
  return(areasAndCapacities)
}

.getTheLastVirualName <- function(nameVirtualArea = NULL, timeStepBindConstraint = NULL) {
  #create the virtual area name
  if (timeStepBindConstraint == "weekly") {
    endName <- paste0(tolower(nameVirtualArea), "_", "w")
  } else {
    endName <- paste0(tolower(nameVirtualArea), "_", "d")
  }
  return(endName)
}

.addBindingConstraintToPSP <- function(areasAndCapacities = NULL,
                                       namePumping = NULL, 
                                       nameTurbining = NULL,
                                       opts = NULL,
                                       overwrite = NULL, 
                                       timeStepBindConstraint = NULL,
                                       efficiency = NULL) {
  

  invisible(sapply(areasAndCapacities$country, function(x) {
    
    #name binding
    nameBindPSP <- tolower(paste0(x, "_psp_", timeStepBindConstraint))
    
    #coef binding
    if (x < namePumping & x < nameTurbining) {
      nameCoefPump <- tolower(paste0(x, "%", namePumping))
      nameCoefTurb <- tolower(paste0(x, "%", nameTurbining))
    } else if (x > namePumping & x > nameTurbining) {
      nameCoefPump <- tolower(paste0(namePumping, "%", x))
      nameCoefTurb <- tolower(paste0(nameTurbining, "%", x))
    } else {
      stop("Change the name of your virtual areas.")
    }
    
    coefficientsPSP <- c( efficiency,  1 )
    names(coefficientsPSP)[1] <- nameCoefPump
    names(coefficientsPSP)[2] <- nameCoefTurb
    
    createBindingConstraint(
      nameBindPSP, 
      values = matrix(data = rep(0, 365 * 3), ncol = 3),
      enabled = TRUE, 
      timeStep = timeStepBindConstraint,
      operator = c("equal"),
      coefficients = coefficientsPSP,
      overwrite = overwrite
    )
    
    #createLink(from = x, to=nameVirtualArea, dataLink = dataLinkVirtual, propertiesLink = dataLinkProperties)
  }))
  
  # Maj simulation
  res <- update_opts(opts)
  
  invisible(res)
  
}

.addRowBalance <- function(namePumping = NULL, nameTurbining = NULL, opts = NULL) {
  
  miscDataPump <- matrix(data = c(rep(0, 8760 * 7), rep(-100000, 8760)), ncol = 8)
  writeMiscGen(data = miscDataPump, area = namePumping, opts = opts)
  
  miscDataTurb <- matrix(data = c(rep(0, 8760 * 7), rep(100000, 8760)), ncol = 8)
  writeMiscGen(data = miscDataTurb, area = nameTurbining, opts = opts)
  
}

.addLinksBetweenPspAndAreas <- function(areasAndCapacities = NULL,
                                        nameVirtualArea = NULL,
                                        pumpP = NULL, 
                                        hurdleCost = NULL, 
                                        overwrite = NULL,
                                        opts = NULL) {
  #error with globalVariable with a data.table R CMD Check 
  installedCapacity <- NULL
  country <- NULL
  
  v7 <- is_antares_v7(opts)
  
  invisible(sapply(areasAndCapacities$country, function(x) {
    
    #get the step capa
    stepCapaX <- areasAndCapacities[country == x, installedCapacity]
    
    conditionToCreateALink <- .getLinkName(x, nameVirtualArea) %in% antaresRead::getLinks()
    
    if (!conditionToCreateALink | overwrite) {
      if (isTRUE(pumpP)){
        dataLinkVirtual <- matrix(
          data = c(
            rep(stepCapaX, 8760),
            rep(0, 8760),
            rep(0, 8760),
            rep(hurdleCost, 8760 * 2)
          ), 
          ncol = 5
        )
        dataLinkProperties <- propertiesLinkOptions()
      } else {
        dataLinkVirtual <- matrix(
          data = c(
            rep(0, 8760),
            rep(stepCapaX, 8760), 
            rep(0, 8760), 
            rep(hurdleCost, 8760 * 2)
          ), 
          ncol = 5
        )
        dataLinkProperties <- propertiesLinkOptions()
        dataLinkProperties$`hurdles-cost` <- TRUE
      }
      
      if (isTRUE(v7)) {
        dataLinkVirtual <- cbind(
          dataLinkVirtual[, c(1, 2, 4, 5, 3)],
          matrix(data = 0, ncol = 3, nrow = 8760)
        )
      }
      
      createLink(
        from = x, 
        to = nameVirtualArea,
        dataLink = dataLinkVirtual, 
        propertiesLink = dataLinkProperties, 
        overwrite = overwrite,
        opts = opts
      )
    } else {
      #print(x)
      #print(nameVirtualArea)
    }
  }))
  
  # Maj simulation
  res <- update_opts(opts)
  
  invisible(res)
}

.addOneOfTheVirtualArea <- function(nameVirtualArea = NULL, 
                                    pump = NULL, 
                                    overwrite = NULL, 
                                    timeStepBindConstraint = NULL, 
                                    opts = NULL){
  #error with globalVariable with a data.table R CMD Check 
  area <- NULL
  
  if (!(casefold(nameVirtualArea, upper = FALSE)  %in% antaresRead::getAreas()) | overwrite) {
    
    #overwrite if the virtual is in getAreas 
    if (overwrite & (casefold(nameVirtualArea, upper = FALSE) %in% antaresRead::getAreas(opts = opts))) {
      opts <- removeArea(name = nameVirtualArea)
    }
    
    xyLayout <- antaresRead::readLayout()
    
    #remove virtual x,y from xyLayout
    if (tolower(nameVirtualArea) %in% antaresRead::getAreas()){
      xyLayout$areas <- xyLayout$areas[area != tolower(nameVirtualArea)]
    }
    
    if (!is.null(xyLayout)) {
      LocX <- round(mean(xyLayout$areas$x))
      
      if (pump){
        LocY <- min(xyLayout$areas$y) - 100
      } else {
        LocY <- max(xyLayout$areas$y) + 100
      }
      
    } else {
      LocX <- 0
      LocY <- 0
    }
    
    createArea(
      name = nameVirtualArea,
      color = grDevices::rgb(40, 220, 240, max = 255),
      localization = c(LocX, LocY),
      overwrite = overwrite,
      opts = opts
    )
    
  } else {
    warning(
      paste0(nameVirtualArea,
             " already exists, use argument overwrite if you want to edit this area.
                   All previous links will be lost."), 
      call. = FALSE
    )
  }
  
  # Maj simulation
  res <- update_opts(opts)
  
  invisible(res)
}


#' @rdname createPSP
#' 
#' @return \code{getCapacityPSP()} returns PSP capacity of the area
#' 
#' @examples
#' \dontrun{
#' 
#' getCapacityPSP("a")
#' editPSP("a", capacity = 8000, hurdleCost = 0.1)
#' getCapacityPSP("a")
#' 
#' areaName<-"suisse"
#' createArea(areaName, overwrite = TRUE)
#' pspData<-data.frame(area=c(areaName), installedCapacity=c(9856))
#' createPSP(pspData, efficiency = 0.5, overwrite = TRUE, timeStepBindConstraint = "daily")
#' 
#' getCapacityPSP(areaName, timeStepBindConstraint = "daily")
#' 
#' 
#' }
#' @export
#' 
getCapacityPSP <- function(area = NULL, 
                           nameTurbining = "Psp_Out", 
                           timeStepBindConstraint = "weekly", 
                           opts = antaresRead::simOptions()) {
  
  check_area_name(area, opts = opts)
  endNameTurbining <- .getTheLastVirualName(
    nameTurbining, 
    timeStepBindConstraint = timeStepBindConstraint
  )
  capaLinkTurb <- antaresRead::readInputTS(
    linkCapacity = .getLinkName(area, endNameTurbining), 
    showProgress = FALSE, 
    opts = opts
  )
  if (area > endNameTurbining) {
    columnTake<- "transCapacityDirect"
  } else {
    columnTake <- "transCapacityIndirect"
  }
  valueCapa <- capaLinkTurb[, get(columnTake)][1]
  
  return(valueCapa)
}


#' @rdname createPSP
#' 
#' @export
#' 
editPSP <- function(area = NULL,
                    capacity = NULL,
                    namePumping = "Psp_In",
                    nameTurbining = "Psp_Out", 
                    timeStepBindConstraint = "weekly",
                    hurdleCost = 0.0005, 
                    opts = antaresRead::simOptions()) {
  v7 <- is_antares_v7(opts)
  check_area_name(area, opts = opts)
  #PUMP
  endNamePumping <- .getTheLastVirualName(
    namePumping, 
    timeStepBindConstraint = timeStepBindConstraint
  )
  dataLinkVirtualPump <- matrix(
    data = c(
      rep(capacity, 8760), 
      rep(0, 8760),
      rep(0, 8760),
      rep(hurdleCost, 8760 * 2)
    ),
    ncol = 5
  )
  if (isTRUE(v7)) {
    dataLinkVirtualPump <- cbind(
      dataLinkVirtualPump[, c(1, 2, 4, 5, 3)],
      matrix(data = 0, ncol = 3, nrow = 8760)
    )
  }
  dataLinkPropertiesPump <- propertiesLinkOptions()
  editLink(
    from = area,
    to = endNamePumping,
    dataLink = dataLinkVirtualPump, 
    hurdles_cost = dataLinkPropertiesPump$`hurdles-cost`, 
    transmission_capacities = dataLinkPropertiesPump$`transmission-capacities`, 
    display_comments = dataLinkPropertiesPump$`display-comments`, 
    filter_synthesis = dataLinkPropertiesPump$`filter-synthesis`, 
    filter_year_by_year = dataLinkPropertiesPump$`filter-year-by-year`, 
    opts = opts
  )
  
  #Turb
  endNameTurbining<- .getTheLastVirualName(
    nameTurbining,
    timeStepBindConstraint = timeStepBindConstraint
  )
  dataLinkVirtualTurb <- matrix(
    data = c(
      rep(0, 8760), 
      rep(capacity, 8760), 
      rep(0, 8760),
      rep(hurdleCost, 8760 * 2)
    ),
    ncol = 5
  )
  if (isTRUE(v7)) {
    dataLinkVirtualTurb <- cbind(
      dataLinkVirtualTurb[, c(1, 2, 4, 5, 3)],
      matrix(data = 0, ncol = 3, nrow = 8760)
    )
  }
  dataLinkPropertiesTurb <- propertiesLinkOptions()
  dataLinkPropertiesTurb$`hurdles-cost` <- TRUE
  editLink(
    from = area,
    to = endNameTurbining, 
    dataLink = dataLinkVirtualTurb, 
    hurdles_cost = dataLinkPropertiesTurb$`hurdles-cost`, 
    transmission_capacities = dataLinkPropertiesTurb$`transmission-capacities`, 
    display_comments = dataLinkPropertiesTurb$`display-comments`, 
    filter_synthesis = dataLinkPropertiesTurb$`filter-synthesis`, 
    filter_year_by_year = dataLinkPropertiesTurb$`filter-year-by-year`, 
    opts = opts
  )
  
  # Maj simulation
  res <- update_opts(opts)
  
  invisible(res)
}

