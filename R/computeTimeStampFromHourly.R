#' @title Compute daily, weekly, monthly and annual mc-ind from hourly data.
#' 
#' @description 
#' `r antaresEditObject::badge_api_no()`
#' 
#' Compute daily, weekly, monthly and annual mc-ind from hourly data.
#'
#' @param opts opts simulation path.
#' @param mcYears mcYears to compute.
#' @param nbcl number of thread for parallel computing.
#' @param verbose verbose for execution.
#' @param type type of file to compute.
#'
#' @examples
#' \dontrun{
#'
#' library(antaresEditObject)
#' opts <- setSimulationPath("my_study")
#' computeTimeStampFromHourly(opts)
#'
#' }
#'
#' @import doParallel pbapply parallel
#' @importFrom stats sd as.formula
#'
#' @export
computeTimeStampFromHourly <- function(opts,
                                       mcYears = "all",
                                       nbcl = 8,
                                       verbose = 1,
                                       type = c("areas", "links", "clusters")) {
  assertthat::assert_that(inherits(opts, "simOptions"))
  api_not_implemented(opts)
  if (verbose == 0) {
    pboptions(type = "none")
  }
  if (verbose == 1) {
    pboptions(type = "txt")
  }
  
  if (verbose == 1) {
    cat("Start compute all mc ind from hourly data\n")
  }
  if (mcYears[1] == "all") {
    mcYears <- opts$mcYears
  }
  if (verbose == 1) {
    cat("Load necessary data\n")
  }
  dayArea <- unique(
    antaresRead::readAntares(
      areas = "all",
      mcYears = mcYears[1],
      timeStep = "daily",
      opts = opts,
      showProgress = FALSE
    )$area
  )
  weArea <- unique(
    antaresRead::readAntares(
      areas = "all",
      mcYears = mcYears[1],
      timeStep = "weekly",
      opts = opts,
      showProgress = FALSE
    )$area
  )
  moArea <- unique(
    antaresRead::readAntares(
      areas = "all",
      mcYears = mcYears[1],
      timeStep = "monthly",
      opts = opts,
      showProgress = FALSE
    )$area
  )
  annualArea <- unique(
    antaresRead::readAntares(
      areas = "all",
      mcYears = mcYears[1],
      timeStep = "annual",
      opts = opts,
      showProgress = FALSE
    )$area
  )
  
  
  
  dayLink <- unique(
    antaresRead::readAntares(
      links = "all",
      mcYears = mcYears[1],
      timeStep = "daily",
      opts = opts,
      showProgress = FALSE
    )$link
  )
  weLink <- unique(
    antaresRead::readAntares(
      links = "all",
      mcYears = mcYears[1],
      timeStep = "weekly",
      opts = opts,
      showProgress = FALSE
    )$link
  )
  moLink <- unique(
    antaresRead::readAntares(
      links = "all",
      mcYears = mcYears[1],
      timeStep = "monthly",
      opts = opts,
      showProgress = FALSE
    )$link
  )
  annualLink <- unique(
    antaresRead::readAntares(
      links = "all",
      mcYears = mcYears[1],
      timeStep = "annual",
      opts = opts,
      showProgress = FALSE
    )$link
  )
  
  
  
  if (nbcl > 1) {
    parallel <- TRUE
  } else{
    parallel <- FALSE
  }
  
  if (parallel) {
    cl <- makeCluster(nbcl)
    clusterExport(
      cl,
      c(
        "dayArea",
        "weArea",
        "moArea",
        "annualArea",
        "opts",
        "dayLink",
        "weLink",
        "moLink",
        "annualLink"
      ),
      envir = environment()
    )
    clusterEvalQ(cl, {
      library(antaresRead)
      library(antaresEditObject)
      library(data.table)
      opts <- antaresRead::setSimulationPath(opts$simPath)
    })
    registerDoParallel(cl)
  } else{
    cl <- NULL
  }
  
  if ("areas" %in% type) {
    if (verbose == 1) {
      cat("Start computing areas\n")
    }
    
    
    pblapply(mcYears,
             function(mcYear) {
               cpt_timstamp(mcYear, opts, dayArea, weArea, moArea, annualArea)
             },
             cl = cl)
    
    
  }
  
  
  if ("links" %in% type) {
    if (verbose == 1) {
      cat("Start computing links\n")
    }
    
    pblapply(mcYears,
             function(mcYear) {
               cpt_timstamp(mcYear, opts, dayLink, weLink, moLink, annualLink, type = "links")
             },
             cl = cl)
  }
  
  if ("clusters" %in% type) {
    if (verbose == 1) {
      cat("Start computing cluster\n")
    }
    
    pblapply(mcYears,
             function(mcYear) {
               cpt_timstamp(mcYear, opts, dayArea, weArea, moArea, annualArea, type = "clusters")
             },
             cl = cl)
  }
  
  
  if (parallel) {
    stopCluster(cl)
  }
  
  if (verbose == 1) {
    cat('Done\n')
  }
  
}

#' Computation function for rebuild mc-ind daily, weekly and monthly from hourly data.
#'
#' @param Year mcyear to compute
#' @param opts antares opts
#' @param dayArea areas to compute for daily
#' @param weArea areas to compute for weArea
#' @param moArea areas to compute for moArea
#' @param annualArea areas to compute for annualArea
#' @param type type of data to write (area, link, ....)
#'
#' @noRd
cpt_timstamp <- function(Year,
                         opts,
                         dayArea,
                         weArea,
                         moArea,
                         annualArea,
                         type = "areas") {
  if (type == "areas") {
    hourlydata <-
      antaresRead::readAntares(
        areas = "all",
        mcYears = Year,
        timeStep = "hourly",
        opts = opts,
        showProgress = FALSE
      )
    colForMean = c("MRG. PRICE", "H. LEV", "LOLP")
  }
  
  if (type == "links") {
    hourlydata <-
      antaresRead::readAntares(
        links = "all",
        mcYears = Year,
        timeStep = "hourly",
        opts = opts,
        showProgress = FALSE
      )
    colForMean = NULL
  }
  
  
  
  if (type == "clusters") {
    hourlydata <-
      antaresRead::readAntares(
        clusters = "all",
        mcYears = Year,
        timeStep = "hourly",
        opts = opts,
        showProgress = FALSE
      )
    colForMean = NULL
  }
  
  
  hourlydata$time <- as.Date(hourlydata$time)
  ##Hourly to daily
  ood <- .aggregateMc(hourlydata, colForMean = colForMean)
  attributes(ood)$timeStep <- "daily"
  
  .writeDT(ood, type, dayArea)
  
  ##Hourly to weekly
  om <- hourlydata
  
  sem <- .give_week_day(opts, om$time)
  om$time <-
    paste0(year(hourlydata$time),
           "-w",
           formatC(
             sem,
             width = 2,
             format = "d",
             flag = "0"
           ))
  om$day <- NULL
  om$month <- NULL
  ood <- .aggregateMc(om, colForMean = colForMean)
  attributes(ood)$timeStep <- "weekly"
  
  .writeDT(ood, type, weArea)
  
  ##Hourly to monthly
  om <- hourlydata
  om$time <-
    paste0(year(hourlydata$time),
           "-",
           formatC(
             month(hourlydata$time) - 1,
             width = 2,
             format = "d",
             flag = "0"
           ))
  om$day <- NULL
  ood <- .aggregateMc(om, colForMean = colForMean)
  ood$time <- as.factor(ood$time)
  attributes(ood)$timeStep <- "monthly"
  
  .writeDT(ood, type, moArea)
  
  ##Hourly to annual
  om <- hourlydata
  om$time <- om$day <- om$month <- NULL
  ood <- .aggregateMc(om, colForMean = colForMean)
  attributes(ood)$timeStep <- "annual"
  .writeDT(ood, type, annualArea)
  
  
}

.aggregateMc <- function(oo,
                         colForMean = c("MRG. PRICE", "H. LEV", "LOLP")) {
  oo$hour <- NULL
  initalOrder <- names(oo)
  oo$timeId <- NULL
  idc <- antaresRead::getIdCols(oo)
  
  if (!is.null(colForMean)) {
    colForMean <- colForMean[colForMean %in% names(oo)]
    sumC <-
      names(oo)[!names(oo) %in% idc & !names(oo) %in% colForMean]
    ooSum <- oo[, lapply(.SD, sum), by = idc, .SDcols = sumC]
    ooMean <- oo[, lapply(.SD, function(X) {
      round(mean(X), 2)
    }), by = idc, .SDcols = colForMean]
    re <- merge(ooSum, ooMean, by = idc)
  } else {
    sumC <- names(oo)[!names(oo) %in% idc]
    re <- oo[, lapply(.SD, sum), by = idc, .SDcols = sumC]
  }
  
  
  
  if (is.null(re$time)) {
    re$timeId <- "Annual"
    data.table::setcolorder(re, initalOrder)
    setnames(re, "timeId", "annual")
  } else{
    seqL <- 1:length(unique(re$time))
    re$timeId <- rep(seqL, nrow(re) / length(seqL))
    data.table::setcolorder(re, initalOrder)
  }
  re
}


.testDT <- function(dt, dt2, seuil) {
  max(unlist(lapply(names(dt), function(X) {
    if (is.numeric(dt[[X]])) {
      max(dt[[X]] - dt2[[X]])
    }
  })), na.rm = T) < seuil
}



.give_week_day <- function(opts, date) {
  dw <-
    c("Monday",
      "Tuesday",
      "Wednesday",
      "Thursday",
      "Friday",
      "Saturday",
      "Sunday")
  wd <- opts$firstWeekday
  fj <-
    antaresEditObject::readIniFile(file.path(opts$studyPath, "settings", "generaldata.ini"))
  fj <- fj$general$january.1st
  fd <- which(wd == dw)
  fj <- which(fj == dw)
  
  if (fj == fd) {
    sem <- c(rep(1:7, 100))
  } else {
    if (fd > fj) {
      dinFS <- fd - fj
      sem <- c((7 - dinFS + 1):7, rep(1:7, 100))
    } else {
      dinFS <- fd - fj
      sem <- c((fj - fd + 1):7,  rep(1:7, 100))
    }
  }
  
  k <- 1
  o <- NULL
  for (i in sem) {
    o <- c(o, k)
    if (i == 7) {
      k = k + 1
    }
  }
  is.leapyear = function(year) {
    return(((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0))
  }
  
  
  
  bis <- is.leapyear(year(date))
  feb29 <- 31 + 28
  
  days <- yday(date)
  days[bis & days > feb29] <- days[bis & days > feb29] - 2
  sem <- o[days]
  sem
  
}

.writeDT <- function(data, type, filtertable) {
  area <- link <- NULL
  if (type == "areas") {
    data <- data[area %in% filtertable]
    antaresEditObject::writeOutputValues(data = data)
  }
  
  if (type == "links") {
    data <- data[link %in% filtertable]
    antaresEditObject::writeOutputValues(data = data)
  }
  
  if (type == "clusters") {
    data <- data[area %in% filtertable]
    antaresEditObject::writeOutputValues(data = data)
  }
  
}
