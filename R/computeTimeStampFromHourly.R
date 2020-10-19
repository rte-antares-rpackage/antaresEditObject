#' Compute daily, weekly, monthly and annual mc-ind from hourly data.
#' 
#' @param opts opts simulation path
#' @param mcYears mcYears to compute
#' @param nbcl number of thread for parallel computing.
#' 
#' @examples
#' \dontrun{
#' 
#' library(antaresEditObject)
#' opts <- setSimulationPath("C:/Users/TitouanRobert/Desktop/Projet/RTE/antares/etude/new/BP19_FB18_2021_60mcAcc/output/20201015-0957eco-test3a")
#' computeTimeStampFromHourly(opts)
#' 
#' }
#' 
#' @import doParallel pbapply parallel
#' 
#' @export
computeTimeStampFromHourly <- function(opts, mcYears = "all", nbcl = 8, verbose = 1){
  
  if(verbose == 0){
    pboptions(type = "none")
  }
  if(verbose == 1){
    pboptions(type = "txt")
  }
  
  if(verbose == 1){
    cat("Start compute all mc ind from hourly data\n")
  }
  if(mcYears[1] == "all"){
    mcYears <- opts$mcYears
  }
  if(verbose == 1){
  cat("Load necessary data\n")
  }
  dayArea <- unique(readAntares(areas = "all", mcYears = 1, timeStep = "daily", opts = opts, showProgress = FALSE)$area)
  weArea <- unique(readAntares(areas = "all", mcYears = 1, timeStep = "weekly", opts = opts, showProgress = FALSE)$area)
  moArea <- unique(readAntares(areas = "all", mcYears = 1, timeStep = "monthly", opts = opts, showProgress = FALSE)$area)
  annualArea <- unique(readAntares(areas = "all", mcYears = 1, timeStep = "annual", opts = opts, showProgress = FALSE)$area)
  
  if(nbcl>1){
    parallel <- TRUE
  }else{
    parallel <- FALSE
  }
  
  if(parallel){
    cl <- makeCluster(nbcl)
    clusterExport(cl, c("dayArea", "weArea", "moArea", "annualArea", "opts"), envir = environment())
    clusterEvalQ(cl, {
      library(antaresRead)
      library(antaresEditObject)
      library(data.table)
      opts <- setSimulationPath(opts$simPath)
    })
    registerDoParallel(cl)
  }else{
    cl <- NULL
  }
  
  if(verbose == 1){
    cat("Start computing\n")
  }
  
  pblapply(mcYears,
        function(mcYear){
          antaresEditObject:::cpt_timstamp(mcYear, opts, dayArea, weArea, moArea, annualArea)
        },
        cl = cl
  )
  
  if(parallel){
    stopCluster(cl)
  }
  
  if(verbose == 1){
    cat('Done\n')
  }
  
}

#' Computation function for rebuild mc-ind daily, weekly and monthly from hourly data.
#'
#' @param Year mcyear to compute
#' @param opts antares opts
#' @param dayArea areas to compute for daily
#' @param dayArea areas to compute for daily
#' @param weArea areas to compute for weArea
#' @param moArea areas to compute for moArea
#' @param annualArea areas to compute for annualArea
#'
#' @noRd
cpt_timstamp <- function(Year, opts, dayArea, weArea, moArea, annualArea){
  hourlydata <- readAntares(areas = "all", mcYears = Year, timeStep = "hourly",
                            opts = opts, showProgress = FALSE)
  
  hourlydata$time <- as.Date(hourlydata$time)
  ##Hourly to daily
  ood <- .aggregateMc(hourlydata)
  attributes(ood)$timeStep <- "daily"
  ood <- ood[area %in% dayArea]
  antaresEditObject::write_area_output_values(opts = opts, data = ood)
  
  ##Hourly to weekly
  om <- hourlydata
  
  sem <- .give_week_day(opts, om$time)
  
  
  om$time <- paste0(year(hourlydata$time), "-w", formatC(sem, width = 2, format = "d", flag = "0"))
  om$day <- NULL
  om$month <- NULL
  ood <- .aggregateMc(om)
  attributes(ood)$timeStep <- "weekly"
  ood <- ood[area %in% weArea]
  
  antaresEditObject::write_area_output_values(opts = opts, data = ood)
  
  
  ##Hourly to monthly
  om <- hourlydata
  om$time <- paste0(year(hourlydata$time), "-", formatC(month(hourlydata$time) - 1, width = 2, format = "d", flag = "0"))
  om$day <- NULL
  ood <- .aggregateMc(om)
  ood$time <- as.factor(ood$time)
  attributes(ood)$timeStep <- "monthly"
  ood <- ood[area %in% moArea]
  antaresEditObject::write_area_output_values(opts = opts, data = ood)
  
  
  ##Hourly to annual
  om <- hourlydata
  om$time <- om$day <- om$month <- NULL
  ood <- .aggregateMc(om)
  attributes(ood)$timeStep <- "annual"
  ood <- ood[area %in% annualArea]
  antaresEditObject::write_area_output_values(opts = opts, data = ood)
  
}

.aggregateMc <- function(oo, colForMean = c("MRG. PRICE", "H. LEV", "LOLP")){
  oo$hour <- NULL
  initalOrder <- names(oo)
  oo$timeId <- NULL
  idc <- getIdCols(oo)
  
  colForMean <- colForMean[colForMean %in% names(oo)]
  sumC <- names(oo)[!names(oo) %in% idc & !names(oo) %in% colForMean]
  ooSum <- oo[, lapply(.SD, sum), by = idc, .SDcols = sumC]
  ooMean <- oo[, lapply(.SD, function(X){
    round(mean(X), 2)
  }), by = idc, .SDcols = colForMean]
  re <- merge(ooSum, ooMean, by = idc)
  if(is.null(re$time)){
    re$timeId <- "Annual"
    setcolorder(re, initalOrder)
    setnames(re, "timeId", "annual")
  }else{
    seqL <- 1:length(unique(re$time))
    re$timeId <- rep(seqL, nrow(re)/length(seqL))
    setcolorder(re, initalOrder)
  }
  re
}


.testDT <- function(dt, dt2, seuil){
  max(unlist(lapply(names(dt), function(X){
    if(is.numeric(dt[[X]])){
      max(dt[[X]] - dt2[[X]])
    }
  })), na.rm = T)<seuil
}



.give_week_day <- function(opts, date){
  dw <- c("Monday", "Tuesday", "Wednesday", "Thursday", "Friday", "Saturday", "Sunday")
  wd <- opts$firstWeekday
  fj <- antaresEditObject::readIniFile(file.path(opts$studyPath,"settings", "generaldata.ini"))
  fj <- fj$general$january.1st
  fd <- which(wd == dw)
  fj <- which(fj == dw)
  
  if(fj == fd){
    sem <- c(rep(1:7, 100))
  }else{
    if(fd>fj){
      dinFS <- fd - fj
      sem <- c((7 - dinFS + 1):7, rep(1:7, 100))
    }else{
      dinFS <- fd - fj
      sem <- c((fj - fd + 1):7,  rep(1:7, 100))
    }
  }
  
  k <- 1
  o <- NULL
  for(i in sem){
    o <- c(o, k)
    if(i == 7){k = k + 1}
  }
  is.leapyear = function(year){
    return(((year %% 4 == 0) & (year %% 100 != 0)) | (year %% 400 == 0))
  }
  
  
  
  bis <- is.leapyear(year(date))
  feb29 <- 31 + 28
  
  days <- yday(date)
  days[bis & days>feb29] <- days[bis & days>feb29] - 2
  sem <- o[days]
  sem
  
}

