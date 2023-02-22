#' @title Computation function for rebuild mc-ind daily, monthly and annual from hourly data.
#'
#' @param hourlydata antaresData for hourly timestep
#' @param timeStep timestep of aggregation (daily, monthly and annual, NO weekly)
#' @param type type of data (areas, links, clusters, clustersRes)
#' 
#' @import data.table
#'
#' @keywords internal
.hourlyToOther <- function(hourlydata, timeStep, type){
  # Get proper time from timeStep
  char_timeStep = switch(timeStep, 
                         "daily" = 10, #XXXX-XX-XX
                         "monthly" = 7, #XXXX-XX
                         "annual" = 4) #XXXX
  
  # Columns for aggregate based on timeStep
  agg_columns = switch(timeStep,
                       "daily" = c("mcYear", "time", "month", "day"),
                       "monthly" = c("mcYear", "time", "month"),
                       "annual" = c("mcYear", "time"))
  
  # Columns for aggregate based on type
  agg_columns = switch(type,
                       "areas" = c("area", agg_columns),
                       "links" = c("link", agg_columns),
                       "clusters" = c("area", "cluster", agg_columns),
                       "clustersRes" = c("area", "cluster", agg_columns))
  
  colForMean = switch(type,
                      "areas" = c("MRG. PRICE", "H. LEV"),
                      "links" = character(0),
                      "clusters" = character(0),
                      "clustersRes" = character(0))
  
  colForMax = switch(type,
                     "areas" = "LOLP",
                     "links" = c("CONG. PROB +",	"CONG. PROB -"),
                     "clusters" = character(0),
                     "clustersRes" = character(0))
  
  colorder <- colnames(hourlydata)
  idcols <- getIdCols(hourlydata)
  
  res <- copy(hourlydata)[, time := substr(as.character(time),1,char_timeStep)][, hour := NULL]
  if (timeStep == "annual") res[, time := "Annual"] #to groupby both years of horizon in one
  resSum <- res[, lapply(.SD, sum), 
                            .SDcols = colorder[!(colorder %in% idcols) & 
                                                 !(colorder %in% colForMean) & !(colorder %in% colForMax)],
                            by = agg_columns]
  
  if (length(colForMean) > 0){
    resMean <- res[, lapply(.SD, function(x){round(mean(x),2)}), .SDcols = colForMean, by = agg_columns]
    resFinal <- merge(resSum, resMean, by = agg_columns)
  } else resFinal <- resSum
  
  if (length(colForMax) > 0){
    resMax <- res[, lapply(.SD, max), .SDcols = colForMax, by = agg_columns]
    resFinal <- merge(resFinal, resMax, by = agg_columns)
  }

  if (timeStep == "annual") {
    setnames(resFinal, "time", "annual")
    colorder <- gsub("time", "annual", colorder)
  }
  
  setcolorder(resFinal, intersect(colorder, colnames(resFinal)))
}

#' @title Computation function for rebuild mc-ind weekly from daily data.
#'
#' @param dailydata antaresData for daily timestep
#' @param opts study opts
#' @param type type of data (areas, links, clusters, clustersRes)
#'
#' @keywords internal
.dailyToWeekly <- function(dailydata, opts = simOptions(), type){
  
  colorder <- gsub("time", "timeId", colnames(dailydata))
  colForMean = switch(type,
                      "areas" = c("MRG. PRICE", "H. LEV"),
                      "links" = character(0),
                      "clusters" = character(0),
                      "clustersRes" = character(0))
  
  colForMax = switch(type,
                     "areas" = "LOLP",
                     "links" = c("CONG. PROB +",	"CONG. PROB -"),
                     "clusters" = character(0),
                     "clustersRes" = character(0))
  
  # Columns for aggregate based on type
  agg_columns = switch(type,
                       "areas" = c("area", "mcYear", "year", "week"),
                       "links" = c("link", "mcYear", "year", "week"),
                       "clusters" = c("area", "cluster", "mcYear", "year", "week"),
                       "clustersRes" = c("area", "cluster", "mcYear", "year", "week"))
  
  weekdays <- c("Monday",
                "Tuesday",
                "Wednesday",
                "Thursday",
                "Friday",
                "Saturday",
                "Sunday")
  weekdays <- c(weekdays, weekdays)
  
  firstDayWeek <- opts$firstWeekday
  firstDayYear <- opts$parameters$general$january.1st
  year <- as.integer(substr(opts$parameters$general$horizon,6,10))
  if (is.na(year)) year <- as.integer(opts$parameters$general$horizon) + 1
  
  # Set up general week
  firstidx <- which(weekdays %in% firstDayWeek)[1]
  weekdays <- weekdays[firstidx:(firstidx+6)]
  
  # Set up year two
  alldaysYearTwo <- seq(as.Date(paste0(year,"-01-01")),as.Date(paste0(year,"-12-31")),by="1 day")
  leapYear <- paste0(year,"-02-29") %in% as.character(alldaysYearTwo)
  
  # Set up week/Year
  firstidx <- which(weekdays %in% firstDayYear)
  daysleftYearTwo <- daysleft <- length(weekdays[firstidx:length(weekdays)])
  weekVector <- rep(1,daysleft)
  weekVector <- c(weekVector, unlist(lapply(2:52, rep.int, times = 7)))
  weekVector <- c(weekVector, rep(1, ifelse(leapYear, 366, 365) - length(weekVector)))
  
  # First merge
  alldaysYearTwo <- data.table(day = alldaysYearTwo, week2 = weekVector)
  alldaysYearTwo[, year2 := year(day)]
  res <- merge(copy(dailydata)[, time := as.Date(time)], alldaysYearTwo, by.x = "time", by.y = "day", all.x = T)
  
  # Set up year One
  alldaysYearOne <- seq(as.Date(paste0(year-1,"-01-01")),as.Date(paste0(year-1,"-12-31")),by="1 day")
  leapYear <- paste0(year-1,"-02-29") %in% as.character(alldaysYearOne)
  
  # Set up week/Year
  firstidx <- (firstidx - ifelse(leapYear, 2, 1))%%7
  daysleft <- length(weekdays[firstidx:length(weekdays)])
  weekVector <- rep(1,daysleft)
  weekVector <- c(weekVector, unlist(lapply(2:52, rep.int, times = 7)))
  weekVector <- c(weekVector, rep(1, 7 - daysleftYearTwo))
  
  # Second merge
  alldaysYearOne <- data.table(day = alldaysYearOne, week1 = weekVector)
  alldaysYearOne[, year1 := year(day)]
  res <- merge(res, alldaysYearOne, by.x = "time", by.y = "day", all.x = T)
  
  res[, `:=` (week = ifelse(is.na(week1),week2,week1),
              year = ifelse(is.na(year1),year2,year1))][, c("week1", "week2", "year1", "year2") := NULL]
  res <- res[week == 1, year := 0]
  
  cols <- setdiff(colnames(res),c(getIdCols(dailydata),"week","year"))
  resSum <- res[, lapply(.SD, sum), by= agg_columns, 
                .SDcols = cols[!(cols %in% colForMean | cols %in% colForMax)]]
  
  if (length(colForMean) > 0){
    resMean <- res[, lapply(.SD, function(x){round(mean(x),2)}),.SDcols = colForMean, by= agg_columns]
    resFinal <- merge(resSum, resMean, by = agg_columns, sort = F)
  } else resFinal <- resSum
  
  if (length(colForMax) > 0){
    resMean <- res[, lapply(.SD, max),.SDcols = colForMax, by= agg_columns]
    resFinal <- merge(resFinal, resMean, by = agg_columns, sort = F)
  }

  resFinal[, timeId := week]
  
  setcolorder(resFinal, intersect(colorder, colnames(resFinal)))[, year := NULL]
}




#' @title Compute daily, weekly, monthly and annual mc-ind from hourly data for one year. (new)
#' 
#' @param mcYear vector of years to compute
#' @param type type of data (areas, links, clusters, clustersRes)
#' @param areas vector of areas. links type will use getLinks() to get data.
#' @param opts study opts
#' @param timeStep timestep of aggregation (daily, monthly and annual, NO weekly)
#' @param writeOutput boolean to write data in mc-ind folder
#' 
#' @seealso
#' \code{\link{computeOtherFromHourlyMulti}}
#' 
#' @export
computeOtherFromHourlyYear <- function(mcYear,
                                       type,
                                       areas = "all",
                                       opts = simOptions(),
                                       timeStep = c("daily", "monthly", "annual", "weekly"), 
                                       writeOutput = F){
  res <- list()
  if (length(areas) == 1 && areas == "all") selected <- "areas" #for the eval(parse(text))
  else if (type != "links") selected <- paste(list(areas), sep = ",")
  else selected <- paste(list(getLinks(areas, internalOnly = T, opts = opts)),
                         sep = ",")
  
  formula <- sprintf('readAntares(%s = %s, timeStep = "hourly",
                            mcYears = mcYear, showProgress = F, opts = opts)', 
                     type, selected)  #read any type data
  hourlydata <- eval(parse(text = formula))
  if (type == "clustersRes" && length(hourlydata) > 1) hourlydata <- hourlydata$clustersRes

  # Multi timestep at once
  # steps <- as.list(intersect(c("daily", "monthly", "annual"), timeStep))
  # res <- llply(steps, .hourlyToOther, hourlydata = hourlydata, type = type,
  #              .parallel = T, .paropts = list(preschedule=TRUE))
  # names(res) <- paste0(steps,"data")
  
  # Separate timesteps
  if ("daily" %in% timeStep) res$dailydata <- .hourlyToOther(hourlydata, timeStep = "daily", type = type)
  if ("monthly" %in% timeStep) res$monthlydata <- .hourlyToOther(hourlydata, timeStep = "monthly", type = type)
  if ("annual" %in% timeStep) res$annualdata <- .hourlyToOther(hourlydata, timeStep = "annual", type = type)

  if ("weekly" %in% timeStep){
    if (!"daily" %in% timeStep){
      warning("Daily mc-ind needed to compute weekly. Computing daily mc-ind...")
      res$weeklydata <- .dailyToWeekly(.hourlyToOther(hourlydata, timeStep = "daily", type = type), type = type, opts = opts)
    }
    else res$weeklydata <- .dailyToWeekly(res$dailydata, opts = opts, type = type)
  }

  rm(hourlydata)
  for (nm in names(res)){ #only loop works for writing attributes 
    setattr(res[[nm]], "type", type)
    setattr(res[[nm]], "synthesis", FALSE)
    setattr(res[[nm]], "timeStep", gsub("data", "", nm))
  }

  if (writeOutput){
    if (!is.null(res$dailydata)){
      if (type == "links") res$dailydata <- res$dailydata[, timeId := rep(1:nrow(unique(.SD[, c("day", "month")])), length(unique(link)))]
      else if (type == "areas") res$dailydata <- res$dailydata[, timeId := rep(1:nrow(unique(.SD[, c("day", "month")])), length(unique(area)))]
      else res$dailydata <- res$dailydata[, timeId := rep(1:nrow(unique(.SD[, c("day", "month")])), nrow(unique(.SD[, c("area", "cluster")])))]
    }

    if (!is.null(res$monthlydata)){
      if (type == "links") res$monthlydata <- res$monthlydata[, timeId := rep(1:12, length(unique(link)))]
      else if (type == "areas") res$monthlydata <- res$monthlydata[, timeId := rep(1:12, length(unique(area)))]
      else res$monthlydata <- res$monthlydata[, timeId := rep(1:12, nrow(unique(.SD[, c("area", "cluster")])))]
    }

    lapply(res, writeOutputValues, opts = opts)
    # llply(res, writeOutputValues, opts = opts, .parallel = T, .paropts = list(preschedule=TRUE))
  }
  
  res

}


#' @title Compute daily, weekly, monthly and annual mc-ind from hourly data multiyear. (new)
#' 
#' @param opts study opts
#' @param areas vector of areas
#' @param type type of aggregation
#' @param timeStep timestep of aggregation (daily, monthly and annual, NO weekly)
#' @param mcYears vector of years to compute
#' @param writeOutput boolean to write data in mc-ind folder
#' @param nbcl number of cpu cores for parallelization
#'
#' @importFrom plyr llply ldply 
#' @importFrom future plan
#' @importFrom doFuture registerDoFuture
#' 
#' @import progressr
#' 
#' @seealso
#' \code{\link{computeOtherFromHourlyYear}}
#' 
#' @export
computeOtherFromHourlyMulti <- function(opts = simOptions(),
                                        areas = "all",
                                        type = c("areas", "links", "clusters"),
                                        timeStep = c("daily", "monthly", "annual", "weekly"), 
                                        mcYears = simOptions()$mcYears,
                                        writeOutput = F,
                                        nbcl = 8,
                                        verbose = F){
  
  if (verbose){
    handlers(global = T)
    handlers("progress")
  }
  
  res <- list()
  
  parallel <- (nbcl > 1)
  #Parallel config####
  if (parallel){
    closeAllConnections()
    cl <- makeCluster(nbcl)
    plan("multisession")
    registerDoFuture()
    paropts <- list(preschedule=TRUE)
    clusterSetRNGStream(cl, 123)
    clusterExport(cl=cl, "opts",
                  envir=environment())
    clusterEvalQ(cl, library("antaresRead"))
  }
  
  if ("areas" %in% type){
    gc()
    exec_time <- Sys.time()
    if (verbose) cat(c("\nComputing :", timeStep, "mc-ind (areas) from hourly...\n"))
    resAreas <- llply(mcYears, computeOtherFromHourlyYear, opts = opts, writeOutput = writeOutput,
                      type = "areas", areas = areas, .parallel = parallel, .progress = "progressr",
                      .paropts = list(.options.snow = paropts))
    res$areas <- resAreas
    if (verbose){
      cat(c("Areas : OK\n"))
      print(Sys.time() - exec_time)
    } 
  }

  if ("links" %in% type){
    gc()
    exec_time <- Sys.time()
    if (verbose) cat(c("\nComputing :", timeStep, "mc-ind (links) from hourly...\n"))
    resLinks <- llply(mcYears, computeOtherFromHourlyYear, opts = opts, writeOutput = writeOutput,
                      type = "links", areas = areas, .parallel = parallel, .progress = "progressr",
                      .paropts = list(.options.snow = paropts))
    res$links <- resLinks
    if (verbose){
      cat(c("Links : OK\n"))
      print(Sys.time() - exec_time)
    } 
  }

  if ("clusters" %in% type){
    gc()
    exec_time <- Sys.time()
    if (verbose) cat(c("\nComputing :", timeStep, "mc-ind (clusters) from hourly...\n"))
    resClusters <- llply(mcYears, computeOtherFromHourlyYear, opts = opts, writeOutput = writeOutput,
                         type = "clusters", areas = areas, .parallel = parallel, .progress = "progressr",
                         .paropts = list(.options.snow = paropts))
    res$clusters <- resClusters
    if (verbose){
      cat(c("Clusters : OK\n"))
      print(Sys.time() - exec_time)
    }
  }

  # gc()
  # if (opts$antaresVersion >= 810 && opts$parameters$`other preferences`$`renewable-generation-modelling` == "clusters"){
  #   cat(c("\nComputing :", timeStep, "mc-ind (clusters Res) from hourly...\n"))
  #   resClustersRes <- llply(mcYears, computeOtherFromHourlyYear, opts = opts, writeOutput = writeOutput,
  #                        type = "clustersRes", areas = areas, .parallel = parallel, .progress = "progressr",
  #                        .paropts = list(.options.snow = paropts))
  #   cat(c("Clusters Res : OK\n"))
  #   res <- list(resAreas, resLinks, resClusters, resClustersRes)
  # } else res <- list(resAreas, resLinks, resClusters)
  # closeAllConnections()
  
  print("Success.")
  if (!writeOutput) return (res)
}
