#' Computation function for rebuild mc-ind daily, monthly and annual from hourly data.
#'
#' @param hourlydata antaresData for hourly timestep
#' @param timeStep timestep of aggregation (daily, monthly and annual, NO weekly)
#' @param type type of data (areas, links, clusters, clustersRes)
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
                      "areas" = c("MRG. PRICE", "H. LEV", "LOLP"),
                      "links" = c("MARG. COST",	"CONG. PROB +",	"CONG. PROB -"),
                      "clusters" = character(0),
                      "clustersRes" = character(0))
  
  colorder <- colnames(hourlydata)
  idcols <- getIdCols(hourlydata)
  
  res <- copy(hourlydata)[, time := substr(as.character(time),1,char_timeStep)][, hour := NULL]
  if (timeStep == "annual") res[, time := "Annual"] #to groupby both years of horizon in one
  resSum <- res[, lapply(.SD, sum), 
                            .SDcols = colorder[!(colorder %in% idcols) & !(colorder %in% colForMean)],
                            by = agg_columns]
  
  if (length(colForMean) > 0){
    resMean <- res[, lapply(.SD, function(x){round(mean(x),2)}), 
                   .SDcols = colForMean,
                   by = agg_columns]
    
    res <- merge(resSum, resMean, by = agg_columns)
  } else res <- resSum

  if (timeStep == "annual") {
    setnames(res, "time", "annual")
    colorder <- gsub("time", "annual", colorder)
  }
  
  setcolorder(res, intersect(colorder, colnames(res)))
}

#' Computation function for rebuild mc-ind weekly from daily data.
#'
#' @param dailydata antaresData for daily timestep
#' @param opts study opts
#' @param type type of data (areas, links, clusters, clustersRes)
#'
#' @keywords internal
.dailyToWeekly <- function(dailydata, opts = simOptions(), type){
  
  colorder <- gsub("time", "timeId", colnames(dailydata))
  
  colForMean = switch(type,
                      "areas" = c("MRG. PRICE", "H. LEV", "LOLP"),
                      "links" = c("MARG. COST",	"CONG. PROB +",	"CONG. PROB -"),
                      "clusters" = character(0),
                      "clustersRes" = character(0))
  
  # Columns for aggregate based on type
  agg_columns = switch(type,
                       "areas" = "area",
                       "links" = "link",
                       "clusters" = c("area", "cluster"),
                       "clustersRes" = c("area", "cluster"))
  
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
  resSum <- res[, lapply(.SD, sum), by= c(agg_columns, "mcYear", "year", "week"), .SDcols = setdiff(cols,colForMean)]
  if (length(colForMean) > 0){
    resMean <- res[, lapply(.SD, function(x){round(mean(x),2)}), by= c(agg_columns, "mcYear", "year", "week"), .SDcols = colForMean]
    res <- merge(resSum, resMean, by = c(agg_columns, "mcYear", "year", "week"), sort = F)
  } else res <- resSum

  res[, timeId := week]
  
  setcolorder(res, intersect(colorder, colnames(res)))[, year := NULL]
}




#' @title Compute daily, weekly, monthly and annual mc-ind from hourly data for one year. (new)
#' 
#' @param mcYear vector of years to compute
#' @param type type of data (areas, links, clusters, clustersRes)
#' @param areas vector of areas
#' @param opts study opts
#' @param timeStep timestep of aggregation (daily, monthly and annual, NO weekly)
#' @param writeOutput boolean to write data in mc-ind folder
#' 
#' @keywords internal
computeOtherFromHourlyYear <- function(mcYear,
                                       type,
                                       areas = "all",
                                       opts = simOptions(),
                                       timeStep = c("daily", "monthly", "annual", "weekly"), 
                                       writeOutput = F){
  
  res <- list()
  formula <- sprintf('readAntares(%s = %s, timeStep = "hourly",
                            mcYears = mcYear, showProgress = F, opts = opts)', type, 
                     ifelse(type == "links" && areas != "all", 
                            getLinks(areas = areas, internalOnly = T),
                            areas))  #read any type data
  hourlydata <- eval(parse(text = formula))
  if (type == "clustersRes" && length(hourlydata) > 1) hourlydata <- hourlydata$clustersRes

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
  }
  
  res

}


#' @title Compute daily, weekly, monthly and annual mc-ind from hourly data multiyear. (new)
#' 
#' @param opts study opts
#' @param areas vector of areas
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
#' @export
computeOtherFromHourlyMulti <- function(opts = simOptions(),
                                        areas = "all",
                                        timeStep = c("daily", "monthly", "annual", "weekly"), 
                                        mcYears = simOptions()$mcYears,
                                        writeOutput = F,
                                        nbcl = 8){
  handlers(global = T)
  handlers("progress")
  
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
  
  cat(c("\nComputing :", timeStep, "mc-ind (areas) from hourly...\n"))
  resAreas <- llply(mcYears, computeOtherFromHourlyYear, opts = opts, writeOutput = writeOutput,
               type = "areas", areas = areas, .parallel = parallel, .progress = "progressr",
               .paropts = list(.options.snow = paropts))
  cat(c("Areas : OK\n"))

  cat(c("\nComputing :", timeStep, "mc-ind (links) from hourly...\n"))
  resLinks <- llply(mcYears, computeOtherFromHourlyYear, opts = opts, writeOutput = writeOutput,
                    type = "links", areas = areas, .parallel = parallel, .progress = "progressr",
                    .paropts = list(.options.snow = paropts))
  cat(c("Links : OK\n"))

  cat(c("\nComputing :", timeStep, "mc-ind (clusters) from hourly...\n"))
  resClusters <- llply(mcYears, computeOtherFromHourlyYear, opts = opts, writeOutput = writeOutput,
                    type = "clusters", areas = areas, .parallel = parallel, .progress = "progressr",
                    .paropts = list(.options.snow = paropts))
  cat(c("Clusters : OK\n"))
  
  if (opts$antaresVersion >= 810 && opts$parameters$`other preferences`$`renewable-generation-modelling` == "clusters"){
    cat(c("\nComputing :", timeStep, "mc-ind (clusters Res) from hourly...\n"))
    resClustersRes <- llply(mcYears, computeOtherFromHourlyYear, opts = opts, writeOutput = writeOutput,
                         type = "clustersRes", areas = areas, .parallel = parallel, .progress = "progressr",
                         .paropts = list(.options.snow = paropts))
    cat(c("Clusters Res : OK\n"))
    res <- list(resAreas, resLinks, resClusters, resClustersRes)
  } else res <- list(resAreas, resLinks, resClusters)
  
  closeAllConnections()
  print("Success.")
  res
}
