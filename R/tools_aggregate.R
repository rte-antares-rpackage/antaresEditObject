
.tableAggregateload <- function(opts){
  Mode <- NULL
	linkTable <- try(data.table::fread(system.file("/format_output/tableOutput.csv", package = "antaresEditObject")),
					 silent = TRUE)
	linkTable$progNam <- linkTable$Stats
	linkTable$progNam[which(linkTable$progNam == "values")] <- "EXP"
	if(opts$mode == "Economy") {
		linkTable <- linkTable[Mode == "economy"]
	}else{
		linkTable <- linkTable[Mode != "economy"]
	}
}
.updateData <- function(opts, dta, linkTable){
  .N <- Name <- progNam <- NULL
	idC <- antaresRead::getIdCols(dta)
	calcC <- names(dta)[!names(dta)%in%idC]
	idC <- idC[idC != "mcYear"]

	dtamean <- dta[, lapply(.SD, function(X){(mean(X))}), .SDcols = calcC, by = idC]
	dtamin <- dta[, lapply(.SD, function(X){(min(X))}), .SDcols = calcC, by = idC]
	dtamax <- dta[, lapply(.SD, function(X){(max(X))}), .SDcols = calcC, by = idC]
	dtastd <- dta[, lapply(.SD, function(X){(sd(X)/(.N*2+1)*(.N*2))}), .SDcols = calcC, by = idC]
	outputData <- unique(dta[, .SD, .SDcols = idC])

	for(i in calcC){
		keep <- linkTable[Name  == i]
		if("EXP" %in% keep$progNam){
			outputData[[i]] <- round(dtamean[[i]],keep[progNam == "EXP"]$digits)
		}
		if("std" %in% keep$progNam){
			outputData[[paste0(i, "_std")]] <- round(dtastd[[i]],keep[progNam == "std"]$digits)
		}
		if("min" %in% keep$progNam){
			outputData[[paste0(i, "_min")]] <- round(dtamin[[i]],keep[progNam == "min"]$digits)
		}
		if("max" %in% keep$progNam){
			outputData[[paste0(i, "_max")]] <- round(dtamax[[i]],keep[progNam == "max"]$digits)
		}
	}
	attributes(outputData)$synthesis <- TRUE
	outputData
}
