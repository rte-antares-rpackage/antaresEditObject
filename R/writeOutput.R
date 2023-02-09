
.writeAntaresOutput <- function(file_name, file_name_new, datatp, nrowkeep = 7, opts = simOptions()){
  
  name_write <- names(datatp)[!names(datatp)%in%c(antaresRead::getIdCols(datatp), "annual")]
  name_write_echap <- paste0(name_write, collapse = "\t")
  
  if (file.exists(file_name)){
    D <- readLines(file_name, nrowkeep)
  } else {D <- .createColumns(datatp, name_write, name_write_echap, opts)}
  
  zz <- file(file_name_new, "w")  # open an output file connection
  
  lapply(D, function(X){
    writeLines(X, zz)}
  )
  
  selr <- datatp[, .SD, .SDcols = name_write]
  
  x <- apply( selr, 1 , function(X){
    X[is.na(X)] <- "N/A"
    paste0(X, collapse = "\t")
  })
  
  if(!attributes(datatp)$timeStep %in% c("weekly", "annual")){
    
    if(attributes(datatp)$timeStep == "monthly"){
      mo <- substr(datatp$time, 6, 7)
    }else{
      day <- formatC(data.table::mday(datatp$time), width = 2, format = "d", flag = "0")
      
      mo <- data.table::month(datatp$time)
    }
    
    
    
    mo <- as.character(mo)
    mo[mo == "1" | mo == "01"] <- "JAN"
    mo[mo == "2" | mo == "02"] <- "FEB"
    mo[mo == "3" | mo == "03"] <- "MAR"
    mo[mo == "4" | mo == "04"] <- "APR"
    mo[mo == "5" | mo == "05"] <- "MAY"
    mo[mo == "6" | mo == "06"] <- "JUN"
    mo[mo == "7" | mo == "07"] <- "JUL"
    mo[mo == "8" | mo == "08"] <- "AUG"
    mo[mo == "9" | mo == "09"] <- "SEP"
    mo[mo == "10"] <- "OCT"
    mo[mo == "11"] <- "NOV"
    mo[mo == "12"] <- "DEC"
  }
  
  
  if(attributes(datatp)$timeStep == "monthly"){
    ro <- paste0("\t", datatp$timeId,"\t", mo, "\t", x, "\n", collapse = "")
  }
  
  if(attributes(datatp)$timeStep == "weekly"){
    ro <- paste0("\t", datatp$timeId,"\t", x, "\n", collapse = "")
  }
  
  if(attributes(datatp)$timeStep == "hourly"){
    ro <- paste0("\t", datatp$timeId,"\t",
                 day, "\t", mo, "\t", format(datatp$time,'%H:%M'),
                 "\t", x, "\n", collapse = "")
  }
  
  if(attributes(datatp)$timeStep == "daily"){
    ro <- paste0("\t", datatp$timeId,"\t",
                 day, "\t", mo, "\t",
                 x, "\n", collapse = "")
  }
  
  if(attributes(datatp)$timeStep == "annual"){
    ro <- paste0("\t", x, "\n", collapse = "")
  }
  
  writeChar(ro, zz, eos = NULL)
  close(zz)
  
  file.remove(file_name)
  file.rename(file_name_new, file_name)
  
}

# Write 7 first lines for values files (areas/links)
.createColumns <- function(datatp, name_write, name_write_echap, opts = simOptions()){
  val <- switch(attributes(datatp)$type,
                "areas" = "va",
                "links" = "va",
                "clusters" = "de",
                "clustersRes" = "res")
  timecols <- switch(attributes(datatp)$timeStep,
                     "daily" = c("day", "month"),
                     "weekly" = character(0),
                     'monthly' = "month",
                     "annual" = character(0))
  
  idx_col <- switch(attributes(datatp)$timeStep,
                    "daily" = "index",
                    "weekly" = "week",
                    "monthly" = "index",
                    "annual" = "")
  
  ar_name <- as.character(unique(datatp[[1]]))
  ar_name1 <- strsplit(ar_name, " - ")[[1]][1]
  ar_name2 <- ifelse(attributes(datatp)$type == "links", strsplit(ar_name, " - ")[[1]][2], "")
  
  table_agg <- ifelse(opts$antaresVersion > 810 && opts$parameters$`other preferences`$`renewable-generation-modelling` == "clusters",
                      "tableOutput_aggreg_v8.csv",
                      "tableOutput_aggreg.csv")
  
  table_agg <- fread(system.file(file.path("format_output",table_agg), package = "antaresRead"))
  table_agg <- table_agg[Mode == tolower(opts$mode) & Stats %in% c("EXP", "values") & Name %in% name_write, 
                         c("Name", "Unit")]
  units <- table_agg[order(factor(Name, levels = name_write))]$Unit
  units_echap <- paste0(units, collapse = "\t")
  
  line1 <- paste0(c(ar_name1, colnames(datatp)[1], val, attributes(datatp)$timeStep), collapse = "\t")
  
  line2 <- paste0(ar_name2,"\tVARIABLES\tBEGIN\tEND")
  
  line3 <- paste0(c("", length(name_write),1,nrow(datatp)), collapse = "\t")
  
  line4 <- ""
  
  line5 <- paste0(c(ar_name1, attributes(datatp)$timeStep, rep("",length(timecols)), name_write_echap), collapse = "\t")
  
  line6 <- paste0(c("", rep("",length(timecols) + 1), units_echap), collapse = "\t")
  
  line7 <- paste0(c("", idx_col, timecols), collapse = "\t")
  
  c(line1,line2,line3,line4,line5,line6,line7)
}
