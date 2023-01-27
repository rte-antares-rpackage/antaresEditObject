
.writeAntaresOutput <- function(file_name, file_name_new, datatp, nrowkeep = 7){
  
  name_write <- names(datatp)[!names(datatp)%in%antaresRead::getIdCols(datatp)]
  name_write_echap <- paste0(name_write, collapse = "\t")
  
  D <- readLines(file_name, nrowkeep)
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
