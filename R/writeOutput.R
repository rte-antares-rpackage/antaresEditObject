
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
  
  
  
  
  day <- formatC(data.table::mday(datatp$time), width = 2, format = "d", flag = "0")
  mo <- data.table::month(datatp$time)
  
  mo <- as.character(mo)
  mo[mo == "1"] <- "JAN"
  mo[mo == "2"] <- "FEB"
  mo[mo == "3"] <- "MAR"
  mo[mo == "4"] <- "APR"
  mo[mo == "5"] <- "MAY"
  mo[mo == "6"] <- "JUN"
  mo[mo == "7"] <- "JUL"
  mo[mo == "8"] <- "AUG"
  mo[mo == "9"] <- "SEP"
  mo[mo == "10"] <- "OCT"
  mo[mo == "11"] <- "NOV"
  mo[mo == "12"] <- "DEC"
  
  ro <- paste0("\t", datatp$timeId,"\t",
               day, "\t", mo, "\t", format(datatp$time,'%H:%M'),
               "\t", x, "\n", collapse = "")
  
  writeChar(ro, zz, eos = NULL)
  close(zz)
  
  file.remove(file_name)
  file.rename(file_name_new, file_name)
  
}
