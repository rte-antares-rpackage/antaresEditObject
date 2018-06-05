
utils::globalVariables(c("rn", "variable", "value"))

#TODO to copy/paste to antaresRead in a next release. 
.getLinkName<-function(areaX=NULL, areaY=NULL){
  if(areaX<areaY){
    c1<-areaX
    c2<-areaY
  }else{
    c1<-areaY
    c2<-areaX
  }
  return(tolower(paste0(c1, " - ", c2)))
}

#TODO to copy/paste to antaresRead in a next release. 
.checkAreName <- function(area = NULL){
  if (!(tolower(area) %in% antaresRead::getAreas())){
    stop(paste0(area, " is not a correct area name."))
  }
}