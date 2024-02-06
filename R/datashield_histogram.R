#' DataSHIELD helper function for histograms
#'
#' @param datasources An Opal connection
#' @param method Type of method for building the histogram
#' @return It creates a directory with subdirectories of each study, containing histograms of all variables
#' @export
#' @importFrom grDevices dev.off png
#'

datashield_histogram<- function(datasources = NULL, method = "deterministic" ) {


  if(is.null(datasources)){
    datasources <- DSI::datashield.connections_find()
  }


  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
  }

  dir.create("histograms")
  setwd("histograms")
  for (p in 1:length(datasources)){
    study <- datasources[p]
    col <- dsBaseClient::ds.colnames("D",study)
    studyName <- study[[1]]@name
    dir.create(studyName)
    for(i in col[[1]]) {
      var <- paste0("D$",i)
      a <- dsBaseClient::ds.class(var,study)
      if ((a== "numeric" || a== "integer") && dsBaseClient::ds.numNA(var,study)< dsBaseClient::ds.length(var, datasources = study)[[1]] && dsBaseClient::ds.mean(var, datasources = study)[[1]][1]){
        a <- dsBaseClient::ds.histogram(var, datasources = study, method = method)
        print(a)
        grDevices::png(filename=paste0("~/histograms/",studyName,"/",studyName,"_",i,".png"))
        dsBaseClient::ds.histogram(var, datasources = study)
        grDevices::dev.off()
      }
    }
  }
  setwd('..')
}
