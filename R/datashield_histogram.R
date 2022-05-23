#' DataSHIELD helper function for histograms
#'
#' @param opal_connection An Opal connection
#' @param method
#'
#' @return It creates a directory with subdirectories of each study, containing histograms of all variables
#' @export
#'
#' @examples datashield_histogram(opal_connection = opal_connection)
datashield_histogram<- function(opal_connection, method = "deterministic" ) {
  dir.create("histograms")
  setwd("histograms")
  for (p in 1:length(opal_connection)){
    study <- opal_connection[p]
    col <- ds.colnames("D",study)
    studyName <- study[[1]]@name
    dir.create(studyName)
    for(i in col[[1]]) {
      var <- paste0("D$",i)
      a <- ds.class(var,study)
      if ((a== "numeric" || a== "integer") && ds.numNA(var,study)< ds.length(var, datasources = study)[[1]] && ds.mean(var, datasources = study)[[1]][1]){
        a <- ds.histogram(var, datasources = study, method = method)
        print(a)
        png(filename=paste0("~/histograms/",studyName,"/",studyName,"_",i,".png"))
        ds.histogram(var, datasources = study)
        dev.off()
      }
    }
  }
  setwd('..')
}
