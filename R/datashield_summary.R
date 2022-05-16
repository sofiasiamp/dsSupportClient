#'
#' @title Descriptive function for DataSHIELD
#' @description The function summarises the outcome of variable-level aggregate DataSHIELD functions.
#' @details datashield_descriptive functions creates summaries for all variables in a data.frame with respect to certain
#' DataSHIELD aggregate functions providing an improved overview for a DataSHIELD analyst.
#' It runs the datashield function ds.summary()and calculates the standard deviation in all numeric and integer variables of each OpalConnection
#'
#' @param opal_connection An Opal connection
#' @param save_summary if TRUE, the output is saved in the working directory as a csv file
#'
#' @return a list with the summary of each variable
#' @export
#'
#' @examples datashield_summary(opals)

datashield_summary<- function(opal_connection, save_summary = FALSE) {
  summary <- list()
  for (p in 1:length(opal_connection)){
    y = data.frame()
    study <- opal_connection[p]
    col <- ds.colnames("D",study)
    for(i in col[[1]]) {
      var <- paste0("D$",i)
      a <- ds.class(var,study)
      if ((a== "numeric" || a== "integer") && ds.numNA(var,study)< ds.length(var, datasources = study)[[1]] ) {
        a<- unlist(ds.summary(var, datasources = study))
        y[i, c(names(a),"Standard deviation")] <- c(a,sqrt(ds.var(var, datasources = study)[["Variance.by.Study"]][1]))
      }
    }
    summary[[as.name(paste0(opal_connection[[p]]@name))]] <- y
    if (save_summary == TRUE) {
      write.csv(as.matrix(y), as.character(paste0(opal_connection[[p]]@name,"_summary.csv")) ,row.names = TRUE)
    }
  }
  return(summary)
}
