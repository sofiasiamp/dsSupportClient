
#' It runs the datashield function ds.summary()and calculates the standard deviation in all numeric and integer variables of each OpalConnection
#'
#' @param opal_connection An Opal connection
#' @param save_summary if TRUE, the output is saved in the working directory as a csv file
#' @param df specifies the  df that was assigned in the login, default is "D"
#' @return a list with the summary of each variable
#' @export
#'
#' @examples datashield_summary(opals)

datashield_summary<- function(opal_connection, save_summary = FALSE, df = "D") {
  summary <- list()
  for (p in 1:length(opal_connection)){
    y = data.frame()
    study <- opal_connection[p]
    col <- ds.colnames(df,study)
    for(i in col[[1]]) {
      var <- paste0(df,"$",i)
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
