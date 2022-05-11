#' DataSHIELD descriptive function
#'
#' @param dsfunction The function you want to run (ds.class, ds.numNA, ds.length)
#' @param opal_connection An Opal connection
#' @param save if TRUE, the output is saved in the working directory as a csv file
#'
#' @return A table with the output of the function, if more than one study, they are joined in one table
#' @export
#' @import plyr
#' @examples datashield_descriptive(ds.class, opals)
datashield_descriptive<-
  function(dsfunction, opal_connection, save = FALSE, df = "D") {
  summary <-data.frame()
  join <- list()
  for (p in 1:length(opal_connection)){
    y <-data.frame()
    study <- opal_connection[p]
    col <- ds.colnames(df,study)
    colNames <- paste0(paste0(opal_connection[[p]]@name),".",(strsplit(as.character(substitute(dsfunction)), ".",fixed =TRUE))[[1]][2])
    for(i in col[[1]]) {
      var <- paste0(df,"$",i)
      y[i,colNames] <- dsfunction(var, datasources= study)[1]
    }
    y$rn <- rownames(y)
    join[[p]]<- y
  }
  summary <- join_all(join, by = 'rn', type="full")
  rownames(summary) <- summary$rn
  summary$rn <-NULL
  return(summary)
}
