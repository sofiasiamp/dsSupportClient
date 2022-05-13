#'
#' @title Descriptive function for DataSHIELD
#' @description The function summarises the outcome of variable-level aggregate DataSHIELD functions.
#' @details datashield_descriptive functions creates summaries for all variables in a data.frame with respect to certain
#' DataSHIELD aggregate functions providing an improved overview for a DataSHIELD analyst.
#' @param dsfunction The function you want to run (ds.class, ds.numNA, ds.length)
#' @param opal_connection An Opal connection
#' @param df data.frame
#' @param save if TRUE, the output is saved in the working directory as a csv file
<<<<<<< HEAD
=======
#' @param df specifies the  df that was assigned in the login, default is "D"
#'
>>>>>>> dff01e7a9f1570d06d2d2c91841336dbff2c9bcf
#' @return A table with the output of the function, if more than one study, they are joined in one table
#' @author Sofia Siampani (MDC), Florian Schwarz (DIfE)
#' @export
#' @import plyr
#' @examples datashield_descriptive(ds.class, opals)
<<<<<<< HEAD
#'


datashield_descriptive <- function(dsfunction, opal_connection, df = "D", save = FALSE){

=======
datashield_descriptive<-
  function(dsfunction, opal_connection, save = FALSE, df = "D") {
>>>>>>> dff01e7a9f1570d06d2d2c91841336dbff2c9bcf
  summary <-data.frame()
  join <- list()

  for (p in 1:length(opal_connection)){

    study <- opal_connection[p]
    col <- ds.colnames(df,study)
    colNames <- paste0(paste0(opal_connection[[p]]@name),".",(strsplit(as.character(substitute(dsfunction)), ".",fixed =TRUE))[[1]][2])

    y <-data.frame()

    for(i in col[[1]]) {
<<<<<<< HEAD

=======
>>>>>>> dff01e7a9f1570d06d2d2c91841336dbff2c9bcf
      var <- paste0(df,"$",i)
      y[i,colNames] <- dsfunction(var, datasources= study)[1]
    }

    y$rn <- rownames(y)
    join[[p]]<- y
  }
<<<<<<< HEAD

=======
>>>>>>> dff01e7a9f1570d06d2d2c91841336dbff2c9bcf
  summary <- plyr::join_all(join, by = 'rn', type="full")
  rownames(summary) <- summary$rn
  summary$rn <-NULL
  return(summary)
}
