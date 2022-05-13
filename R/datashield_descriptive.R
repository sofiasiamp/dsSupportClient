#'
#' @title Descriptive function for DataSHIELD
#' @description The function summarises the outcome of variable-level aggregate DataSHIELD functions.
#' @details datashield_descriptive functions creates summaries for all variables in a data.frame with respect to certain
#' DataSHIELD aggregate functions providing an improved overview for a DataSHIELD analyst.
#' @param dsfunction The function you want to run (ds.class, ds.numNA, ds.length)
#' @param opal_connection An Opal connection
#' @param df a data.frame on the server-side
#' @param save if TRUE, the output is saved in the working directory as a csv file
#' @return A table with the output of the function, if more than one study, they are joined in one table
#' @author Sofia Siampani (Max-Delbrueck-Center Berlin), Florian Schwarz (German Institute of Human Nutrition)
#' @import plyr
#' @export
#' @examples datashield_descriptive(ds.class, opals, df = "D_Clean")
#'


datashield_descriptive <- function(df = "D", dsfunction = NULL, datasources = NULL,  save = FALSE){


  # look for DS connections
  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }


  summary <-data.frame()
  join <- list()

  for (p in 1:length(datasources)){

    study <- datasources[p]
    col <- ds.colnames(df,study)
    colNames <- paste0(paste0(datasources[[p]]@name),".",(strsplit(as.character(substitute(dsfunction)), ".",fixed =TRUE))[[1]][2])

    y <-data.frame()

    for(i in col[[1]]) {

      var <- paste0(df,"$",i)
      y[i,colNames] <- dsfunction(var, datasources= study)[1]
    }

    y$rn <- rownames(y)
    join[[p]]<- y
  }

  summary <- plyr::join_all(join, by = 'rn', type="full")
  rownames(summary) <- summary$rn
  summary$rn <-NULL
  return(summary)
}
