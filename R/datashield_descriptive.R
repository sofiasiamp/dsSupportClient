#'
#' @title Descriptive function for DataSHIELD
#' @description The function summarises the outcome of variable-level aggregate DataSHIELD functions.
#' @details datashield_descriptive functions creates summaries for all variables in a data.frame with respect to certain
#' DataSHIELD aggregate functions providing an improved overview for a DataSHIELD analyst.
#' @param dsfunction The DataSHIELD function you want to run (e.g. ds.class)
#' @param datasources a list of \code{\link{DSConnection-class}} objects obtained after login.
#' If the \code{datasources} argument is not specified the default set of connections will be
#' used: see \code{\link{datashield.connections_default}}.
#' @param df a data.frame on the server-side
#' @param save if TRUE, the output is saved in the working directory as a csv file
#' @return A table with the output of the function, if more than one study, they are joined in one table
#' @author Sofia Siampani (Max-Delbrueck-Center), Florian Schwarz (German Institute of Human Nutrition)
#' @import plyr
#' @examples
#' #' \dontrun{
#'
#' # Version 1.0
#' # Connecting to Opal Servers
#'
#' # loading necessary packages
#' require("DSI")
#' require("DSOpal")
#' require("dsBaseClient")
#' require("datashieldDescriptives")
#'
#' builder <- DSI::newDSLoginBuilder()
#' builder$append(server = "study1",
#'                url = "http://192.168.56.100:8080/",
#'                user = "administrator", password = "datashield_test&",
#'                table = "CNSIM.CNSIM1", driver = "OpalDriver")
#' builder$append(server = "study2",
#'                url = "http://192.168.56.100:8080/",
#'                user = "administrator", password = "datashield_test&",
#'                table = "CNSIM.CNSIM2", driver = "OpalDriver")
#' builder$append(server = "study3",
#'                url = "http://192.168.56.100:8080/",
#'                user = "administrator", password = "datashield_test&",
#'                table = "CNSIM.CNSIM3", driver = "OpalDriver")
#'
#' logindata <- builder$build()
#'
#' # Log onto the remote Opal training servers
#' connections <- DSI::datashield.login(logins = logindata, assign = TRUE, symbol = "D")
#'
#' # Retrieving information on variable classes in the specified data.frame
#' datashield_descriptive(df = "D", dsfunction = ds.class)
#'
#'
#' # Retrieving information on how many NAs are present in each variable in the specified data.frame
#' datashield_descriptive(df = "D", dsfunction = ds.numNA)
#'
#'
#' # Clear the Datashield R sessions and logout
#' datashield.logout(connections)
#'
#' }
#' @export
#'


datashield_descriptive <- function(df = "D", dsfunction = NULL, datasources = NULL,  save = FALSE){


  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }


  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
  }


  if(is.null(dsfunction)){
    stop("You need to specify a DataSHIELD function.")
  }


  defined <- dsBaseClient:::isDefined(datasources, df)


  summary <- data.frame()
  join <- list()

  for (p in 1:length(datasources)){

    col <- ds.colnames(df,datasources[p])
    colNames <- paste0(datasources[[p]]@name,".",(strsplit(as.character(substitute(dsfunction)), ".",fixed =TRUE))[[1]][2])

    y <-data.frame()

    for(i in col[[1]]) {

      var <- paste0(df,"$",i)
      y[i,colNames] <- dsfunction(var, datasources = datasources[p])[1]
    }

    y$rn <- rownames(y)
    join[[p]] <- y
  }

  summary <- plyr::join_all(join, by = 'rn', type="full")

  rownames(summary) <- summary$rn

  summary$rn <- NULL

  return(summary)
}
