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
#' @examples
#' \dontrun{
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
#' datashield_summary(df = "D", dsfunction = ds.class)
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


datashield_summary<- function(df = "D", datasources = NULL, save = FALSE){


  if(is.null(datasources)){
    datasources <- datashield.connections_find()
  }


  if(!(is.list(datasources) && all(unlist(lapply(datasources, function(d) {methods::is(d,"DSConnection")}))))){
    stop("The 'datasources' were expected to be a list of DSConnection-class objects", call.=FALSE)
  }


  #Check whether object are present in all datasources
  defined <- dsBaseClient:::isDefined(datasources, df)



  summary <- list()

  for (p in 1:length(datasources)){


    y <- data.frame()

    for (i in dsBaseClient::ds.colnames(df)[[1]]){

      var <- paste0(df,"$",i)
      a <- dsBaseClient::ds.class(var)

      if ((a == "numeric" || a == "integer") && (dsBaseClient::ds.numNA(var) < dsBaseClient::ds.length(var)[[1]])){
        a <- unlist(dsBaseClient::ds.summary(var))
        y[i, c(names(a),"Standard deviation")] <- c(a, sqrt(dsBaseClient::ds.var(var)[["Variance.by.Study"]][1]))
      }
    }

    summary[[as.name(paste0(datasources[[p]]@name))]] <- y
    if (save == TRUE){
      write.csv(as.matrix(y), paste0(datasources[[p]]@name,"_summary.csv"), row.names = TRUE)
      print(paste0("The summary file ", paste0(datasources[[p]]@name,"_summary.csv")," has been saved at ",getwd(), "."))
    }
  }

  return(summary)
}
