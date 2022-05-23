#' DataSHIELD descriptive function
#'
<<<<<<< HEAD
<<<<<<< HEAD
=======
>>>>>>> 1f4aa41ef13195a9b4e03ed03171a8f968684c4f
#' @title Descriptive function for DataSHIELD analysts
#' @description The function summarises the outcome of variable-level aggregate DataSHIELD functions.
#' @details datashield_descriptive functions creates summaries for all variables in a data.frame with respect to certain
#' DataSHIELD aggregate functions providing an improved overview for the DataSHIELD analyst.
#' @param dsfunction The aggregate DataSHIELD function you want to run to receive information (e.g. ds.class).
#' @param datasources A list of \code{\link{DSConnection-class}} objects obtained after login.
#' If the \code{datasources} argument is not specified the default set of connections will be
#' used: see \code{\link{datashield.connections_default}}.
#' @param df A data.frame on the server-side.
#' @param save If TRUE, the output is saved in the working directory as a csv file.
#' @return A table with the output of the function. If more than one study is connected, they are joined in one table.
#' @author Sofia Siampani (Max-Delbrueck-Center), Florian Schwarz (German Institute of Human Nutrition)
#' @import plyr
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
    stop("You need to specify an aggregate DataSHIELD function.")
  }


  #Check whether object are present in all datasources
  defined <- dsBaseClient:::isDefined(datasources, df)


  summary <- data.frame()
  join    <- list()

  for (p in 1:length(datasources)){

    colNames <- paste0(datasources[[p]]@name,".",(strsplit(as.character(substitute(dsfunction)), ".",fixed =TRUE))[[1]][2])


    y <- data.frame()

    for(i in dsBaseClient::ds.colnames(df,datasources[p])[[1]]) {

      variable <- paste0(df,"$",i)
      y[i,colNames] <- dsfunction(variable)[1]

<<<<<<< HEAD
=======
#' @param dsfunction The function you want to run (ds.class, ds.numNA, ds.length)
#' @param opal_connection An Opal connection
#' @param save if TRUE, the output is saved in the working directory as a csv file
#' @param df specifies the  df that was assigned in the login, default is "D"
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
>>>>>>> 3412a23193bd2b5634b0772760b0fc35f3716065
=======
>>>>>>> 1f4aa41ef13195a9b4e03ed03171a8f968684c4f
    }
    y$rn <- rownames(y)
    join[[p]] <- y

  }
<<<<<<< HEAD
<<<<<<< HEAD

  summary <- plyr::join_all(join, by = "rn", type = "full")
=======
  summary <- plyr::join_all(join, by = 'rn', type="full")
>>>>>>> 3412a23193bd2b5634b0772760b0fc35f3716065
=======

  summary <- plyr::join_all(join, by = "rn", type = "full")

>>>>>>> 1f4aa41ef13195a9b4e03ed03171a8f968684c4f
  rownames(summary) <- summary$rn
  summary$rn <- NULL


  if (save == TRUE){
    write.csv(summary, file = paste0(as.character(substitute(dsfunction)),"_overview.csv"), row.names = TRUE)
    print(paste0("The overview file ", paste0("'",as.character(substitute(dsfunction)),"_overview.csv'")," has been saved at ",getwd(), "."))
  }

  return(summary)
}
