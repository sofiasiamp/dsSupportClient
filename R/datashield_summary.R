<<<<<<< HEAD
<<<<<<< HEAD
=======
>>>>>>> 1f4aa41ef13195a9b4e03ed03171a8f968684c4f
#'
#' @title Executes ds.summary function for variables in a data.frame
#' @description The function executes the ds.summary function for all numeric or integer variables of a data.frame.
#' @details datashield_summary analyses the classes of variables of a server-side data.frame and executes the ds.summary
#' function for all variables which are of type 'numeric' or 'integer'. Additionally, it also provides the standard deviation
#' for those variables.
#' @param datasources A list of \code{\link{DSConnection-class}} objects obtained after login.
#' If the \code{datasources} argument is not specified the default set of connections will be
#' used: see \code{\link{datashield.connections_default}}.
#' @param df A data.frame on the server-side.
#' @param save if TRUE, the output is saved in the working directory as a csv file
<<<<<<< HEAD
=======

#' It runs the datashield function ds.summary()and calculates the standard deviation in all numeric and integer variables of each OpalConnection
#'
#' @param opal_connection An Opal connection
#' @param save_summary if TRUE, the output is saved in the working directory as a csv file
#' @param df specifies the  df that was assigned in the login, default is "D"
>>>>>>> 3412a23193bd2b5634b0772760b0fc35f3716065
=======
>>>>>>> 1f4aa41ef13195a9b4e03ed03171a8f968684c4f
#' @return a list with the summary of each variable
#' @author Sofia Siampani (Max-Delbrueck-Center), Florian Schwarz (German Institute of Human Nutrition)
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
#' datashield_summary(df = "D", save = TRUE)
#'
#' # Clear the Datashield R sessions and logout
#' datashield.logout(connections)
#'
#' }
#' @export
#'

<<<<<<< HEAD
<<<<<<< HEAD

=======
>>>>>>> 1f4aa41ef13195a9b4e03ed03171a8f968684c4f
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

      variable <- paste0(df,"$",i)
      a <- dsBaseClient::ds.class(variable)

      if ((a == "numeric" || a == "integer") && (dsBaseClient::ds.numNA(variable) < dsBaseClient::ds.length(variable)[[1]])){
        a <- unlist(dsBaseClient::ds.summary(variable))
        y[i, c(names(a),"Standard deviation")] <- c(a, sqrt(dsBaseClient::ds.var(variable)[["Variance.by.Study"]][1]))
<<<<<<< HEAD
=======
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
>>>>>>> 3412a23193bd2b5634b0772760b0fc35f3716065
=======
>>>>>>> 1f4aa41ef13195a9b4e03ed03171a8f968684c4f
      }
    }

    summary[[as.name(paste0(datasources[[p]]@name))]] <- y
    if (save == TRUE){
      write.csv(as.matrix(y), paste0(datasources[[p]]@name,"_summary.csv"), row.names = TRUE)
      print(paste0("The summary file ", paste0("'",datasources[[p]]@name,"_summary.csv'")," has been saved at ",getwd(), "."))
    }
  }

  return(summary)
}
