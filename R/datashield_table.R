
# This function takes a list of variable names (they have to be categorical) and an opal connection as an argument, 
# runs the datashield function ds.table() on all studies and outputs a table with the outcome

datashield_table <- function(opal_connection){
  if (!require(dplyr)) install.packages('dplyr')
  library(dplyr)
  source(paste0("/home/",username,"/DataSHIELD/Netzlaufwerke/T_DataSHIELD/Projekte/INTIMIC/Functions/datashield_descriptive.R"))
  classes <- datashield_descriptive(ds.class, opal_connection)
  classes <- classes %>% filter_all(any_vars(.=="factor"))
  variables <- rownames(classes)
  y = data.frame()
  for (i in variables){
    var <- paste0("D$",i)
    numNA <- ds.numNA(var)
    length <- ds.length(var)
    length <- length[length(length)]
    a = 0
    for (k in 1:length(numNA)){
      a=a+ numNA[[k]]
    }
    if (length == a) {
      next
    }
    b <- ds.table(var, datasources = opals, useNA="always")
    b[["output.list"]][["TABLE_rvar.by.study_row.props"]] <- NULL
    b <- as.data.frame(b[1])
    rownames(b)<- paste(i,rownames(b), sep="_")
    y <- dplyr::bind_rows(y,b)
  }
  return(y)
}
