#' Table function for categorical data
#'
#' @param opal_connection
#'
#' @return runs the datashield function ds.table() on all categorical variables of all studies and outputs a table with the outcome
#' @export
#' @import dplyr
#' @examples ds.table(opals)
datashield_table <- function(opal_connection){
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
