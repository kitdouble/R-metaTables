#' regTitle
#'
#' This function allows you to add a single line to a meta-regression table with a label for identifying.
#' Labels are added automatically by the regTable function, but these may be of further use for deliniating sections of the table
#'
#' @param label The label that will be added to the table
#' @param headings Set to NULL by default. Can be used to specify custom headings. The default is headings are "Var","b","SE","ciL","ciU", "p"
#' @param append.table Determines whether to append to an exisitng table. By default it is seet to false and will create a new table or overide the existing object. To append specify the existing table object
#' @return A matrix of the infile
#' @export

regTitle <- function(label, headings = NULL, append.table = FALSE){


  if(!isFALSE(append.table)) {
    newRow <- c(label, rep(NA, length(append.table)-1))
    newRow <- rbind(append.table, newRow)

    return(newRow)

  }

  if(is.null(headings)){
    headings = c("Var","b","SE","ciL","ciU", "p")
  }

  if(!is.null(headings)) {
    headings = headings
  }

  newRow <- as.data.frame(matrix(ncol = length(headings), nrow = 1))
  newRow[1,1] <- label
  names(newRow) <- headings
  return(newRow)
}

