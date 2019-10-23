#' subTitle
#'
#' This function allows you to add a single line to a sub-group analysis table  with a label for identifying.
#' Labels are added automatically by the subGroup function, but these may be of further use for deliniating sections of the table
#'
#' @param label The label that will be added to the table
#' @param headings Set to NULL by default. Can be used to specify custom headings. If unspecified then headings are determined by the type argument
#' @param type Either "cor" or "MD". Determines the headings used if not user specified.
#' @param append.table Determines whether to append to an exisitng table. By default it is set to false and will create a new table or overide the existing object. To append specify the existing table object
#' @return A table with a single row with the label or a label appended to existing table
#' @export

subTitle <- function(label, type = NULL, headings = NULL, append.table = FALSE){


  if(!isFALSE(append.table)) {
    newRow <- c(label, rep(NA, length(append.table)-1))
    newRow <- rbind(append.table, newRow)

    return(newRow)

  }

  if(is.null(headings) & is.null(type)){
    warning('Must either provide type or headings argument')
  }

  if(!is.null(headings)) {
    headings = headings
  } else if(type == "MD") {
    headings = c("label", "n","k","g","SE","i2","p")
  } else if (type == "cor") {
    headings = c("label", "n","k","rho","SE","i2","p")
  }



  newRow <- as.data.frame(cbind(label, NA, NA, NA, NA, NA, NA))
  names(newRow) <- headings
  return(newRow)

}


