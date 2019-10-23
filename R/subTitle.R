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


