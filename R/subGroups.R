
subGroups <- function(append.table = FALSE, data = NULL, grouping.factor = NULL, es = NULL, vi = NULL, study.id = NULL, label = NULL, modelweights = "CORR", ...){
  
  if(is.null(label))  {label = grouping.factor}
  
  if(isFALSE(append.table)) {newTable <- subTitle(label = label, ...)  }
  
  
  if(!isFALSE(append.table)) {newTable <- rbind(append.table, subTitle(label = label, ...))  }  
  
  subgroups <- unique(data[,grouping.factor])  
  
  for(i in subgroups){
    df <- data[data[,grouping.factor] == i,]
    
    
    ma <- robu(df[,es] ~ 1, studynum = df[,study.id], data = df, var.eff.size = vi, modelweights = modelweights)
    
    n <- length(unique(ma$study_orig_id))
    k <- length(ma$study_orig_id )
    g <- round(ma$reg_table$b.r,2)
    se <- round(ma$reg_table$SE,2)
    p <- round(ma$reg_table$prob,3)
    p <- ifelse(p == 0,"<.001",p)
    
    I <- paste(round(ma$mod_info$I.2[1],2),"%", sep="")
    
    
    newRow <- as.data.frame(cbind(i,n,k,g,se,I,p)) 
    if(exists("newTable")) {names(newRow) <- names(newTable)}
    
    
    
    if(!exists("results")) {results <-  newRow
    } else {results <- rbind(results,newRow)}
    
    
    if(i == tail(subgroups, n = 1)) {results <- rbind(newTable, results)}
    if(i == tail(subgroups, n = 1)) {
      results <- data.frame(lapply(results, as.character), stringsAsFactors=FALSE)
      return(results)}
  }
  
  results <- data.frame(lapply(results, as.character), stringsAsFactors=FALSE)
  return(results)
}