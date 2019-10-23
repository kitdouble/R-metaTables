#################################################################################
# Meta Tables Version 0.1

# Author: Kit Double

# Date: 23/10/19

#################################################################################

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

########################################################################################################################

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




regTable <- function(append.table = FALSE, data = NULL, moderators = NULL, es = NULL, vi = NULL,
                     study.id = NULL, labels = NULL ,modelweights = "CORR", ...){
  if(length(moderators) != length(labels)) {warning('Length of labels does not match length of moderators')}
  z = 0
  for(i in moderators){
    z <- z + 1
    ma <- robu(data[,es] ~ 1 + data[,i], studynum = data[,study.id], data = data, var.eff.size = vi, modelweights = modelweights)
    
    # Create table
    ma <- ma$reg_table
    ma <- ma[,c("labels", "b.r", "SE", "CI.L", "CI.U", "prob")]
    ma$labels <- gsub("^.*\\.","", ma$labels )
    ma[1, "labels"] <-  "Intercept"
    ma$b.r <- round(ma$b.r,2)
    ma$SE <- round(ma$SE,2)
    ma$CI.L <- round(ma$CI.L,2)
    ma$CI.U <- round(ma$CI.U,2)
    ma$prob <- round(ma$prob,3)
    labeling <- ma[FALSE,]
    if(is.null(labels))  {labeler = i}
    else {labeler = labels[z]}
    labeling[1,1] <- labeler
    ma <- rbind(labeling, ma)
    
    
    
    if(!exists("results")) {results <-  ma
    } else {results <- rbind(results,ma)}
    
    if(i == tail(moderators, n = 1)) {
      if(!isFALSE(append.table)) {results <- rbind(append.table, results)}
      results <- data.frame(lapply(results, as.character), stringsAsFactors=FALSE)
      return(results)}
    
    
    
  }
  
  
  
}


################################################################################################################

propTable <- function(data = NULL, variables = NULL, k= T, study.id = NULL, labels = NULL){
  
  z = 0 
  
  for(i in variables){
    z <- z + 1
    Count <- table(data[,i])
    Proportion <- paste(round(prop.table(Count)*100,2), "%", sep="")
    p1 <- as.data.frame(cbind(Count, Proportion))
    p1$var <- rownames(p1)
    names(p1) <- c("K", "K Perc.", "Group")
    
    
    df <- unique(data[,c(i,study.id)])
    Count <- table(df[,i])
    Proportion <- paste(round(prop.table(Count)*100,2), "%", sep="")
    p2 <- cbind(Count, Proportion)
    p2 <- as.data.frame(cbind(Count, Proportion))
    p2$var <- rownames(p2)
    names(p2) <- c("N", "N Perc.", "Group")
    
    t1 <- merge(p2, p1, by = "Group", all=T)
    labeling <- t1[FALSE,]
    if(is.null(labels))  {labeler = i}
    else {labeler = labels[z]}
    labeling[1,1] <- labeler
    t1 <- rbind(labeling, t1)
    
    
    if(!exists("results")) {results <-  t1
    } else {results <- rbind(results,t1)}
    
    if(i == tail(variables, n = 1)) {
      return(results)
    }
    
    
  }
  
  
  
}






