#' regTable
#'
#' This function allows you to perform meta-regressions on a list of variables and output the results in a table
#'
#'#' @param data The dataset
#' @param moderators The variables used for the meta-regressions. Each moderator will be used for its own meta-regresssion.
#' @param es The effect size variable
#' @param yi The variance in the effect size
#' @param study.id The study identifier
#' @param labels The labels that will be added to the top of each table. The variable name is used if unspecified
#' @param modelweights Either "CORR" or "HEIR" implmentation of Robust Variance Estimation. "CORR" is set as default
#' @param append.table Determines whether to append to an exisitng table. By default it is set to FALSE and will create a new table or overide the existing object. To append specify the existing table object
#' @return A table of a series of meta-regressions
#' @export



regTable <- function(append.table = FALSE, data = NULL, moderators = NULL, es = NULL, vi = NULL,
                     study.id = NULL, labels = NULL ,modelweights = "CORR", ...){

  z = 0
  for(i in moderators){
    z <- z + 1
    ma <- robu(data[,es] ~ 1 + data[,i], studynum = data[,study.id], data = data, var.eff.size = vi, modelweights = modelweights,...)

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
