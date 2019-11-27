#' subGroups
#'
#' This function allows you to perform sub-groups on all levels of a variable and output the results in a table
#'
#'#' @param data The dataset
#' @param grouping.factor The variables used for the sub-groups. All levels of each grouping factor will be run as a sub-group
#' @param es The effect size variable
#' @param yi The variance in the effect size
#' @param study.id The study identifier
#' @param headings Set to NULL by default. Can be used to specify custom headings. If unspecified then headings are determined by the type argument
#' @param type Either "cor" or "MD". Determines the headings used if not user specified.
#' @param label The labels that will be added to the top of each table section. The grouping.factor variable name is used if unspecified
#' @param modelweights Either "CORR" or "HEIR" implmentation of Robust Variance Estimation. "CORR" is set as default
#' @param append.table Determines whether to append to an exisitng table. By default it is set to FALSE and will create a new table or overide the existing object. To append specify the existing table object
#' @return A table of a series of sub-groups analyses
#' @export
#'
#'
subGroups <- function(append.table = FALSE, data = NULL, grouping.factor = NULL, es = NULL, vi = NULL, study.id = NULL, label = NULL, modelweights = "CORR", type = NULL, headings = NULL, ...){

  if(is.null(label))  {label = grouping.factor}

  if(isFALSE(append.table)) {newTable <- subTitle(headings = headings, type = type, label = label, ...)  }


  if(!isFALSE(append.table)) {newTable <- rbind(append.table, subTitle(headings = headings, type = type, label = label, ...))  }

  subgroups <- unique(data[,grouping.factor])
  subgroups <- subgroups[!is.na(subgroups)]

  for(i in subgroups){
    df <- data[data[,grouping.factor] == i,]


    ma <- robu(df[,es] ~ 1, studynum = df[,study.id], data = df, var.eff.size = vi, modelweights = modelweights)

    n <- length(unique(ma$study_orig_id))
    k <- length(ma$study_orig_id )
    g <- round(ma$reg_table$b.r,2)
    ciL <- round(ma$reg_table$CI.L,2)
    ciU <- round(ma$reg_table$CI.U,2)

    se <- round(ma$reg_table$SE,2)
    p <- round(ma$reg_table$prob,3)
    p <- ifelse(p == 0,"<.001",p)

    I <- paste(round(ma$mod_info$I.2[1],2),"%", sep="")


    newRow <- as.data.frame(cbind(i,n,k,g,se,ciL,ciU,I,p))
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
