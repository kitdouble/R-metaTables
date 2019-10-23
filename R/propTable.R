#' propTable
#'
#' This function allows you to create a proportions table with frequencies and percentages broken down by variables
#'
#' @param data The dataset
#' @param variables The variables that you  would like a proportion table of
#' @param study.id The study identifier
#' @param labels The labels that can be used in place of the variable names
#' @return A table of proportions and frequencies
#' @export

propTable <- function(data = NULL, variables = NULL,  study.id = NULL, labels = NULL){

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
