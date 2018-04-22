setwd("C:\\Neola\\ADS\\Project1\\ProjectFiles")
trainData <- read.csv("train.csv")
summary(trainData)

#finding the missing percentage
missSummary <- function(trainData){
  VarName <- names(trainData)
  Observations <- sapply(trainData,function(x) length(x))
  missCount <-sapply(trainData,function(x) sum(is.na(x)))
  missPercent <-sapply(trainData,
                       function(x) paste(100*sum(is.na(x))/length(x),"%",sep="")
  )
  out.DF <- cbind(VarName,
                  Observations,
                  missCount,
                  missPercent)
  row.names(out.DF) <- NULL
  out.DF  
}
missings <-missSummary(trainData)
missings


