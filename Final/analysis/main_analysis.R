library(dplyr)
library(stringr)


Data <- read.csv("compiled.csv")
## Pearson's median skewness (MS) = 3 * (Mean - Median) / Standard_Deviation
bootstrap_skewness <- function(dataset,bootstrap_times,log_or_not){
  MSs <- numeric(bootstrap_times)
  for(time in 1:bootstrap_times){
    idx <- sample(nrow(dataset),replace=TRUE)
    if(log_or_not==TRUE){
      smpl <- log(dataset$View_Count[idx])
    }
    else{
      smpl <- dataset$View_Count[idx]
    }
    MSs[time] <- 3* (mean(smpl)-median(smpl)) / sd(smpl)
  }
  return(MSs)
}
par(mfcol=c(2,1))

#symmetric is much better when View counts are log-transformed
# hist plot and set the title
hist(bootstrap_skewness(Data,1000,log_or_not = FALSE),main="Bootstrap Skewness",xlab="Pearson's Median Skewness")
hist(bootstrap_skewness(Data,1000,log_or_not = TRUE),main="Bootstrap Skewness (Log Transformation)",xlab="Pearson's Median Skewness")


