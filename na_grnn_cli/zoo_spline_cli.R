# 20.3.17 IVA
# http://stackoverflow.com/questions/18695335/replacing-all-nas-with-smoothing-spline
# http://stackoverflow.com/questions/18695335/replacing-all-nas-with-smoothing-spline
require(zoo)

setZeroNegativeNumber <- function(n){
  if(n < 0) return(0)
  else return(n)
}
setNameCli <- function(cli_dataset){
  names(cli_dataset) <- c("day", "month", "year", "prec", "temp");
  cli_dataset[cli_dataset$prec == -9999, 4] <- NA; 
  cli_dataset[cli_dataset$temp == -9999, 5] <- NA; 
  return(cli_dataset)
}
# Replacement numbers-9999 to missing value NA
replacementNumbersMinus9999.NA <- function(cli_dataset){
  for(i in 1:nrow(cli_dataset)){
    if(!is.na(cli_dataset[i,4])){
      if(cli_dataset[i,4]==-9999) cli_dataset[i,4]<-NA
    }
    if(!is.na(cli_dataset[i,5])) {
      if(cli_dataset[i,5]==-9999) cli_dataset[i,5]<-NA
    }
  }
  return(cli_dataset)
}
na.spline.cli <- function(cli_dataset){
  #cli_dataset<-setNameCli(cli_dataset)
  cli_dataset<-replacementNumbersMinus9999.NA(cli_dataset) 
  prec_vector <- cli_dataset[, 4]
  temp_vector <- cli_dataset[, 5]
  #days <- length(cli_dataset[,1])
  days <- c(1:dim(cli_dataset)[1])
  prec_zoo <- zoo(prec_vector, days)
  temp_zoo <- zoo(temp_vector, days)
  prec_filled <- sapply(as.integer(na.spline(prec_zoo)), setZeroNegativeNumber)
  temp_filled <- as.integer(na.spline(temp_zoo))
  new_cli <- data.frame(day=cli_dataset[,1], month=cli_dataset[,2], year=cli_dataset[,3], 
                        prec=prec_filled, temp=temp_filled)
  return(new_cli)                     
}
plotTempFilled <- function(naDF, filledDF, fileName){
  plot(filledDF[,5], main=fileName, ylab="Temp(c)*10", xlab="Days", col="red", pch=19); 
  points(naDF[,5], col="blue", pch=19)
}
plotPrecFilled <- function(naDF, filledDF, fileName){
  plot(filledDF[,4], main=fileName, ylab="Prec(mm)*10", xlab="Days", col="red", pch=19); 
  points(naDF[,4], col="blue", pch=19)
}

if(FALSE){
  filled_path <- "C:/Users/IVA/Dropbox/Apps/na_grnn"
  #filled_path <- "/home/larisa/Dropbox/Apps/na_grnn"
  setwd(filled_path); getwd(); file_name <- "1969.cli"
  cli_dataset <- read.table(file_name, header = FALSE, sep = "")
  summary(cli_dataset)
  cli_dataset<-replacementNumbersMinus9999.NA(cli_dataset)
  summary(cli_dataset)
  cli_dataset_filled <- na.spline.cli(cli_dataset)
  cli_dataset<-replacementNumbersMinus9999.NA(cli_dataset)
  summary(cli_dataset_filled)
  plotTempFilled(cli_dataset, cli_dataset_filled, file_name)
  plotPrecFilled(cli_dataset, cli_dataset_filled, file_name)
}


# https://www.analyticsvidhya.com/blog/2016/12/practical-guide-to-implement-machine-learning-with-caret-package-in-r-with-practice-problem/
# https://gist.github.com/anonymous/6c738ab40823ce8466a942897850d262
# install.packages('shiny', dependencies=TRUE, repos='http://cran.rstudio.com/')
# install.packages('imputeTS', dependencies=TRUE, repos='http://cran.rstudio.com/')