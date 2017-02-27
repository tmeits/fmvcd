#
require(zoo)
rm(list=ls())
setZeroNegativeNumber <- function(n){
  if(n < 0) return(0)
  else return(n)
}
filled_path <- "C:/Users/IVA/Dropbox/Apps/wmoID/24763"
filled_path <- "/home/larisa/Dropbox/Apps/wmoID/24763-2"
setwd(filled_path); getwd()

file_list <- list.files(pattern = "\\.cli$"); file_list
sink(paste(filled_path,"/log/","logofcode.txt", sep=""))
for (file_name in file_list){
  cat("**************  ", file_name, "  *****************\n")
  cli_dataset <-read.table(file_name, header=FALSE, sep="")
  names(cli_dataset) <- c("day", "month", "year", "prec", "temp");
  cli_dataset[cli_dataset$prec == -9999, 4] <- NA ; prec_vector <- cli_dataset[, 4];
  cli_dataset[cli_dataset$temp == -9999, 5] <- NA ; temp_vector <- cli_dataset[, 5]; 
  print(str(prec_vector)); print(str(temp_vector))
  print(summary(prec_vector)); print(summary(temp_vector))
  days <- c(1:dim(cli_dataset)[1])
  prec_zoo <- zoo(prec_vector, days)
  temp_zoo <- zoo(temp_vector, days)
  prec_filled <- sapply(as.integer(na.spline(prec_zoo)), setZeroNegativeNumber)
  temp_filled <- as.integer(na.spline(temp_zoo))
  print(str(prec_filled)); print(str(temp_filled))
  print(summary(prec_filled)); print(summary(temp_filled))
  new_cli <- data.frame(cli_dataset[,1], cli_dataset[,2], cli_dataset[,3], 
                        prec_filled, temp_filled)
  write.table(new_cli, file = paste(filled_path,"/filled/",file_name, sep=""),
              row.names=FALSE, col.names=FALSE, sep="\t")
  png(filename=paste(filled_path,"/png/perc-",file_name, ".png", sep=""))
  #par(mfrow=c(1,2))
  plot(prec_filled, main=file_name, xlab="Days", col="red", pch=19); points(days, prec_vector, col="blue", pch=19); dev.off()
  png(filename=paste(filled_path,"/png/temp-",file_name, ".png", sep=""))
  plot(temp_filled, main=file_name, xlab="Days", col="red", pch=19); points(days, temp_vector, col="blue", pch=19); dev.off()
}
sink()
# https://www.analyticsvidhya.com/blog/2016/12/practical-guide-to-implement-machine-learning-with-caret-package-in-r-with-practice-problem/
# https://gist.github.com/anonymous/6c738ab40823ce8466a942897850d262
